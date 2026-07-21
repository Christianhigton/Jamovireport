correlation_test_data <- function() {
    set.seed(1042)
    n <- 80L
    x <- seq_len(n) + stats::rnorm(n, sd = 8)
    data.frame(
        anxiety = x,
        depression = 0.35 * x + stats::rnorm(n, sd = 22),
        stress = -0.2 * x + stats::rnorm(n, sd = 25),
        wellbeing = stats::rnorm(n)
    )
}

correlation_source_root <- function() {
    root <- getwd()
    while (!file.exists(file.path(root, "DESCRIPTION"))) {
        parent <- dirname(root)
        if (identical(parent, root))
            skip("Source package root is not available in this test context.")
        root <- parent
    }
    root
}

correlation_family <- function(method = "pearson") {
    d <- correlation_test_data()
    list(
        edu_correlation(d, "anxiety", "depression", method = method),
        edu_correlation(d, "anxiety", "stress", method = method),
        edu_correlation(d, "anxiety", "wellbeing", method = method)
    )
}

set_correlation_p <- function(results, p) {
    Map(function(result, value) {
        result$statistics$p[1] <- value
        result$main$p[1] <- value
        result$effects$p[1] <- value
        result
    }, results, p)
}

test_that("correlation adjustments match base R for all supported methods", {
    for (correlation_method in c("pearson", "spearman", "kendall")) {
        results <- correlation_family(correlation_method)
        raw <- vapply(results, function(result) result$statistics$p[1], numeric(1))

        holm <- .jr_correlation_adjustment_info(results, "holm")
        bonferroni <- .jr_correlation_adjustment_info(results, "bonferroni")
        bh <- .jr_correlation_adjustment_info(results, "bh")

        expect_equal(unname(holm$adjusted), stats::p.adjust(raw, method = "holm"))
        expect_equal(unname(bonferroni$adjusted), stats::p.adjust(raw, method = "bonferroni"))
        expect_equal(unname(bh$adjusted), stats::p.adjust(raw, method = "BH"))
        expect_equal(holm$heading, "Holm-adjusted p")
        expect_equal(bonferroni$heading, "Bonferroni-adjusted p")
        expect_equal(bh$heading, "BH-adjusted p")
    }
})

test_that("adjusted p-values map to stable correlation-pair identifiers", {
    results <- correlation_family()
    original <- .jr_correlation_adjustment_info(results, "holm")
    reordered <- .jr_correlation_adjustment_info(results[c(3, 1, 2)], "holm")

    expect_equal(
        original$adjusted[sort(names(original$adjusted))],
        reordered$adjusted[sort(names(reordered$adjusted))]
    )
    expect_true(all(grepl("^correlation::pearson::", names(original$adjusted))))
})

test_that("mirrored, diagonal, and duplicated correlations are not counted twice", {
    d <- correlation_test_data()
    forward <- edu_correlation(d, "anxiety", "depression")
    reverse <- edu_correlation(d, "depression", "anxiety")
    diagonal <- edu_correlation(d, "anxiety", "anxiety")
    second <- edu_correlation(d, "anxiety", "stress")

    info <- .jr_correlation_adjustment_info(
        list(forward, reverse, diagonal, forward, second), "holm"
    )

    expect_equal(nrow(info$records), 2L)
    expect_equal(info$n_valid, 2L)
    expect_equal(length(unique(info$records$test_id)), 2L)
    expect_false(any(info$records$variable1 == info$records$variable2))
})

test_that("failed and invalid correlations are retained but excluded from correction", {
    d <- correlation_test_data()
    valid <- edu_correlation(d, "anxiety", "depression")
    failed <- .jr_tag_correlation_attempt(
        try(edu_correlation(data.frame(x = 1:8, constant = 1), "x", "constant"), silent = TRUE),
        "pearson", c("x", "constant")
    )
    invalid <- edu_correlation(d, "anxiety", "stress")
    invalid$statistics$p[1] <- NA_real_

    info <- .jr_correlation_adjustment_info(list(valid, failed, invalid), "holm")
    rows <- .jr_correlation_apa_rows(list(valid, failed, invalid), "holm")

    expect_equal(info$n_valid, 1L)
    expect_false(info$active)
    expect_equal(nrow(info$records), 3L)
    expect_equal(sum(rows$adjustment_result == "Test could not be calculated"), 2L)
    expect_match(info$note, "Only one valid correlation test was available", fixed = TRUE)
})

test_that("corrected significance decisions use unrounded adjusted p-values", {
    results <- set_correlation_p(correlation_family(), c(.01, .04, .30))
    info <- .jr_correlation_adjustment_info(results, "holm", alpha = .05)
    rows <- .jr_correlation_apa_rows(results, "holm", alpha = .05)

    expect_equal(unname(info$adjusted), c(.03, .08, .30))
    expect_equal(
        unname(info$decision),
        c(
            "Significant after adjustment",
            "Significant before adjustment only",
            "Not significant after adjustment"
        )
    )
    expect_equal(rows$adjustment_result, unname(info$decision))

    report <- .jr_addon_report_html(
        results, options = .jr_addon_reporting_options(), adjustment = "holm"
    )
    expect_match(report, "did not remain statistically significant following Holm adjustment", fixed = TRUE)
    expect_match(report, "unadjusted p", fixed = TRUE)
    expect_match(report, "Holm-adjusted p", fixed = TRUE)
})

test_that("table metadata identifies unique tests and selected correction", {
    results <- correlation_family()
    holm <- .jr_correlation_apa_rows(results, "holm")
    bonferroni <- .jr_correlation_apa_rows(results, "bonferroni")
    bh <- .jr_correlation_apa_rows(results, "bh")
    none <- .jr_correlation_apa_rows(results, "none")

    expect_true(all(c(
        "variable1", "variable2", "method", "statistic", "p", "p_adjusted",
        "adjustment_result", "ci", "n"
    ) %in% names(holm)))
    expect_match(attr(holm, "adjustment")$note, "3 unique, valid correlation tests", fixed = TRUE)
    expect_match(attr(bonferroni, "adjustment")$note, "Bonferroni procedure", fixed = TRUE)
    expect_match(attr(bh, "adjustment")$note, "false discovery rate", fixed = TRUE)
    expect_match(attr(none, "adjustment")$note, "P-values are unadjusted", fixed = TRUE)
    expect_false(attr(none, "adjustment")$active)
})

test_that("correlation family guidance distinguishes FWER, FDR, and no adjustment", {
    results <- correlation_family()
    holm <- .jr_addon_interpretation_html(results, adjustment = "holm")
    bonferroni <- .jr_addon_interpretation_html(results, adjustment = "bonferroni")
    bh <- .jr_addon_interpretation_html(results, adjustment = "bh")
    none <- .jr_addon_interpretation_html(results, adjustment = "none")
    none_report <- .jr_addon_report_html(
        results, options = .jr_addon_reporting_options(), adjustment = "none"
    )

    expect_match(holm, "meaningful family of related tests", fixed = TRUE)
    expect_match(holm, "Holm procedure controls the familywise", fixed = TRUE)
    expect_match(bonferroni, "Bonferroni procedure controls the familywise", fixed = TRUE)
    expect_match(bh, "expected proportion of false discoveries", fixed = TRUE)
    expect_match(bh, "exploratory correlation set", fixed = TRUE)
    expect_false(grepl("Benjamini-Hochberg procedure controls the familywise", bh, fixed = TRUE))
    expect_match(none, "have not been adjusted for multiple testing", fixed = TRUE)
    expect_match(none, "study design, preregistration", fixed = TRUE)
    expect_match(none_report, "risk of false-positive findings", fixed = TRUE)
})

test_that("correction references are method-specific", {
    holm <- .jr_methods_references_html(keys = c("Holm1979", "Vickerstaff2019"))
    bonferroni <- .jr_methods_references_html(keys = "Vickerstaff2019")
    bh <- .jr_methods_references_html(keys = "BenjaminiHochberg1995")

    expect_match(holm, "Holm (1979)", fixed = TRUE)
    expect_match(holm, "Vickerstaff")
    expect_false(grepl("Benjamini", holm, fixed = TRUE))
    expect_false(grepl("Holm (1979)", bonferroni, fixed = TRUE))
    expect_match(bh, "Benjamini &amp; Hochberg (1995)", fixed = TRUE)
    expect_match(bh, "false discovery rate", ignore.case = TRUE)

    results <- correlation_family()
    expect_equal(
        .jr_adjustment_reference_keys(.jr_correlation_adjustment_info(results, "holm")),
        c("Holm1979", "Vickerstaff2019")
    )
    expect_equal(
        .jr_adjustment_reference_keys(.jr_correlation_adjustment_info(results, "bonferroni")),
        "Vickerstaff2019"
    )
    expect_equal(
        .jr_adjustment_reference_keys(.jr_correlation_adjustment_info(results, "bh")),
        "BenjaminiHochberg1995"
    )
    expect_length(
        .jr_adjustment_reference_keys(.jr_correlation_adjustment_info(results, "none")),
        0L
    )
})

test_that("correlation methods retain their coefficients and method-specific assumptions", {
    d <- correlation_test_data()
    pearson <- edu_correlation(d, "anxiety", "depression", method = "pearson")
    spearman <- edu_correlation(d, "anxiety", "depression", method = "spearman")
    kendall <- edu_correlation(d, "anxiety", "depression", method = "kendall")

    expect_equal(
        pearson$statistics$statistic,
        unname(stats::cor.test(d$anxiety, d$depression, method = "pearson")$estimate)
    )
    expect_true(is.finite(pearson$statistics$ci_low))
    expect_match(paste(pearson$diagnostics$check, collapse = " "), "Approximate bivariate normality", fixed = TRUE)
    expect_false(any(grepl("bivariate normality", spearman$diagnostics$check, ignore.case = TRUE)))
    expect_false(any(grepl("bivariate normality", kendall$diagnostics$check, ignore.case = TRUE)))
    expect_match(paste(spearman$diagnostics$check, collapse = " "), "tied ranks", ignore.case = TRUE)
    expect_match(paste(kendall$diagnostics$check, collapse = " "), "Tied observations", fixed = TRUE)
})

test_that("one correlation remains unchanged and an empty valid vector is safe", {
    result <- correlation_family()[[1]]
    holm_report <- .jr_addon_report_html(
        list(result), options = .jr_addon_reporting_options(), adjustment = "holm"
    )
    none_report <- .jr_addon_report_html(
        list(result), options = .jr_addon_reporting_options(), adjustment = "none"
    )
    empty <- .jr_correlation_adjustment_info(list(), "holm")

    expect_identical(holm_report, none_report)
    expect_false(grepl("adjusted p", holm_report, fixed = TRUE))
    expect_equal(empty$n_valid, 0L)
    expect_length(empty$adjusted, 0L)
    expect_false(empty$active)
})

test_that("correlation correction extends the existing saved-analysis schema", {
    root <- correlation_source_root()
    analysis <- yaml::read_yaml(file.path(root, "jamovi", "jrReportCorrMatrix.a.yaml"))
    results <- yaml::read_yaml(file.path(root, "jamovi", "jrReportCorrMatrix.r.yaml"))
    ui_text <- paste(
        readLines(file.path(root, "jamovi", "jrReportCorrMatrix.u.yaml"), warn = FALSE),
        collapse = "\n"
    )
    correlation_analyses <- list.files(
        file.path(root, "jamovi"), pattern = "(Correlation|CorrMatrix)\\.a\\.yaml$"
    )

    expect_equal(analysis$name, "jrReportCorrMatrix")
    expect_equal(analysis$options[[2]]$name, "pAdjustment")
    expect_equal(analysis$options[[2]]$default, "holm")
    expect_equal(jrReportCorrMatrixOptions$new()$pAdjustment, "holm")
    expect_equal(
        vapply(analysis$options[[2]]$options, `[[`, character(1), "name"),
        c("holm", "bonferroni", "bh", "none")
    )
    result_names <- vapply(results$items, function(item) item$name, character(1))
    expect_equal(sum(result_names == "jReportApaTable"), 1L)
    table_columns <- results$items[[which(result_names == "jReportApaTable")]]$columns
    table_names <- vapply(table_columns, `[[`, character(1), "name")
    expect_true(all(c("p", "p_adjusted", "adjustment_result") %in% table_names))
    expect_false(table_columns[[which(table_names == "p_adjusted")]]$visible)
    expect_match(ui_text, "meaningful family of related hypotheses", fixed = TRUE)
    expect_match(ui_text, "research aims and hypothesis structure", fixed = TRUE)
    expect_setequal(correlation_analyses, c("eduCorrelation.a.yaml", "jrReportCorrMatrix.a.yaml"))
})
