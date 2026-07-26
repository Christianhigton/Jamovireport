multiple_ttest_results <- function(p_values = c(.01, .04), invalid_between = FALSE) {
    base <- edu_t_test(ToothGrowth, "len", "supp")
    make_result <- function(p, statistic = 2) {
        result <- base
        result$statistics$p <- p
        result$statistics$statistic <- statistic
        result
    }
    valid <- lapply(p_values, make_result)
    if (!invalid_between)
        return(valid)
    invalid <- make_result(.03, statistic = NA_real_)
    list(valid[[1]], invalid, valid[[2]])
}

multiple_comparison_source_root <- function() {
    dir <- getwd()
    while (!file.exists(file.path(dir, "DESCRIPTION"))) {
        parent <- dirname(dir)
        if (identical(parent, dir))
            skip("Source package root is not available in this installed-package check context.")
        dir <- parent
    }
    if (!dir.exists(file.path(dir, "jamovi")))
        skip("Source jamovi metadata is not available in this installed-package check context.")
    dir
}

test_that("Holm and Bonferroni adjustments match stats::p.adjust", {
    results <- multiple_ttest_results(c(.004, .03, .20))
    holm <- .jr_ttest_adjustment_info(results, "holm")
    bonferroni <- .jr_ttest_adjustment_info(results, "bonferroni")

    expect_equal(unname(holm$adjusted), stats::p.adjust(c(.004, .03, .20), "holm"))
    expect_equal(
        unname(bonferroni$adjusted),
        stats::p.adjust(c(.004, .03, .20), "bonferroni")
    )
    expect_equal(holm$heading, "Holm-adjusted p")
    expect_equal(bonferroni$heading, "Bonferroni-adjusted p")
    expect_true(holm$active)
    expect_true(bonferroni$active)
})

test_that("adjustments map by stable test id and exclude invalid statistics", {
    results <- multiple_ttest_results(c(.01, .04), invalid_between = TRUE)
    rows <- .jr_addon_apa_rows(results, "holm")
    info <- attr(rows, "adjustment")
    expected <- stats::p.adjust(c(.01, .04), "holm")

    expect_equal(info$n_valid, 2L)
    expect_equal(rows$test_id, c(
        "result-1-statistic-1", "result-2-statistic-1", "result-3-statistic-1"
    ))
    expect_equal(rows$p_adjusted[c(1, 3)], expected)
    expect_true(is.na(rows$p_adjusted[2]))
    expect_equal(rows$adjustment_result[2], "Test could not be calculated")
    expect_match(info$note, "2 valid t-tests", fixed = TRUE)
})

test_that("failed requested tests are excluded and retained as labelled rows", {
    valid <- multiple_ttest_results(.01)[[1]]
    failed <- .jr_tag_ttest_attempt(
        try(stop("insufficient observations"), silent = TRUE),
        "outcome_b Student t-test"
    )
    results <- list(valid, failed)
    rows <- .jr_addon_apa_rows(results, "holm")
    info <- attr(rows, "adjustment")
    guidance <- .jr_addon_interpretation_html(results, adjustment = "holm")

    expect_equal(info$n_valid, 1L)
    expect_false(info$active)
    expect_equal(nrow(rows), 2L)
    expect_equal(rows$test_id[2], "result-2-statistic-1")
    expect_equal(rows$test[2], "outcome_b Student t-test")
    expect_equal(rows$adjustment_result[2], "Test could not be calculated")
    expect_true(is.na(rows$p_adjusted[2]))
    expect_match(info$note, "Only one valid t-test was available", fixed = TRUE)
    expect_match(guidance, "Only one valid t-test was available", fixed = TRUE)
})

test_that("decision labels use adjusted rather than raw p-values", {
    results <- multiple_ttest_results(c(.03, .04))
    holm <- .jr_ttest_adjustment_info(results, "holm", alpha = .05)

    expect_equal(unname(holm$adjusted), c(.06, .06))
    expect_equal(
        unname(holm$decision),
        rep("Significant before adjustment only", 2)
    )

    mixed <- .jr_ttest_adjustment_info(multiple_ttest_results(c(.001, .04)), "holm")
    expect_equal(mixed$decision[["result-1-statistic-1"]], "Significant after adjustment")
    expect_equal(mixed$decision[["result-2-statistic-1"]], "Significant after adjustment")
})

test_that("column state, notes, and guidance follow the selected method", {
    results <- multiple_ttest_results(c(.01, .20))
    holm <- .jr_ttest_adjustment_info(results, "holm")
    bonferroni <- .jr_ttest_adjustment_info(results, "bonferroni")
    none <- .jr_ttest_adjustment_info(results, "none")

    expect_true(holm$active)
    expect_true(bonferroni$active)
    expect_false(none$active)
    expect_match(holm$note, "Holm procedure", fixed = TRUE)
    expect_match(bonferroni$note, "Bonferroni procedure", fixed = TRUE)
    expect_match(none$note, "P-values are unadjusted", fixed = TRUE)

    holm_guidance <- .jr_addon_interpretation_html(results, adjustment = "holm")
    bonferroni_guidance <- .jr_addon_interpretation_html(
        results, adjustment = "bonferroni"
    )
    none_guidance <- .jr_addon_interpretation_html(results, adjustment = "none")
    expect_match(holm_guidance, "meaningful family of related comparisons", fixed = TRUE)
    expect_match(holm_guidance, "Holm procedure", fixed = TRUE)
    expect_match(bonferroni_guidance, "Bonferroni procedure", fixed = TRUE)
    expect_match(none_guidance, "Multiple unadjusted t-tests", fixed = TRUE)
})

test_that("adjusted reporting labels raw p-values and correction outcomes", {
    results <- multiple_ttest_results(c(.03, .04))
    holm_html <- .jr_addon_report_html(results, adjustment = "holm")
    bonferroni_html <- .jr_addon_report_html(results, adjustment = "bonferroni")
    holm_guidance <- .jr_addon_interpretation_html(results, adjustment = "holm")
    bonferroni_guidance <- .jr_addon_interpretation_html(
        results, adjustment = "bonferroni"
    )

    expect_match(holm_guidance, "Holm procedure", fixed = TRUE)
    expect_match(holm_html, "unadjusted p", fixed = TRUE)
    expect_match(holm_html, "did not remain statistically significant", fixed = TRUE)
    expect_match(holm_html, "Holm-adjusted p", fixed = TRUE)
    expect_match(bonferroni_guidance, "Bonferroni procedure", fixed = TRUE)
    expect_match(bonferroni_html, "Bonferroni-adjusted p", fixed = TRUE)
})

test_that("multiple-comparison references render only when requested", {
    holm <- .jr_methods_references_html(keys = c("Holm1979", "Vickerstaff2019"))
    bonferroni <- .jr_methods_references_html(keys = "Vickerstaff2019")

    expect_match(holm, "simple sequentially rejective", ignore.case = TRUE)
    expect_match(holm, "Vickerstaff", fixed = TRUE)
    expect_false(grepl("Holm", bonferroni, fixed = TRUE))
    expect_match(bonferroni, "multiple comparisons", ignore.case = TRUE)
})

test_that("a single valid t-test retains existing output", {
    result <- multiple_ttest_results(.02)[[1]]
    holm_rows <- .jr_addon_apa_rows(list(result), "holm")
    holm_html <- .jr_addon_report_html(list(result), adjustment = "holm")
    none_html <- .jr_addon_report_html(list(result), adjustment = "none")

    expect_false(attr(holm_rows, "adjustment")$active)
    expect_identical(holm_html, none_html)
    expect_false(grepl("adjusted p", holm_html, fixed = TRUE))
    expect_false(grepl("multiple-comparison", holm_html, fixed = TRUE))
})

test_that("existing add-ons gain a backward-compatible Holm default", {
    independent <- jrReportTTestISOptions$new()
    paired <- jrReportTTestPSOptions$new()

    expect_equal(independent$pAdjustment, "holm")
    expect_equal(paired$pAdjustment, "holm")
    root <- multiple_comparison_source_root()
    definition <- paste(readLines(
        file.path(root, "jamovi", "jrReportTTestIS.a.yaml"),
        warn = FALSE
    ), collapse = "\n")
    expect_match(definition, "Holm (recommended)", fixed = TRUE)
    expect_match(definition, "Bonferroni", fixed = TRUE)
    expect_match(definition, "None", fixed = TRUE)
})

test_that("no new t-test analysis or menu entry was created", {
    root <- multiple_comparison_source_root()
    analyses <- list.files(file.path(root, "jamovi"), pattern = "\\.a\\.yaml$")
    ttests <- analyses[grepl("TTest", analyses, fixed = TRUE)]

    expect_setequal(
        ttests,
        c(
            "eduTTest.a.yaml", "eduTTestIndependent.a.yaml", "eduTTestPaired.a.yaml",
            "jrReportTTestIS.a.yaml", "jrReportTTestPS.a.yaml"
        )
    )
})
