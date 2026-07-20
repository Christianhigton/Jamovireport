reporting_matrix_results <- local({
    cache <- NULL
    function() {
        if (!is.null(cache))
            return(cache)
        d <- ToothGrowth
        d$dose <- factor(d$dose)
        d$dose_num <- as.numeric(as.character(d$dose))
        paired <- data.frame(
            pre = c(10, 9, 8, 12, 7, 11, 8, 13, 9, 10),
            post = c(8, 8, 7, 9, 8, 9, 7, 10, 8, 8)
        )
        set.seed(20260720)
        repeated <- data.frame(
            group = factor(rep(c("control", "treatment"), each = 18)),
            pre = rnorm(36, 10, 2),
            mid = rnorm(36, 11, 2),
            post = rnorm(36, 12, 2)
        )
        binary <- transform(mtcars, am = factor(am, labels = c("automatic", "manual")))
        gof <- data.frame(choice = factor(rep(c("A", "B", "C"), c(20, 12, 8))))
        data(bfi, package = "psych")
        scale <- psych::bfi[1:100, c("A1", "A2", "A3", "A4", "A5")]
        reliability <- edu_reliability_omega(
            scale, names(scale), reverse_items = "A1", bootstrap = FALSE
        )
        cache <<- list(
            independent_t = edu_t_test(d, "len", "supp", var_equal = FALSE),
            paired_t = edu_t_test(paired, "pre", paired_outcome = "post", type = "paired"),
            one_way_anova = edu_anova_oneway(d, "len", "dose", posthoc = FALSE),
            factorial_anova = edu_anova_between(d, "len", c("dose", "supp")),
            repeated_anova = edu_anova_rm(repeated, c("pre", "mid", "post")),
            mixed_anova = edu_anova_mixed(repeated, c("pre", "mid", "post"), "group"),
            ancova = edu_ancova(d, "len", "supp", "dose_num"),
            manova = edu_manova(iris, c("Sepal.Length", "Sepal.Width"), "Species"),
            mancova = edu_manova(
                iris, c("Sepal.Length", "Sepal.Width"), "Species", "Petal.Length"
            ),
            correlation = edu_correlation(mtcars, "mpg", "wt"),
            chisq_independence = edu_chisq_independence(d, "supp", "dose"),
            chisq_gof = edu_chisq_gof(gof, "choice"),
            linear_regression = edu_lm(mtcars, mpg ~ wt + hp),
            binomial_logistic = edu_logistic_regression(binary, am ~ wt + hp),
            multinomial_logistic = suppressWarnings(
                edu_multinomial_logistic(iris, Species ~ Sepal.Length + Sepal.Width)
            ),
            reliability = reliability,
            cronbach_alpha = reliability,
            mcdonald_omega = reliability
        )
        cache
    }
})

test_that("all 18 reporting variants expose one validated reporting model", {
    required <- c(
        "analysisType", "analysisVariant", "outcome", "predictors", "factors",
        "groups", "conditions", "n", "groupNs", "descriptives", "testStatistic",
        "df", "p", "effectSizes", "confidenceIntervals", "corrections",
        "postHocTests", "simpleEffects", "modelFit", "coefficients", "assumptions",
        "warnings", "tableData", "referenceCategory", "narrativeUnits",
        "validationIssues"
    )
    results <- reporting_matrix_results()
    expect_length(results, 18L)
    for (name in names(results)) {
        model <- results[[name]]$report_model
        expect_s3_class(model, "jr_report_model")
        expect_true(all(required %in% names(model)), info = name)
        expect_false(any(grepl("contains an infinite", model$validationIssues)), info = name)
    }
    expect_equal(results$manova$report_model$analysisVariant, "manova")
    expect_equal(results$mancova$report_model$analysisVariant, "mancova")
    expect_equal(results$reliability$report_model$analysisVariant, "alpha_and_omega")
})

test_that("every reporting variant renders compact and detailed APA tables", {
    for (name in names(reporting_matrix_results())) {
        result <- reporting_matrix_results()[[name]]
        compact <- .jr_apa_table_html(result, "compact")
        detailed <- .jr_apa_table_html(result, "detailed")
        expect_match(compact, "APA-style results table", fixed = TRUE, info = name)
        expect_match(compact, "<table", fixed = TRUE, info = name)
        expect_match(detailed, "Detailed reporting view", fixed = TRUE, info = name)
        expect_false(grepl("NaN|Inf|null|undefined", compact), info = name)
        expect_false(grepl("NaN|Inf|null|undefined", detailed), info = name)
    }
})

test_that("style, format, and detail rendering never mutate numerical claims", {
    result <- reporting_matrix_results()$linear_regression
    before <- result$report_model
    outputs <- character()
    for (style in c("apa7", "plain", "journal", "dissertation")) {
        for (format in c("short", "paragraph", "bullets", "table_paragraph", "copy_ready")) {
            for (tone in c("concise", "student_friendly", "detailed", "critical"))
                outputs <- c(outputs, edu_report(result, style = style, format = format, tone = tone))
        }
    }
    expect_identical(result$report_model, before)
    expect_gt(length(unique(outputs)), 8L)
    expect_false(any(grepl("NaN|Inf|null|undefined|p = \\.000", outputs)))
})

test_that("table plus paragraph is concise and does not repeat the table statistic", {
    result <- reporting_matrix_results()$independent_t
    text <- edu_report(result, style = "journal", format = "table_paragraph", tone = "concise")
    statistic <- .jr_num(result$statistics$statistic[1])
    expect_match(text, "accompanying table|APA table", ignore.case = TRUE)
    expect_false(grepl(statistic, text, fixed = TRUE))
    expect_false(grepl("Use the accompanying results table", text, fixed = TRUE))
})

test_that("APA tables use analysis-specific schemas", {
    results <- reporting_matrix_results()
    expect_match(.jr_apa_table_html(results$linear_regression, "detailed"), "Regression coefficients", fixed = TRUE)
    expect_match(.jr_apa_table_html(results$binomial_logistic, "detailed"), "OR", fixed = TRUE)
    expect_match(.jr_apa_table_html(results$multinomial_logistic, "detailed"), "Outcome-category comparisons", fixed = TRUE)
    expect_match(.jr_apa_table_html(results$chisq_independence, "detailed"), "Observed and expected frequencies", fixed = TRUE)
    expect_match(.jr_apa_table_html(results$manova, "detailed"), "Pillai", ignore.case = TRUE)
    reliability <- .jr_apa_table_html(results$reliability, "compact")
    expect_match(reliability, "McDonald", fixed = TRUE)
    expect_match(reliability, "Cronbach", fixed = TRUE)
})

test_that("logistic report models identify event and reference categories", {
    results <- reporting_matrix_results()
    expect_length(results$binomial_logistic$report_model$eventCategory, 1L)
    expect_length(results$binomial_logistic$report_model$referenceCategory, 1L)
    expect_length(results$multinomial_logistic$report_model$referenceCategory, 1L)
    expect_true("category" %in% names(results$multinomial_logistic$report_model$coefficients))
})

test_that("formatters suppress invalid and non-finite values", {
    expect_equal(.jr_num(Inf), "NA")
    expect_equal(.jr_num(-Inf), "NA")
    expect_equal(.jr_p(Inf), "not available")
    expect_equal(.jr_p(-.1), "not available")
    expect_equal(.jr_p(1.1), "not available")
    expect_equal(.jr_table_p(0), "&lt; .001")
    expect_false(grepl(".000", .jr_table_p(0), fixed = TRUE))
})

test_that("user-provided labels are escaped in APA table HTML", {
    d <- data.frame(check.names = FALSE, "score<script>" = 1:12, "other&value" = 12:1)
    result <- edu_correlation(d, "score<script>", "other&value")
    html <- .jr_apa_table_html(result, "detailed")
    expect_false(grepl("<script>", html, fixed = TRUE))
    expect_match(html, "score&lt;script&gt;", fixed = TRUE)
    expect_match(html, "other&amp;value", fixed = TRUE)
})

test_that("severe warnings survive concise rendering", {
    result <- reporting_matrix_results()$independent_t
    result$diagnostics <- rbind(result$diagnostics, data.frame(
        check = "Estimability", tested = "Yes", statistic = NA_real_, p = NA_real_,
        status = "Serious", interpretation = "The requested effect is not estimable.",
        action = "Do not interpret this effect.", stringsAsFactors = FALSE
    ))
    result <- .jr_finalize_edu_analysis(result)
    expect_match(edu_report(result, tone = "concise"), "not estimable", fixed = TRUE)
    expect_match(.jr_apa_table_html(result, "compact"), "not estimable", fixed = TRUE)
})

test_that("guided manifests expose ordered table controls and table results", {
    root <- if (file.exists("DESCRIPTION")) "." else file.path("..", "..")
    jamovi_dir <- file.path(root, "jamovi")
    if (!file.exists(file.path(jamovi_dir, "eduTTest.a.yaml")))
        skip("Source jamovi metadata is not available in this installed-package check context.")
    analyses <- c(
        "eduTTest", "eduTTestIndependent", "eduTTestPaired", "eduAnova",
        "eduBetweenAnova", "eduRMAnova", "eduMixedAnova", "eduAncova",
        "eduMancova", "eduCorrelation", "eduChiSquareIndependence",
        "eduChiSquareGoodness", "eduRegression", "eduLogistic",
        "eduMultinomialLogistic", "eduReliabilityOmega"
    )
    for (analysis in analyses) {
        definition <- paste(readLines(file.path(jamovi_dir, paste0(analysis, ".a.yaml"))), collapse = "\n")
        ui <- paste(readLines(file.path(jamovi_dir, paste0(analysis, ".u.yaml"))), collapse = "\n")
        result <- paste(readLines(file.path(jamovi_dir, paste0(analysis, ".r.yaml"))), collapse = "\n")
        expect_match(definition, "title: Detail level", fixed = TRUE, info = analysis)
        expect_lt(regexpr("name: reportTone", definition, fixed = TRUE), regexpr("name: reportTable", definition, fixed = TRUE))
        expect_lt(regexpr("name: reportTable", definition, fixed = TRUE), regexpr("name: reportTableDetail", definition, fixed = TRUE))
        expect_match(ui, "name: reportTable", fixed = TRUE, info = analysis)
        expect_match(result, "name: apaTable", fixed = TRUE, info = analysis)
    }
})
