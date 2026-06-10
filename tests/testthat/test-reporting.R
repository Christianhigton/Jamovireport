test_that("report inclusion options alter copy-ready statistical text", {
    result <- edu_t_test(ToothGrowth, "len", "supp")
    minimal <- edu_report(
        result,
        format = "short",
        include = c("test")
    )

    expect_false(grepl("Cohen's d", minimal, fixed = TRUE))
    expect_false(grepl("95% CI", minimal, fixed = TRUE))
    expect_false(grepl("p =", minimal, fixed = TRUE))
    expect_false(grepl("t(", minimal, fixed = TRUE))
    expect_false(grepl("M =", minimal, fixed = TRUE))
})

test_that("plain-language format exposes interpretation and cautions", {
    result <- edu_lm(mtcars, mpg ~ wt + hp)
    text <- edu_report(result, style = "plain", format = "paragraph")

    expect_match(text, "What this analysis asks")
    expect_match(text, "predictors accounted for")
    expect_match(text, "Diagnostics")
})

test_that("ANOVA post-hoc reporting can be excluded", {
    d <- ToothGrowth
    d$dose <- factor(d$dose)
    result <- edu_anova_oneway(d, "len", "dose")
    no_followups <- edu_report(
        result,
        format = "short",
        include = c("descriptives", "test", "test_statistic", "df", "p", "effect_size", "ci")
    )

    expect_false(grepl("Tukey-adjusted", no_followups, fixed = TRUE))
})

test_that("factorial ANOVA effect sizes can be omitted from reporting", {
    d <- ToothGrowth
    d$dose <- factor(d$dose)
    result <- edu_anova_between(d, "len", c("dose", "supp"))
    text <- edu_report(
        result,
        format = "short",
        include = c("test", "test_statistic", "df", "p")
    )

    expect_false(grepl("partial eta-squared", text, fixed = TRUE))
    expect_false(grepl("Interpretation note", text, fixed = TRUE))
})

test_that("reported effect sizes include benchmarks and interpretation note", {
    result <- edu_t_test(ToothGrowth, "len", "supp")
    text <- edu_report(result, format = "short")
    rows <- .jr_addon_apa_rows(list(result))

    expect_match(text, "Effect-size benchmark")
    expect_match(text, "Interpretation note")
    expect_match(text, "\n\n\\*Interpretation note:")
    expect_match(text, "Cohen, 1988; Cumming, 2014", fixed = TRUE)
    expect_match(rows$effect[1], "large|medium|small|below small")
})

test_that("omega estimates and intervals follow reporting inclusion controls", {
    data(bfi, package = "psych")
    scale <- psych::bfi[1:120, c("A1", "A2", "A3", "A4", "A5")]
    set.seed(42)
    result <- edu_reliability_omega(
        scale, names(scale), reverse_items = "A1",
        bootstrap = TRUE, boot_iterations = 20
    )
    text <- edu_report(result, format = "short", include = c("test"))

    expect_false(grepl("omega =", text, fixed = TRUE))
    expect_false(grepl("alpha =", text, fixed = TRUE))
    expect_false(grepl("bootstrap CI", text, fixed = TRUE))
})

test_that("package and method references are declared for jamovi references", {
    module_dir <- system.file("jamovi", package = "jReport")
    if (!nzchar(module_dir))
        module_dir <- file.path(getwd(), "jamovi")
    if (!file.exists(file.path(module_dir, "eduReliabilityOmega.a.yaml")))
        skip("Source jamovi metadata is not available in this installed-package check context.")
    refs <- yaml::read_yaml(file.path(module_dir, "eduReliabilityOmega.a.yaml"))$description$references
    all_refs <- yaml::read_yaml(file.path(module_dir, "00refs.yaml"))$refs

    expect_true(all(c("jReport", "psych", "McDonald1999", "RevelleCondon2019") %in% refs))
    expect_true("jReport" %in% names(all_refs))
    expect_true("McDonald1999" %in% names(all_refs))
    expect_true("RevelleCondon2019" %in% names(all_refs))
    result_refs <- yaml::read_yaml(file.path(module_dir, "eduReliabilityOmega.r.yaml"))$refs
    expect_true(all(c("jReport", "psych", "McDonald1999", "RevelleCondon2019") %in% result_refs))
})

test_that("jReport content is rendered as structured HTML cards", {
    result <- edu_t_test(ToothGrowth, "len", "supp")
    options <- list(
        reportStyle = "apa7", reportFormat = "paragraph", reportTone = "student_friendly",
        reportDescriptives = TRUE, reportAssumptions = TRUE, reportStatistic = TRUE,
        reportDf = TRUE, reportP = TRUE, reportEffect = TRUE, reportCI = TRUE,
        reportInterpretation = TRUE, reportCautions = TRUE
    )

    overview <- .jr_jamovi_overview_html(result)
    report <- .jr_jamovi_report_html(result, options)
    interpretation <- .jr_jamovi_interpretation_html(result)

    expect_match(overview, "<div")
    expect_match(overview, "jReport")
    expect_match(overview, "Reporting controls")
    expect_match(report, "Copy-ready reporting")
    expect_match(report, "Welch independent-samples t-test")
    expect_match(report, "border-left:4px solid #4b66a2", fixed = TRUE)
    expect_match(report, "Interpretation note:")
    expect_match(interpretation, "What does this mean")
})

test_that("jReport display names prefer variable descriptions with name fallback", {
    d <- ToothGrowth
    attr(d$len, "description") <- "Tooth length in millimetres"
    attr(d$supp, "description") <- " "
    attr(d$supp, "label") <- "Supplement label should not be used"
    result <- .jr_apply_variable_descriptions(edu_t_test(d, "len", "supp"), d)
    text <- edu_report(result, style = "plain", format = "paragraph")

    expect_match(text, "Tooth length in millimetres", fixed = TRUE)
    expect_match(text, "supp", fixed = TRUE)
    expect_false(grepl("Supplement label should not be used", text, fixed = TRUE))
    expect_match(result$question, "Tooth length in millimetres", fixed = TRUE)
    expect_match(result$question, "supp", fixed = TRUE)
})

test_that("MANOVA report display names use variable descriptions", {
    d <- iris
    attr(d$Sepal.Length, "description") <- "Sepal length"
    attr(d$Sepal.Width, "description") <- "Sepal width"
    attr(d$Species, "description") <- "Flower species"
    result <- .jr_apply_variable_descriptions(
        edu_manova(d, c("Sepal.Length", "Sepal.Width"), "Species"),
        d
    )
    text <- edu_report(result, style = "apa7", format = "short")

    expect_match(text, "Sepal length", fixed = TRUE)
    expect_match(text, "Sepal width", fixed = TRUE)
    expect_match(text, "Flower species", fixed = TRUE)
    expect_equal(result$statistics$term[1], "Flower species")
    expect_true(all(c("Sepal length", "Sepal width") %in% result$followups$outcome))
})

test_that("variable display names ignore blank and identical descriptions", {
    d <- data.frame(
        raw = rnorm(10),
        described = rnorm(10)
    )
    attr(d$raw, "description") <- "raw"
    attr(d$described, "description") <- "Meaningful outcome"

    labels <- .jr_variable_display_labels(d)

    expect_equal(labels[["raw"]], "raw")
    expect_equal(labels[["described"]], "Meaningful outcome")
})

test_that("chi-square goodness add-on accepts atomic and list expected ratios", {
    list_ratios <- list(list(ratio = 1), list(ratio = 2), list(ratio = 3))
    atomic_ratios <- c(1, 2, 3)
    malformed_ratios <- list(list(ratio = 1), list(other = 2), list(ratio = 3))

    expect_equal(.jr_expected_ratio_values(list_ratios), c(1, 2, 3))
    expect_equal(.jr_expected_ratio_values(atomic_ratios), c(1, 2, 3))
    expect_null(.jr_expected_ratio_values(malformed_ratios))
})

test_that("add-on report options update the guided report card content", {
    result <- edu_t_test(ToothGrowth, "len", "supp")
    options_no_effect <- list(
        reportStyle = "plain", reportFormat = "bullets", reportTone = "detailed",
        reportDescriptives = TRUE, reportAssumptions = TRUE, reportStatistic = TRUE,
        reportDf = TRUE, reportP = TRUE, reportEffect = FALSE, reportCI = FALSE,
        reportInterpretation = TRUE, reportCautions = TRUE
    )
    options_with_effect <- list(
        reportStyle = "apa7", reportFormat = "paragraph", reportTone = "concise",
        reportDescriptives = TRUE, reportAssumptions = TRUE, reportStatistic = TRUE,
        reportDf = TRUE, reportP = TRUE, reportEffect = TRUE, reportCI = TRUE,
        reportInterpretation = FALSE, reportCautions = FALSE
    )

    report_no_effect <- .jr_addon_report_html(list(result), options = options_no_effect)
    report_with_effect <- .jr_addon_report_html(list(result), options = options_with_effect)

    expect_match(report_no_effect, "What this analysis asks")
    expect_match(report_no_effect, "Suggested APA-style report wording")
    expect_match(report_with_effect, "Cohen")
})

test_that("native add-ons use a fixed automatic APA reporting profile", {
    result <- edu_t_test(ToothGrowth, "len", "supp", var_equal = TRUE)
    report <- .jr_addon_report_html(
        list(result), options = .jr_addon_reporting_options()
    )

    expect_match(report, "Student's independent-samples t-test")
    expect_match(report, "Cohen")
    expect_match(report, "95% CI")
    expect_false(grepl("GAMLj", report, fixed = TRUE))
    expect_match(.jr_addon_heading_html(), "jReport")
})

test_that("automatic native reports render for supported group comparison designs", {
    d <- ToothGrowth
    d$dose <- factor(d$dose)
    d$dose_num <- as.numeric(as.character(d$dose))
    paired <- data.frame(
        pre = c(10, 9, 8, 12, 7, 11, 8, 13),
        post = c(8, 8, 7, 9, 8, 9, 7, 10)
    )
    set.seed(42)
    repeated <- data.frame(
        group = factor(rep(c("control", "treatment"), each = 15)),
        pre = rnorm(30, 10, 2),
        mid = rnorm(30, 11, 2),
        post = rnorm(30, 12, 2)
    )

    results <- list(
        edu_t_test(d, "len", "supp", var_equal = FALSE),
        edu_mann_whitney(d, "len", "supp"),
        edu_t_test(paired, "pre", paired_outcome = "post", type = "paired"),
        edu_wilcoxon_signed_rank(paired, "pre", "post"),
        edu_anova_oneway(d, "len", "dose", method = "standard", posthoc = FALSE),
        edu_anova_oneway(d, "len", "dose", method = "welch", posthoc = FALSE),
        edu_anova_between(d, "len", c("dose", "supp")),
        edu_ancova(d, "len", "supp", "dose_num"),
        edu_manova(iris, c("Sepal.Length", "Sepal.Width"), "Species"),
        edu_anova_rm(repeated, c("pre", "mid", "post"), c("Pre", "Mid", "Post")),
        edu_anova_mixed(repeated, c("pre", "mid", "post"), "group", c("Pre", "Mid", "Post"))
    )

    for (result in results) {
        report <- .jr_addon_report_html(
            list(result), options = .jr_addon_reporting_options()
        )
        expect_match(report, "Suggested APA-style report wording")
        expect_false(grepl("Select valid analysis variables", report, fixed = TRUE))
        expect_false(grepl("could not be generated", report, fixed = TRUE))
        expect_false(grepl("GAMLj", report, fixed = TRUE))
    }
})

test_that("between-subjects ANOVA report uses separated guidance sections", {
    d <- ToothGrowth
    d$dose <- factor(d$dose)
    result <- edu_anova_between(d, "len", c("dose", "supp"))
    report <- .jr_anova_between_report_sections_html(
        result,
        options = .jr_addon_reporting_options(),
        note = .jr_accuracy_note("This generated paragraph describes the selected factorial design.")
    )

    expect_match(report, "Suggested APA-style report wording")
    expect_match(report, "This is suggested wording only. Check all values against your jamovi output")
    expect_match(report, "Optional assumptions / diagnostic note")
    expect_match(report, "Interpretation guidance")
    expect_match(report, "For understanding only - do not copy directly into your report.")
    expect_match(report, "Check before reporting")
    expect_match(report, "A between-subjects ANOVA examined")
    expect_match(report, "Descriptive statistics indicated")
    expect_match(report, "M = ")
    expect_match(report, "SD = ")
    expect_match(report, "\u03b7p\u00b2")
    expect_match(report, "\u03c9p\u00b2")
    expect_match(report, "Assumption checks did not indicate substantial violations")
    expect_match(report, "Residual normality \\(Shapiro-Wilk\\)")
    expect_match(report, "A between-subjects ANOVA compares mean")
    expect_match(report, "Effect-size benchmark")
    expect_match(report, "The correct dependent variable is reported.")
    expect_match(report, "Correction methods are named correctly.")
    expect_false(grepl("Copy-ready report text", report, fixed = TRUE))

    wording_start <- regexpr("Suggested APA-style report wording", report, fixed = TRUE)
    diagnostics_start <- regexpr("Optional assumptions / diagnostic note", report, fixed = TRUE)
    wording_card <- substr(report, wording_start, diagnostics_start - 1L)

    expect_true(grepl("Descriptive statistics indicated", wording_card, fixed = TRUE))
    expect_true(grepl("partial omega squared", wording_card, fixed = TRUE))
    expect_false(grepl("Residual normality", wording_card, fixed = TRUE))
    expect_false(grepl("For understanding only", wording_card, fixed = TRUE))
})

test_that("between-subjects ANOVA references appear in report and jamovi result refs", {
    skip_if_not_installed("yaml")

    root <- getwd()
    while (!file.exists(file.path(root, "DESCRIPTION"))) {
        parent <- dirname(root)
        if (identical(parent, root))
            skip("Source package root is not available in this installed-package check context.")
        root <- parent
    }
    module_dir <- file.path(root, "jamovi")
    if (!file.exists(file.path(module_dir, "eduBetweenAnova.r.yaml")))
        skip("Source jamovi metadata is not available in this installed-package check context.")

    refs_yaml <- yaml::read_yaml(file.path(module_dir, "00refs.yaml"))
    ref_keys <- names(refs_yaml$refs)
    result_yaml <- yaml::read_yaml(file.path(module_dir, "eduBetweenAnova.r.yaml"))
    report_item <- Filter(function(item) identical(item$name, "report"), result_yaml$items)[[1]]
    stand_alone_refs <- c("jReport", "afex", "car", "effectsize", "emmeans", "ggplot2")
    expect_true(all(stand_alone_refs %in% report_item$refs))
    expect_true(all(report_item$refs %in% ref_keys))

    addon_yaml <- yaml::read_yaml(file.path(module_dir, "jrReportAnova.r.yaml"))
    addon_refs <- c("jReport", "jmvcore", "afex", "effectsize", "emmeans")
    for (name in c("jReportApaTable", "jReportAssumptions", "jReportPostHoc")) {
        item <- Filter(function(item) identical(item$name, name), addon_yaml$items)[[1]]
        expect_true(all(addon_refs %in% item$refs))
        expect_true(all(item$refs %in% ref_keys))
    }
    html_items <- Filter(function(item) identical(item$type, "Html"), addon_yaml$items)
    html_names <- sapply(html_items, `[[`, "name")
    expect_true("jReportHeading" %in% html_names)
    expect_true("jReportCard" %in% html_names)

    # Items are declared in h.R files (jReport namespace, for refs collection)
    hr_text <- paste(readLines(file.path(root, "R", "jrReportAnova.h.R"), warn = FALSE), collapse = "\n")
    expect_true(grepl('"jReportApaTable"', hr_text, fixed = TRUE))
    expect_true(grepl('"jReportAssumptions"', hr_text, fixed = TRUE))
    expect_true(grepl('"jReportHeading"', hr_text, fixed = TRUE))
    expect_true(grepl('"jReportCard"', hr_text, fixed = TRUE))
    expect_true(grepl('refs=list(', hr_text, fixed = TRUE))

    # helpers.R adds display items to self$parent$results and injects refs into parent namespace
    helper_text <- paste(readLines(file.path(root, "R", "jamovi-helpers.R"), warn = FALSE), collapse = "\n")
    expect_true(grepl('self$parent$results', helper_text, fixed = TRUE))
    expect_true(grepl('.jr_addon_inject_refs', helper_text, fixed = TRUE))
    expect_true(grepl('"jReportApaTable"', helper_text, fixed = TRUE))
    expect_true(grepl('"jReportAssumptions"', helper_text, fixed = TRUE))

    d <- ToothGrowth
    d$dose <- factor(d$dose)
    result <- edu_anova_between(d, "len", c("dose", "supp"))
    report <- .jr_anova_between_report_sections_html(result, options = .jr_addon_reporting_options())

    expect_match(report, "Suggested APA-style report wording")
    expect_match(report, "Check before reporting")
})

test_that("one-way between-subjects ANOVA suggested wording reports omega squared", {
    d <- ToothGrowth
    d$dose <- factor(d$dose)
    result <- edu_anova_between(d, "len", "dose")
    report <- .jr_anova_between_report_sections_html(
        result,
        options = .jr_addon_reporting_options()
    )
    wording_start <- regexpr("Suggested APA-style report wording", report, fixed = TRUE)
    diagnostics_start <- regexpr("Optional assumptions / diagnostic note", report, fixed = TRUE)
    wording_card <- substr(report, wording_start, diagnostics_start - 1L)

    expect_match(wording_card, "\u03c9\u00b2")
    expect_match(wording_card, "\u03b7p\u00b2")
    expect_match(wording_card, "M = ")
    expect_match(wording_card, "SD = ")
})

test_that("between-subjects ANOVA add-on keeps post hoc wording in suggested section", {
    skip_if_not_installed("emmeans")

    set.seed(2)
    d <- expand.grid(first = factor(c("A", "B")), second = factor(c("C", "D")), rep = seq_len(30))
    match_cell <- (d$first == "A" & d$second == "C") |
        (d$first == "B" & d$second == "D")
    d$outcome <- ifelse(match_cell, 8, 0) + stats::rnorm(nrow(d), sd = .5)
    result <- edu_anova_between(d, "outcome", c("first", "second"))
    result$posthoc_report <- .jr_model_posthoc(result, list(list("first", "second")), "holm")

    report <- .jr_addon_report_html(
        list(result), options = .jr_addon_reporting_options()
    )
    wording_start <- regexpr("Suggested APA-style report wording", report, fixed = TRUE)
    diagnostics_start <- regexpr("Optional assumptions / diagnostic note", report, fixed = TRUE)
    wording_card <- substr(report, wording_start, diagnostics_start - 1L)

    expect_match(report, "Suggested APA-style report wording")
    expect_match(report, "Optional assumptions / diagnostic note")
    expect_match(report, "Interpretation guidance")
    expect_false(grepl("Post hoc interpretation", report, fixed = TRUE))
    expect_true(grepl("Holm-adjusted post hoc comparisons", wording_card, fixed = TRUE))
    expect_true(grepl("mean difference", wording_card, fixed = TRUE))
})

test_that("automatic native report renders selected Bayesian t-test paths", {
    skip_if_not(.jr_enable_bayesfactor_library(), "BayesFactor is not available.")
    paired <- data.frame(
        pre = c(10, 9, 8, 12, 7, 11, 8, 13),
        post = c(8, 8, 7, 9, 8, 9, 7, 10)
    )
    results <- list(
        edu_bayes_t_test(ToothGrowth, "len", "supp"),
        edu_bayes_t_test(paired, "pre", paired_outcome = "post", type = "paired")
    )

    report <- .jr_addon_report_html(results, options = .jr_addon_reporting_options())
    rows <- .jr_addon_apa_rows(results)

    expect_match(report, "BF10")
    expect_equal(nrow(rows), 2L)
    expect_true(all(grepl("BF10", rows$effect, fixed = TRUE)))
})

test_that("automatic native reports render for association, model, and omega paths", {
    data(bfi, package = "psych")
    scale <- psych::bfi[1:120, c("A1", "A2", "A3", "A4", "A5")]
    binary <- mtcars
    binary$am <- factor(binary$am, labels = c("automatic", "manual"))
    frequency <- ToothGrowth
    frequency$dose <- factor(frequency$dose)
    choices <- data.frame(choice = factor(c(rep("A", 20), rep("B", 10), rep("C", 10))))
    results <- list(
        edu_correlation(mtcars, "mpg", "wt", method = "pearson"),
        edu_correlation(mtcars, "mpg", "wt", method = "spearman"),
        edu_correlation(mtcars, "mpg", "wt", method = "kendall"),
        edu_lm(mtcars, mpg ~ wt + hp),
        edu_logistic_regression(binary, am ~ wt + hp),
        edu_chisq_independence(frequency, "supp", "dose"),
        edu_chisq_gof(choices, "choice"),
        edu_reliability_omega(
            scale, names(scale), reverse_items = "A1", bootstrap = FALSE
        )
    )

    for (result in results) {
        report <- .jr_addon_report_html(
            list(result), options = .jr_addon_reporting_options()
        )
        if (identical(result$analysis, "regression"))
            expect_match(report, "Copy-ready report text")
        else
            expect_match(report, "Suggested APA-style report wording")
        expect_false(grepl("Select valid analysis variables", report, fixed = TRUE))
        expect_false(grepl("could not be generated", report, fixed = TRUE))
    }
})

test_that("linear regression add-on separates copy-ready text from guidance", {
    result <- edu_lm(mtcars, mpg ~ wt + hp)
    report <- .jr_addon_report_html(
        list(result),
        options = .jr_addon_reporting_options(),
        note = .jr_accuracy_note("This generated paragraph reports the selected predictors.")
    )

    expect_match(report, "Copy-ready report text")
    expect_match(report, "Select and copy this paragraph into your report.")
    expect_match(report, "Optional assumptions / diagnostic note")
    expect_match(report, "Include this only if relevant to your study.")
    expect_match(report, "Interpretation guidance")
    expect_match(report, "For understanding only - do not copy directly.")
    expect_match(report, "Check before reporting")
    expect_match(report, "Outcome variable is the intended dependent variable.")
    expect_match(report, "b, SE, beta, t, p, and confidence intervals")

    copy_start <- regexpr("Copy-ready report text", report, fixed = TRUE)
    diagnostic_start <- regexpr("Optional assumptions / diagnostic note", report, fixed = TRUE)
    copy_card <- substr(report, copy_start, diagnostic_start - 1L)

    expect_true(grepl(.jr_html_escape(result$report_blocks$apa), copy_card, fixed = TRUE))
    expect_false(grepl("For understanding only", copy_card, fixed = TRUE))
    expect_false(grepl("Together, the predictors accounted", copy_card, fixed = TRUE))
})

test_that("automatic native reports display calculation failures clearly", {
    report <- .jr_addon_report_html(
        list(try(stop("example failure"), silent = TRUE)),
        options = .jr_addon_reporting_options()
    )

    expect_match(report, "Report could not be generated")
    expect_match(report, "example failure")
})

test_that("native add-ons do not mutate the global library path", {
    original <- .libPaths()

    expect_true(.jr_addon_enable_library())
    expect_equal(.libPaths(), original)
    package_code <- unlist(lapply(
        list.files("R", pattern = "\\.R$", full.names = TRUE),
        readLines,
        warn = FALSE
    ))
    expect_false(any(grepl("\\.libPaths\\(", package_code)))
})

test_that("native add-ons generate APA results table rows across analysis families", {
    d <- ToothGrowth
    d$dose <- factor(d$dose)
    binary <- mtcars
    binary$am <- factor(binary$am, labels = c("automatic", "manual"))
    rows <- .jr_addon_apa_rows(list(
        edu_t_test(d, "len", "supp"),
        edu_mann_whitney(d, "len", "supp"),
        edu_anova_oneway(d, "len", "dose", posthoc = FALSE),
        edu_correlation(mtcars, "mpg", "wt"),
        edu_lm(mtcars, mpg ~ wt + hp),
        edu_logistic_regression(binary, am ~ wt + hp)
    ))

    expect_equal(nrow(rows), 6L)
    expect_named(rows, c("analysis", "test", "statistic", "df1", "df2", "p", "effect", "ci"))
    expect_equal(rows$df2[1], "")
    expect_true(nzchar(rows$df2[3]))
    expect_match(rows$effect[1], "Cohen's d")
    expect_match(rows$effect[2], "Rank-biserial")
    expect_match(rows$effect[3], "Eta-squared")
    expect_match(rows$effect[4], "pearson")
    expect_match(rows$effect[5], "R-squared")
    expect_match(rows$effect[6], "McFadden")
    expect_equal(rows$ci[1], .jr_ci(
        edu_t_test(d, "len", "supp")$effects$CI_low[1],
        edu_t_test(d, "len", "supp")$effects$CI_high[1],
        2L, TRUE
    ))
    expect_true(all(is.finite(rows$statistic)))
})

test_that("native logistic add-on creates odds-ratio rows for predictors", {
    d <- mtcars
    d$am <- factor(d$am, labels = c("automatic", "manual"))
    result <- edu_logistic_regression(d, am ~ wt + hp)
    rows <- .jr_addon_coefficient_rows(list(result))

    expect_equal(nrow(rows), 2L)
    expect_named(rows, c("predictor", "estimate", "se", "statistic", "p", "odds_ratio", "confidence_interval"))
    expect_true(all(rows$odds_ratio > 0))
})

test_that("model-dependent report warning requires accuracy checking", {
    warning <- .jr_accuracy_note("Generated wording.")

    expect_match(warning, "must be checked for accuracy", ignore.case = TRUE)
    expect_match(warning, "reference levels")
})

test_that("native logistic helper honours selected interaction terms and reference levels", {
    formula <- .jr_parent_model_formula("outcome", c("x", "group"), list(list(c("x"), c("x", "group"))))
    d <- data.frame(outcome = factor(c("no", "yes")), group = factor(c("a", "b")))
    adjusted <- .jr_apply_reference_levels(
        d, list(list(var = "outcome", ref = "yes"), list(var = "group", ref = "b"))
    )

    expect_equal(attr(stats::terms(formula), "term.labels"), c("x", "x:group"))
    expect_equal(levels(adjusted$outcome)[1], "yes")
    expect_equal(levels(adjusted$group)[1], "b")
})

test_that("native assumption table identifies concerns and recommended actions", {
    result <- edu_t_test(ToothGrowth, "len", "supp")
    rows <- .jr_addon_assumption_rows(list(result))

    expect_named(rows, c("analysis", "assumption", "tested", "statistic", "p", "met", "interpretation", "action"))
    expect_true(any(rows$met == "Concern"))
    expect_true(all(rows$tested %in% c("Yes", "No")))
    expect_true(any(nzchar(rows$action)))
    expect_match(rows$assumption, "Normality|Homogeneity", all = FALSE)
})

test_that("native chi-square add-ons create observed and expected cell rows", {
    d <- ToothGrowth
    d$dose <- factor(d$dose)
    choices <- data.frame(choice = factor(c(rep("A", 20), rep("B", 10), rep("C", 10))))
    rows <- .jr_addon_cell_rows(list(
        edu_chisq_independence(d, "supp", "dose"),
        edu_chisq_gof(choices, "choice")
    ))

    expect_equal(nrow(rows), 9L)
    expect_named(rows, c("analysis", "category", "observed", "expected", "standardised_residual"))
    expect_true(all(rows$expected > 0))
})

test_that("one-way ANOVA native post hocs generate APA-ready Tukey and Games-Howell rows", {
    d <- ToothGrowth
    d$dose <- factor(d$dose)
    tukey <- .jr_oneway_posthoc(d, "len", "dose", "tukey")
    games_howell <- .jr_oneway_posthoc(d, "len", "dose", "gamesHowell")

    expect_equal(nrow(tukey), 3L)
    expect_equal(nrow(games_howell), 3L)
    expect_true(all(tukey$adjustment == "Tukey"))
    expect_true(all(games_howell$adjustment == "Games-Howell"))
    expect_true(any(tukey$significant == "Yes"))
    expect_true(any(games_howell$significant == "Yes"))
})

test_that("automatic report explains selected ANOVA post hoc comparisons", {
    d <- ToothGrowth
    d$dose <- factor(d$dose)
    result <- edu_anova_oneway(d, "len", "dose", posthoc = FALSE)
    result$posthoc_report <- .jr_oneway_posthoc(d, "len", "dose", "tukey")
    rows <- .jr_addon_posthoc_rows(list(result))
    report <- .jr_addon_report_html(
        list(result), options = .jr_addon_reporting_options()
    )

    expect_equal(nrow(rows), 3L)
    expect_match(report, "Post hoc interpretation")
    expect_match(report, "Tukey-adjusted")
    expect_match(report, "mean difference")
})

test_that("post hoc reporting requires a significant omnibus effect", {
    d <- data.frame(
        score = rep(c(1, 2, 3, 4), 3),
        group = factor(rep(c("a", "b", "c"), each = 4))
    )
    result <- edu_anova_oneway(d, "score", "group", posthoc = TRUE)

    expect_false(.jr_has_significant_omnibus(result))
    expect_null(result$posthoc)
})

test_that("significant interactions trigger conditional post hoc follow-ups", {
    set.seed(2)
    d <- expand.grid(first = factor(c("A", "B")), second = factor(c("C", "D")), rep = seq_len(30))
    match_cell <- (d$first == "A" & d$second == "C") |
        (d$first == "B" & d$second == "D")
    d$outcome <- ifelse(match_cell, 8, 0) + stats::rnorm(nrow(d), sd = .5)
    result <- edu_anova_between(d, "outcome", c("first", "second"))
    terms <- .jr_significant_posthoc_terms(result, list("first"))

    expect_false(any(vapply(terms, function(term) identical(term, "first"), logical(1))))
    expect_true(any(vapply(terms, function(term) identical(term, c("first", "second")), logical(1))))
})

test_that("post hoc term parsing retains factorial interaction components", {
    expect_equal(.jr_posthoc_terms(~ dose:supp), list(c("dose", "supp")))
})

# --- Reference key propagation ---

test_that("Cohen1988 and Cumming2014 reference keys included for t-test", {
    result <- edu_t_test(ToothGrowth, "len", "supp")
    keys <- .jr_text_reference_keys(result)
    expect_true("Cohen1988" %in% keys)
    expect_true("Cumming2014" %in% keys)
})

test_that("Cohen1988 and Cumming2014 reference keys included for RM ANOVA", {
    set.seed(42)
    d <- data.frame(pre = rnorm(30, 10, 2), mid = rnorm(30, 11, 2), post = rnorm(30, 12, 2))
    result <- edu_anova_rm(d, c("pre", "mid", "post"), c("Pre", "Mid", "Post"))
    keys <- .jr_text_reference_keys(result)
    expect_true("Cohen1988" %in% keys)
    expect_true("Cumming2014" %in% keys)
})

test_that("Cohen1988 and Cumming2014 reference keys absent for reliability omega", {
    data(bfi, package = "psych")
    scale <- psych::bfi[1:120, c("A1", "A2", "A3", "A4", "A5")]
    result <- edu_reliability_omega(scale, names(scale), reverse_items = "A1", bootstrap = FALSE)
    keys <- .jr_text_reference_keys(result)
    expect_false("Cohen1988" %in% keys)
    expect_false("Cumming2014" %in% keys)
})

# --- ηG² and ηp² for RM/mixed ANOVA ---

test_that("RM ANOVA statistics include ges column with ges at most partial eta-squared", {
    set.seed(42)
    d <- data.frame(pre = rnorm(30, 10, 2), mid = rnorm(30, 11, 2), post = rnorm(30, 12, 2))
    result <- edu_anova_rm(d, c("pre", "mid", "post"), c("Pre", "Mid", "Post"))
    expect_true("ges" %in% names(result$statistics))
    valid <- is.finite(result$statistics$ges) & is.finite(result$statistics$effect)
    expect_true(any(valid))
    expect_true(all(result$statistics$ges[valid] <= result$statistics$effect[valid] + 1e-10))
})

test_that("mixed ANOVA statistics include ges column with ges at most partial eta-squared", {
    set.seed(42)
    d <- data.frame(
        group = factor(rep(c("control", "treatment"), each = 15)),
        pre = rnorm(30, 10, 2), mid = rnorm(30, 11, 2), post = rnorm(30, 12, 2)
    )
    d$post[d$group == "treatment"] <- d$post[d$group == "treatment"] + 2
    result <- edu_anova_mixed(d, c("pre", "mid", "post"), "group", c("Pre", "Mid", "Post"))
    expect_true("ges" %in% names(result$statistics))
    valid <- is.finite(result$statistics$ges) & is.finite(result$statistics$effect)
    expect_true(any(valid))
    expect_true(all(result$statistics$ges[valid] <= result$statistics$effect[valid] + 1e-10))
})

test_that(".jr_effect_sentences includes both ηG² and ηp² when ges is present", {
    set.seed(42)
    d <- data.frame(pre = rnorm(30, 10, 2), mid = rnorm(30, 11, 2), post = rnorm(30, 12, 2))
    result <- edu_anova_rm(d, c("pre", "mid", "post"), c("Pre", "Mid", "Post"))
    sentences <- .jr_effect_sentences(result$statistics)
    expect_match(sentences, "ηG²", fixed = TRUE)
    expect_match(sentences, "ηp²", fixed = TRUE)
})

test_that(".jr_rm_ges_guidance returns educational text for RM ANOVA", {
    set.seed(42)
    d <- data.frame(pre = rnorm(30, 10, 2), mid = rnorm(30, 11, 2), post = rnorm(30, 12, 2))
    result <- edu_anova_rm(d, c("pre", "mid", "post"), c("Pre", "Mid", "Post"))
    guidance <- .jr_rm_ges_guidance(result)
    expect_true(nzchar(guidance))
    expect_match(guidance, "Generalised eta squared", fixed = TRUE)
    expect_match(guidance, "ηG²", fixed = TRUE)
    expect_match(guidance, "ηp²", fixed = TRUE)
})

test_that(".jr_rm_ges_guidance returns empty string for non-RM analyses", {
    result <- edu_t_test(ToothGrowth, "len", "supp")
    expect_equal(.jr_rm_ges_guidance(result), "")
})

test_that(".jr_rm_ges_guidance discrepancy note triggers when ηp² exceeds twice ηG²", {
    set.seed(1)
    n <- 30
    # Large participant-level variation dwarfs the time effect so ηp² >> ηG²
    baselines <- seq(0, 100, length.out = n)
    d <- data.frame(
        pre  = baselines + rnorm(n, 0, 0.5),
        mid  = baselines + 0.5 + rnorm(n, 0, 0.5),
        post = baselines + 1 + rnorm(n, 0, 0.5)
    )
    result <- edu_anova_rm(d, c("pre", "mid", "post"), c("Pre", "Mid", "Post"))
    stats <- result$statistics
    valid <- is.finite(stats$effect) & is.finite(stats$ges)
    if (!any(valid) || !any(stats$effect[valid] > 2 * stats$ges[valid]))
        skip("Dataset did not produce a large enough ηp²/ηG² ratio")
    guidance <- .jr_rm_ges_guidance(result)
    expect_match(guidance, "individual differences", fixed = TRUE)
})
