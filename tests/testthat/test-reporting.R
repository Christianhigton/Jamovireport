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
    expect_false(grepl("bootstrap CI", text, fixed = TRUE))
})

test_that("jamovi report content is rendered as structured HTML cards", {
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
    expect_match(overview, "jamovi Report")
    expect_match(overview, "Reporting controls")
    expect_match(report, "Copy-ready reporting")
    expect_match(report, "Welch independent-samples t-test")
    expect_match(interpretation, "What does this mean")
})

test_that("jamovi report display names prefer variable descriptions with name fallback", {
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
})

test_that("add-on report options update the guided report card content", {
    result <- edu_t_test(ToothGrowth, "len", "supp")
    options <- list(
        reportStyle = "plain", reportFormat = "bullets", reportTone = "detailed",
        reportDescriptives = TRUE, reportAssumptions = TRUE, reportStatistic = TRUE,
        reportDf = TRUE, reportP = TRUE, reportEffect = FALSE, reportCI = FALSE,
        reportInterpretation = TRUE, reportCautions = TRUE
    )

    report <- .jr_addon_report_html(list(result), options = options)

    expect_match(report, "What this analysis asks")
    expect_false(grepl("Cohen", report, fixed = TRUE))
})

test_that("native add-ons use a fixed automatic APA reporting profile", {
    result <- edu_t_test(ToothGrowth, "len", "supp", var_equal = TRUE)
    report <- .jr_addon_report_html(
        list(result), options = .jr_addon_reporting_options()
    )

    expect_match(report, "Student's independent-samples t-test")
    expect_match(report, "Cohen")
    expect_match(report, "95% CI")
    expect_match(.jr_addon_heading_html(), "jamovi Report")
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
        expect_match(report, "Report add-on")
        expect_false(grepl("Select valid analysis variables", report, fixed = TRUE))
        expect_false(grepl("could not be generated", report, fixed = TRUE))
    }
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
        expect_match(report, "Report add-on")
        expect_false(grepl("Select valid analysis variables", report, fixed = TRUE))
        expect_false(grepl("could not be generated", report, fixed = TRUE))
    }
})

test_that("automatic native reports display calculation failures clearly", {
    report <- .jr_addon_report_html(
        list(try(stop("example failure"), silent = TRUE)),
        options = .jr_addon_reporting_options()
    )

    expect_match(report, "Report could not be generated")
    expect_match(report, "example failure")
})

test_that("native add-ons register their installed dependency library path", {
    module_library <- file.path(tempdir(), "JamoviReport-module-library")
    dir.create(file.path(module_library, "JamoviReport"), recursive = TRUE, showWarnings = FALSE)
    original <- .libPaths()
    on.exit(.libPaths(original), add = TRUE)
    .libPaths(setdiff(original, module_library))

    expect_true(.jr_addon_enable_library(file.path(module_library, "JamoviReport")))
    expect_true(normalizePath(module_library) %in% normalizePath(.libPaths()))
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
