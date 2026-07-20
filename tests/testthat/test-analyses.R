test_that("independent t-test returns reportable statistics and diagnostics", {
    result <- edu_t_test(ToothGrowth, "len", "supp")

    expect_s3_class(result, "edu_analysis")
    expect_equal(result$analysis, "ttest")
    expect_true(all(c("statistic", "df", "p", "effect") %in% names(result$statistics)))
    expect_true(any(grepl("Homogeneity", result$diagnostics$check)))
    expect_match(edu_report(result), "Welch independent-samples t-test")
    expect_match(edu_report(result), "Cohen's d")
})

test_that("paired t-test evaluates change between paired columns", {
    d <- data.frame(
        pre = c(10, 9, 8, 12, 7, 11, 8, 13),
        post = c(8, 8, 7, 9, 8, 9, 7, 10)
    )
    result <- edu_t_test(d, "pre", paired_outcome = "post", type = "paired")

    expect_equal(result$statistics$test, "Paired t")
    expect_match(edu_report(result), "paired-samples")
    expect_true(result$statistics$p < .05)
})

test_that("non-parametric two-group tests report rank-biserial effects", {
    independent <- edu_mann_whitney(ToothGrowth, "len", "supp")
    paired_data <- data.frame(
        pre = c(10, 9, 8, 12, 7, 11, 8, 13),
        post = c(8, 8, 7, 9, 8, 9, 7, 10)
    )
    paired <- edu_wilcoxon_signed_rank(paired_data, "pre", "post")

    expect_equal(independent$analysis, "mann_whitney")
    expect_match(edu_report(independent), "Mann-Whitney U")
    expect_match(edu_report(independent), "rank-biserial r")
    expect_equal(paired$analysis, "wilcoxon_signed_rank")
    expect_match(edu_report(paired), "Wilcoxon signed-rank")
    expect_match(edu_report(paired), "rank-biserial r")
    expect_s3_class(edu_plot(independent), "ggplot")
    expect_s3_class(edu_plot(paired), "ggplot")
})

test_that("Bayesian t-test reports BF10 and selected prior", {
    skip_if_not(.jr_enable_bayesfactor_library(), "BayesFactor is not available.")
    result <- edu_bayes_t_test(ToothGrowth, "len", "supp", prior_width = .707)
    paired_data <- data.frame(
        pre = c(10, 9, 8, 12, 7, 11, 8, 13),
        post = c(8, 8, 7, 9, 8, 9, 7, 10)
    )
    paired <- edu_bayes_t_test(
        paired_data, "pre", paired_outcome = "post",
        type = "paired", prior_width = .707
    )

    expect_equal(result$analysis, "bayes_ttest")
    expect_match(edu_report(result), "BF10")
    expect_match(edu_report(result), "Cauchy prior width")
    expect_match(edu_report(paired), "Bayesian paired-samples")
})

test_that("one-way ANOVA reports effect size and follow-up comparisons", {
    data(ToothGrowth)
    ToothGrowth$dose <- factor(ToothGrowth$dose)
    result <- edu_anova_oneway(ToothGrowth, "len", "dose")

    expect_equal(result$analysis, "anova_oneway")
    expect_true(result$statistics$p < .001)
    expect_match(edu_report(result), "η²", fixed = TRUE)
    expect_false(is.null(result$posthoc))
})

test_that("between-subjects ANOVA reports factorial effects and interactions", {
    d <- ToothGrowth
    d$dose <- factor(d$dose)
    result <- edu_anova_between(d, "len", c("dose", "supp"))

    expect_equal(result$analysis, "anova_between")
    expect_true("dose:supp" %in% result$statistics$term)
    expect_match(edu_report(result), "ηp²", fixed = TRUE)
    expect_s3_class(edu_plot(result), "ggplot")
})

test_that("ANCOVA identifies the regression-slope assumption", {
    d <- ToothGrowth
    d$dose_num <- as.numeric(as.character(d$dose))
    result <- edu_ancova(d, "len", "supp", "dose_num")

    expect_equal(result$analysis, "ancova")
    expect_true("Homogeneity of regression slopes" %in% result$diagnostics$check)
    expect_match(edu_report(result, format = "paragraph", tone = "detailed"), "homogeneity of regression slopes assumption", ignore.case = TRUE)
})

test_that("MANOVA and MANCOVA report multivariate Pillai tests", {
    d <- iris
    manova_result <- edu_manova(d, c("Sepal.Length", "Sepal.Width"), "Species")
    d$Petal.Length.Centered <- d$Petal.Length - mean(d$Petal.Length)
    mancova_result <- edu_manova(
        d, c("Sepal.Length", "Sepal.Width"), "Species", "Petal.Length.Centered"
    )

    expect_equal(manova_result$label, "MANOVA")
    expect_equal(mancova_result$label, "MANCOVA")
    expect_equal(manova_result$analysis, "manova")
    expect_true(all(c("statistic", "df1", "df2", "p", "effect") %in% names(manova_result$statistics)))
    expect_match(edu_report(manova_result), "Pillai's trace")
    expect_match(edu_report(mancova_result), "MANCOVA")
    expect_true("Homogeneity of covariance matrices" %in% manova_result$diagnostics$check)
})

test_that("significant MANOVA and MANCOVA add Holm-adjusted univariate follow-ups", {
    d <- iris
    manova_result <- edu_manova(d, c("Sepal.Length", "Sepal.Width"), "Species")
    d$Petal.Length.Centered <- d$Petal.Length - mean(d$Petal.Length)
    mancova_result <- edu_manova(
        d, c("Sepal.Length", "Sepal.Width"), "Species", "Petal.Length.Centered"
    )

    expect_true(nrow(manova_result$followups) > 0L)
    expect_true(all(c("term", "outcome", "statistic", "df1", "df2", "p", "p_holm", "effect") %in% names(manova_result$followups)))
    expect_true(all(manova_result$followups$p_holm >= manova_result$followups$p))
    expect_true(nrow(mancova_result$followups) > 0L)
    expect_match(edu_report(manova_result), "Follow-up analyses")
    expect_match(edu_report(manova_result), "Holm procedure")
    expect_match(edu_report(manova_result), "Field, 2024")
})

test_that("non-significant MANOVA omits automatic follow-ups", {
    set.seed(123)
    d <- data.frame(
        group = factor(rep(c("A", "B"), each = 20)),
        y1 = rnorm(40),
        y2 = rnorm(40)
    )
    result <- edu_manova(d, c("y1", "y2"), "group")

    expect_equal(nrow(result$followups), 0L)
    expect_false(grepl("Follow-up analyses", edu_report(result), fixed = TRUE))
})

test_that("repeated-measures and mixed ANOVA return guided within-subject results", {
    set.seed(42)
    d <- data.frame(
        group = factor(rep(c("control", "treatment"), each = 15)),
        pre = rnorm(30, 10, 2),
        mid = rnorm(30, 11, 2),
        post = rnorm(30, 12, 2)
    )
    d$post[d$group == "treatment"] <- d$post[d$group == "treatment"] + 2

    repeated <- edu_anova_rm(d, c("pre", "mid", "post"), c("Pre", "Mid", "Post"))
    mixed <- edu_anova_mixed(d, c("pre", "mid", "post"), "group", c("Pre", "Mid", "Post"))

    expect_equal(repeated$analysis, "anova_rm")
    expect_true("occasion" %in% repeated$statistics$term)
    expect_true(any(grepl("Sphericity", repeated$diagnostics$check)))
    expect_true("group:occasion" %in% mixed$statistics$term)
    expect_s3_class(edu_plot(repeated), "ggplot")
    expect_s3_class(edu_plot(mixed), "ggplot")
})

test_that("correlation and regression provide educational reports", {
    correlation <- edu_correlation(mtcars, "mpg", "wt")
    regression <- edu_lm(mtcars, mpg ~ wt + hp)

    expect_match(edu_report(correlation), "Pearson correlation")
    expect_match(edu_report(regression), "R-squared")
    expect_true("beta" %in% names(regression$parameters))
    expect_s3_class(edu_plot(regression), "ggplot")
})

test_that("chi-square tests report expected-count diagnostics and effects", {
    d <- ToothGrowth
    d$dose <- factor(d$dose)
    independence <- edu_chisq_independence(d, "supp", "dose")
    preferences <- data.frame(
        choice = factor(c(rep("A", 20), rep("B", 10), rep("C", 10)))
    )
    goodness <- edu_chisq_gof(preferences, "choice", expected = c(1, 1, 1))

    expect_equal(independence$analysis, "chisq_independence")
    expect_match(edu_report(independence), "Cramer's V")
    expect_equal(goodness$analysis, "chisq_gof")
    expect_match(edu_report(goodness), "Cohen's w")
    expect_true(all(c("observed", "expected", "standardised_residual") %in% names(goodness$cells)))
    expect_true("Expected cell counts" %in% independence$diagnostics$check)
})

test_that("binomial logistic regression reports odds ratios and diagnostic guidance", {
    d <- mtcars
    d$am <- factor(d$am, labels = c("automatic", "manual"))
    result <- edu_logistic_regression(d, am ~ wt + hp)

    expect_equal(result$analysis, "logistic_regression")
    expect_match(edu_report(result), "binomial logistic regression", ignore.case = TRUE)
    expect_match(edu_report(result), "OR =")
    expect_false(grepl("OR = 0.00,", edu_report(result), fixed = TRUE))
    expect_match(edu_report(result), "OR = < 0.001")
    expect_true(all(c("OR", "CI_low", "CI_high") %in% names(result$parameters)))
    expect_true("Model convergence" %in% result$diagnostics$check)
})

test_that("reliability analysis reports omega with item-quality guidance", {
    data(bfi, package = "psych")
    scale <- psych::bfi[1:120, c("A1", "A2", "A3", "A4", "A5")]
    set.seed(42)
    result <- edu_reliability_omega(
        scale, names(scale), reverse_items = "A1",
        bootstrap = TRUE, boot_iterations = 20
    )
    text <- edu_report(result, format = "paragraph", tone = "detailed")

    expect_equal(result$analysis, "reliability_omega")
    expect_match(text, "McDonald's omega total")
    expect_match(text, "Cronbach's alpha")
    expect_match(text, "bootstrap CI")
    expect_match(text, "We recommend reporting McDonald's omega alongside Cronbach's alpha", fixed = TRUE)
    expect_match(text, "McDonald \\(1999\\)")
    expect_false(grepl("References:", text, fixed = TRUE))
    expect_false(grepl("Cohen, J. \\(1988\\)", text))
    expect_true("Item direction and consistency" %in% result$diagnostics$check)
    expect_s3_class(edu_plot(result), "ggplot")
    expect_no_error(eduReliabilityOmega(
        scale, items = names(scale), reverseItems = "A1",
        bootstrapCI = TRUE, bootstrapSamples = 20, showPlot = FALSE
    ))
})

test_that("jamovi entry points execute the shared MVP analyses", {
    a <- ToothGrowth
    a$dose <- factor(a$dose)
    a$dose_num <- as.numeric(as.character(a$dose))
    set.seed(42)
    repeated <- data.frame(
        group = factor(rep(c("control", "treatment"), each = 15)),
        pre = rnorm(30, 10, 2),
        mid = rnorm(30, 11, 2),
        post = rnorm(30, 12, 2)
    )

    expect_no_error(eduTTest(ToothGrowth, outcome = "len", group = "supp", showPlot = FALSE))
    expect_no_error(eduAnova(a, outcome = "len", group = "dose", showPlot = FALSE))
    expect_no_error(eduBetweenAnova(a, outcome = "len", factors = c("dose", "supp"), showPlot = FALSE))
    expect_no_error(eduAncova(a, outcome = "len", factors = "supp", covariates = "dose_num", showPlot = FALSE))
    expect_no_error(eduRMAnova(repeated, measures = c("pre", "mid", "post"), occasionLabels = "Pre, Mid, Post", showPlot = FALSE))
    expect_no_error(eduMixedAnova(repeated, measures = c("pre", "mid", "post"), group = "group", occasionLabels = "Pre, Mid, Post", showPlot = FALSE))
    expect_no_error(eduCorrelation(mtcars, x = "mpg", y = "wt", showPlot = FALSE))
    expect_no_error(eduRegression(mtcars, outcome = "mpg", predictors = c("wt", "hp"), showPlot = FALSE))
    binary <- mtcars
    binary$am <- factor(binary$am, labels = c("automatic", "manual"))
    expect_no_error(eduLogistic(binary, outcome = "am", covariates = c("wt", "hp")))
    expect_no_error(eduChiSquareIndependence(a, rowVariable = "supp", columnVariable = "dose"))
    choices <- data.frame(choice = factor(c(rep("A", 20), rep("B", 10), rep("C", 10))))
    expect_no_error(eduChiSquareGoodness(choices, variable = "choice", expectedRatios = "1, 1, 1"))
})
