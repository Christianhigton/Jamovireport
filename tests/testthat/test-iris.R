# Smoke and structure tests for all guided analyses using the iris dataset.
# iris provides a well-behaved fixture for most guided analyses. Logistic tests
# use overlapping built-in datasets to avoid deterministic separation warnings.
# Tests here focus on: no runtime error, correct result class, expected column
# names in tables, and key phrases in edu_report() output.

data(iris)

# --------------------------------------------------------------------------
# Helpers
# --------------------------------------------------------------------------

two_species <- function(exclude = "virginica") {
    d <- iris[iris$Species != exclude, ]
    d$Species <- droplevels(d$Species)
    d
}

binary_logistic_fixture <- function() {
    d <- mtcars
    d$am <- factor(d$am, labels = c("automatic", "manual"))
    d
}

multinomial_fixture <- function() {
    warpbreaks
}

iris_two_factor <- function() {
    d <- iris
    d$petal_size <- factor(ifelse(d$Petal.Length > median(d$Petal.Length), "large", "small"))
    d
}

iris_binned <- function() {
    data.frame(
        petal_size  = factor(ifelse(iris$Petal.Length > 3.5, "large", "small")),
        sepal_size  = factor(ifelse(iris$Sepal.Length > 5.8, "large", "small"))
    )
}

# --------------------------------------------------------------------------
# Core analysis — function-level tests
# --------------------------------------------------------------------------

test_that("t-test on two iris species returns correct structure", {
    result <- edu_t_test(two_species(), "Sepal.Length", "Species")

    expect_s3_class(result, "edu_analysis")
    expect_equal(result$analysis, "ttest")
    expect_true(all(c("statistic", "df", "p", "effect") %in% names(result$statistics)))
    expect_true(nrow(result$descriptives) == 2L)
    expect_true(any(grepl("Homogeneity", result$diagnostics$check)))
    expect_match(edu_report(result), "t-test")
    expect_match(edu_report(result), "Cohen's d")
    expect_s3_class(edu_plot(result), "ggplot")
})

test_that("one-way ANOVA on iris petal length across three species", {
    result <- edu_anova_oneway(iris, "Petal.Length", "Species")

    expect_s3_class(result, "edu_analysis")
    expect_equal(result$analysis, "anova_oneway")
    expect_true(result$statistics$p < .001)
    expect_true(nrow(result$descriptives) == 3L)
    expect_false(is.null(result$posthoc))
    expect_match(edu_report(result), "eta-squared")
    expect_s3_class(edu_plot(result), "ggplot")
})

test_that("between-subjects ANOVA on iris with two factors detects interaction term", {
    result <- edu_anova_between(iris_two_factor(), "Sepal.Length", c("Species", "petal_size"))

    expect_s3_class(result, "edu_analysis")
    expect_equal(result$analysis, "anova_between")
    expect_true(any(grepl("Species", result$statistics$term)))
    expect_true(any(grepl("petal_size", result$statistics$term)))
    expect_match(edu_report(result), "partial eta-squared")
    expect_s3_class(edu_plot(result), "ggplot")
})

test_that("ANCOVA on iris adjusts sepal length by petal length covariate", {
    result <- edu_ancova(iris, "Sepal.Length", "Species", "Petal.Length")

    expect_s3_class(result, "edu_analysis")
    expect_equal(result$analysis, "ancova")
    expect_true("Homogeneity of regression slopes" %in% result$diagnostics$check)
    expect_match(edu_report(result, style = "apa7"), "ANCOVA", ignore.case = TRUE)
})

test_that("correlation on iris sepal dimensions returns coefficient and plot", {
    result <- edu_correlation(iris, "Sepal.Length", "Sepal.Width")

    expect_s3_class(result, "edu_analysis")
    expect_equal(result$analysis, "correlation")
    expect_true(all(c("statistic", "p", "ci_low", "ci_high") %in% names(result$statistics)))
    expect_true(all(c("variable", "n", "mean", "sd") %in% names(result$descriptives)))
    expect_match(edu_report(result), "Pearson correlation")
    expect_s3_class(edu_plot(result), "ggplot")
})

test_that("linear regression on iris predicts sepal length correctly", {
    result <- edu_lm(iris, Sepal.Length ~ Petal.Length + Petal.Width)

    expect_s3_class(result, "edu_analysis")
    expect_equal(result$analysis, "regression")
    expect_true(all(c("r2", "adjusted_r2", "statistic", "p") %in% names(result$statistics)))
    expect_true(all(c("Parameter", "Coefficient", "SE", "beta", "t", "p") %in% names(result$parameters)))
    expect_match(edu_report(result), "R-squared")
    expect_s3_class(edu_plot(result), "ggplot")
})

test_that("binomial logistic regression on overlapping binary data runs correctly", {
    result <- edu_logistic_regression(binary_logistic_fixture(), am ~ wt + hp)

    expect_s3_class(result, "edu_analysis")
    expect_equal(result$analysis, "logistic_regression")
    expect_true(all(c("OR", "CI_low", "CI_high") %in% names(result$parameters)))
    expect_true("Model convergence" %in% result$diagnostics$check)
    expect_match(edu_report(result), "logistic regression", ignore.case = TRUE)
    expect_match(edu_report(result), "OR =")
})

test_that("multinomial logistic regression on overlapping three-class data runs without error", {
    result <- edu_multinomial_logistic(
        multinomial_fixture(), tension ~ breaks + wool, ci = 0.95
    )

    expect_s3_class(result, "edu_analysis")
    expect_equal(result$analysis, "multinomial_logistic")
    expect_true(all(c("statistic", "df", "p", "r2") %in% names(result$statistics)))
    expect_true(all(c("category", "term", "B", "SE", "z", "p", "RRR", "CI_low", "CI_high")
                    %in% names(result$parameters)))
    expect_match(edu_report(result, style = "apa7"), "multinomial logistic regression",
                 ignore.case = TRUE)
})

test_that("multinomial logistic regression produces one comparison per non-reference level", {
    result <- edu_multinomial_logistic(multinomial_fixture(), tension ~ breaks + wool)

    categories <- unique(result$parameters$category)
    # medium vs. low AND high vs. low tension
    expect_equal(length(categories), 2L)
    expect_true(any(grepl("M", categories)))
    expect_true(any(grepl("H", categories)))
})

test_that("multinomial logistic regression diagnostics include a convergence row", {
    result <- edu_multinomial_logistic(multinomial_fixture(), tension ~ breaks + wool)

    expect_true("Model convergence" %in% result$diagnostics$check)
    converged <- result$diagnostics[result$diagnostics$check == "Model convergence", "status"]
    expect_true(converged %in% c("Acceptable", "Serious"))  # status always set
})

test_that("multinomial logistic regression RRRs are positive", {
    result <- edu_multinomial_logistic(multinomial_fixture(), tension ~ breaks + wool)

    expect_true(all(result$parameters$RRR > 0))
})

test_that("MANOVA on iris species returns multivariate test and follow-ups", {
    result <- edu_manova(iris, c("Sepal.Length", "Sepal.Width"), "Species")

    expect_s3_class(result, "edu_analysis")
    expect_equal(result$analysis, "manova")
    expect_true(result$statistics$p < .001)
    expect_true(all(c("statistic", "df1", "df2", "p", "effect") %in% names(result$statistics)))
    expect_true(nrow(result$followups) > 0L)
    expect_match(edu_report(result), "Pillai's trace")
    expect_match(edu_report(result), "Follow-up analyses")
})

test_that("MANCOVA on iris with covariate runs without error", {
    d <- iris
    d$Petal.Length.c <- d$Petal.Length - mean(d$Petal.Length)
    result <- edu_manova(d, c("Sepal.Length", "Sepal.Width"), "Species", "Petal.Length.c")

    expect_s3_class(result, "edu_analysis")
    expect_equal(result$label, "MANCOVA")
    expect_match(edu_report(result), "MANCOVA")
})

test_that("chi-square independence on iris-derived categories runs correctly", {
    result <- edu_chisq_independence(iris_binned(), "petal_size", "sepal_size")

    expect_s3_class(result, "edu_analysis")
    expect_equal(result$analysis, "chisq_independence")
    expect_match(edu_report(result), "Cramer's V")
    expect_true("Expected cell counts" %in% result$diagnostics$check)
})

test_that("reliability omega on all four iris numeric columns runs correctly", {
    result <- edu_reliability_omega(
        iris[, 1:4],
        items = c("Sepal.Length", "Sepal.Width", "Petal.Length", "Petal.Width"),
        bootstrap = FALSE
    )

    expect_s3_class(result, "edu_analysis")
    expect_equal(result$analysis, "reliability_omega")
    expect_match(edu_report(result), "McDonald's omega total")
    expect_match(edu_report(result), "Cronbach's alpha")
    expect_s3_class(edu_plot(result), "ggplot")
})

# --------------------------------------------------------------------------
# Repeated-measures / mixed — synthetic data matching iris structure
# --------------------------------------------------------------------------

test_that("repeated-measures ANOVA on three iris measurement columns", {
    # Treat Sepal.Length, Petal.Length, Petal.Width as three occasions
    result <- edu_anova_rm(
        iris[, c("Sepal.Length", "Petal.Length", "Petal.Width")],
        measures = c("Sepal.Length", "Petal.Length", "Petal.Width"),
        levels = c("Sepal", "Petal.L", "Petal.W")
    )

    expect_s3_class(result, "edu_analysis")
    expect_equal(result$analysis, "anova_rm")
    expect_true("occasion" %in% result$statistics$term)
    expect_s3_class(edu_plot(result), "ggplot")
})

test_that("mixed ANOVA on iris with Species as between-subjects factor", {
    two_sp <- two_species()
    result <- edu_anova_mixed(
        two_sp,
        measures = c("Sepal.Length", "Petal.Length"),
        group    = "Species",
        levels   = c("Sepal", "Petal")
    )

    expect_s3_class(result, "edu_analysis")
    expect_equal(result$analysis, "anova_mixed")
    expect_true("group:occasion" %in% result$statistics$term)
    expect_s3_class(edu_plot(result), "ggplot")
})

# --------------------------------------------------------------------------
# jamovi entry points — no-error smoke tests
# --------------------------------------------------------------------------

test_that("eduTTest entry point runs on two-species iris subset", {
    expect_no_error(
        eduTTest(two_species(), outcome = "Sepal.Length", group = "Species", showPlot = FALSE)
    )
})

test_that("eduAnova entry point runs on full iris dataset", {
    expect_no_error(
        eduAnova(iris, outcome = "Petal.Length", group = "Species", showPlot = FALSE)
    )
})

test_that("eduBetweenAnova entry point runs on two-factor iris dataset", {
    expect_no_error(
        eduBetweenAnova(
            iris_two_factor(),
            outcome = "Sepal.Length", factors = c("Species", "petal_size"),
            showPlot = FALSE
        )
    )
})

test_that("eduAncova entry point runs on iris with petal length covariate", {
    expect_no_error(
        eduAncova(iris, outcome = "Sepal.Length", factors = "Species",
                  covariates = "Petal.Length", showPlot = FALSE)
    )
})

test_that("eduCorrelation entry point runs on iris sepal dimensions", {
    expect_no_error(
        eduCorrelation(iris, x = "Sepal.Length", y = "Sepal.Width", showPlot = FALSE)
    )
})

test_that("eduRegression entry point runs on iris petal predictors", {
    expect_no_error(
        eduRegression(
            iris, outcome = "Sepal.Length",
            predictors = c("Petal.Length", "Petal.Width"),
            showPlot = FALSE
        )
    )
})

test_that("eduLogistic entry point runs on overlapping binary outcome", {
    expect_no_error(
        eduLogistic(
            binary_logistic_fixture(),
            outcome = "am",
            covariates = c("wt", "hp")
        )
    )
})

test_that("eduMultinomialLogistic entry point runs on overlapping three-class outcome", {
    expect_no_error(
        eduMultinomialLogistic(
            multinomial_fixture(),
            outcome = "tension",
            covariates = "breaks",
            factors = "wool"
        )
    )
})

test_that("eduMultinomialLogistic entry point runs with factor predictor", {
    d <- iris_two_factor()
    expect_no_error(
        eduMultinomialLogistic(d, outcome = "Species", factors = "petal_size")
    )
})

test_that("eduMancova entry point runs on iris species as factor", {
    expect_no_error(
        eduMancova(
            iris,
            outcomes   = c("Sepal.Length", "Sepal.Width"),
            factors    = "Species",
            covariates = NULL
        )
    )
})

test_that("eduMancova entry point runs with covariate", {
    d <- iris
    d$Petal.Length.c <- d$Petal.Length - mean(d$Petal.Length)
    expect_no_error(
        eduMancova(
            d,
            outcomes    = c("Sepal.Length", "Sepal.Width"),
            factors     = "Species",
            covariates  = "Petal.Length.c"
        )
    )
})

test_that("eduRMAnova entry point runs on three iris numeric columns", {
    expect_no_error(
        eduRMAnova(
            iris,
            measures = c("Sepal.Length", "Petal.Length", "Petal.Width"),
            occasionLabels = "Sepal, Petal.L, Petal.W",
            showPlot = FALSE
        )
    )
})

test_that("eduMixedAnova entry point runs on two-species iris subset", {
    expect_no_error(
        eduMixedAnova(
            two_species(),
            measures = c("Sepal.Length", "Petal.Length"),
            group = "Species",
            occasionLabels = "Sepal, Petal",
            showPlot = FALSE
        )
    )
})

test_that("eduChiSquareIndependence entry point runs on iris-derived categories", {
    expect_no_error(
        eduChiSquareIndependence(iris_binned(), rowVariable = "petal_size",
                                 columnVariable = "sepal_size")
    )
})

test_that("eduReliabilityOmega entry point runs on iris numeric items", {
    expect_no_error(
        eduReliabilityOmega(
            iris[, 1:4],
            items = c("Sepal.Length", "Sepal.Width", "Petal.Length", "Petal.Width"),
            bootstrapCI = FALSE,
            showPlot = FALSE
        )
    )
})

# --------------------------------------------------------------------------
# Error handling — multinomial edge cases
# --------------------------------------------------------------------------

test_that("multinomial logistic rejects a binary outcome with a clear message", {
    d <- two_species()
    expect_error(
        edu_multinomial_logistic(d, Species ~ Sepal.Length),
        "3 or more",
        ignore.case = TRUE
    )
})

test_that("multinomial logistic rejects non-formula input", {
    expect_error(
        edu_multinomial_logistic(iris, "Species ~ Sepal.Length"),
        "formula"
    )
})
