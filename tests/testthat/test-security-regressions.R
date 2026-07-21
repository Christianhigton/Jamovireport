security_formula_data <- function() {
    data <- iris
    names(data) <- c(
        "out come",
        "second ` outcome",
        "quote\" predictor",
        "covariate space",
        "stop(\"executed\")"
    )
    data[["binary outcome"]] <- factor(rep(c("no", "yes", "no"), length.out = nrow(data)))
    data
}

test_that("formula builders treat non-syntactic names as data symbols", {
    data <- security_formula_data()
    dangerous <- "stop(\"executed\")"

    formulas <- list(
        regression = .jr_formula("out come", c("quote\" predictor", dangerous)),
        interaction = .jr_formula(
            "out come",
            rhs = "covariate space",
            factorial = c(dangerous, "second ` outcome")
        ),
        multivariate = .jr_formula(c("out come", "second ` outcome"), dangerous),
        addon = .jr_parent_model_formula(
            "out come",
            "covariate space",
            list(list(c("covariate space"), c("covariate space", dangerous)))
        )
    )

    expect_equal(all.vars(formulas$regression), c("out come", "quote\" predictor", dangerous))
    expect_equal(
        all.vars(formulas$multivariate),
        c("out come", "second ` outcome", dangerous)
    )
    for (formula in formulas)
        expect_no_error(stats::model.frame(formula, data = data))
})

test_that("guided model families accept non-syntactic and code-like names", {
    data <- security_formula_data()
    dangerous <- "stop(\"executed\")"

    expect_no_error(suppressWarnings(edu_anova_between(data, "out come", dangerous)))
    expect_no_error(suppressWarnings(edu_ancova(data, "out come", dangerous, "covariate space")))
    expect_no_error(suppressWarnings(edu_manova(data, c("out come", "second ` outcome"), dangerous)))

    regression <- .jr_formula("out come", c("quote\" predictor", dangerous))
    logistic <- .jr_formula("binary outcome", c("quote\" predictor", dangerous))
    multinomial <- .jr_formula(dangerous, "quote\" predictor")
    expect_no_error(suppressWarnings(edu_lm(data, regression)))
    expect_no_error(suppressWarnings(edu_logistic_regression(data, logistic)))
    expect_no_error(suppressWarnings(edu_multinomial_logistic(data, multinomial)))
})

test_that("demographics omission HTML escapes variable metadata", {
    payload <- "<img src=x onerror=alert(1)> & \"quoted\""
    html <- .dm_omit_note_html(payload, character())

    expect_false(grepl("<img", html, fixed = TRUE))
    expect_true(grepl("&lt;img src=x onerror=alert(1)&gt;", html, fixed = TRUE))
    expect_true(grepl("&amp;", html, fixed = TRUE))
    expect_true(grepl("&quot;quoted&quot;", html, fixed = TRUE))
})

test_that("constant samples do not abort otherwise valid primary analyses", {
    data <- data.frame(
        outcome = c(rep(1, 5), 1:5),
        group = rep(c("constant", "varying"), each = 5)
    )

    result <- edu_t_test(data, "outcome", "group")
    diagnostic <- result$diagnostics[result$diagnostics$check == "Normality in constant", ]

    expect_equal(nrow(diagnostic), 1L)
    expect_equal(diagnostic$status, "Not assessed")
    expect_equal(diagnostic$tested, "No")
    expect_true(is.na(diagnostic$statistic))
    expect_true(is.na(diagnostic$p))
})

test_that("display label replacement is literal and rendered labels are escaped", {
    slash <- intToUtf8(92)
    label <- paste0("Scale ", slash, "1 $ trailing", slash)
    labels <- stats::setNames(label, "score")
    expect_identical(
        .jr_replace_variable_names("score and score", labels),
        paste0(label, " and ", label)
    )

    data <- ToothGrowth
    payload <- "<img src=x onerror=alert(1)>"
    attr(data$len, "description") <- payload
    result <- .jr_apply_variable_descriptions(edu_t_test(data, "len", "supp"), data)
    html <- .jr_jamovi_overview_html(result)
    expect_false(grepl("<img", html, fixed = TRUE))
    expect_true(grepl("&lt;img", html, fixed = TRUE))
})
