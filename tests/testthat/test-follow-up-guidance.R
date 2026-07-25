test_that("significant unresolved one-way ANOVA receives follow-up guidance", {
    unresolved <- edu_anova_oneway(
        iris, "Sepal.Length", "Species", posthoc = FALSE
    )
    guidance <- .jr_follow_up_analysis_guidance(unresolved)
    html <- .jr_guided_report_sections_html(
        unresolved, .jr_addon_reporting_options()
    )

    expect_match(guidance, "Human decision required", fixed = TRUE)
    expect_match(guidance, "Holm", fixed = TRUE)
    expect_match(guidance, "References:", fixed = TRUE)
    expect_match(html, "Follow-up analysis", fixed = TRUE)
    expect_match(
        html,
        "the choice requires human judgement",
        fixed = TRUE
    )
})

test_that("completed, non-significant, and two-level tests avoid unnecessary guidance", {
    completed <- edu_anova_oneway(
        iris, "Sepal.Length", "Species", posthoc = TRUE
    )
    expect_identical(.jr_follow_up_analysis_guidance(completed), "")

    set.seed(42)
    null_data <- data.frame(
        score = stats::rnorm(90),
        group = factor(rep(c("a", "b", "c"), each = 30))
    )
    null_result <- edu_anova_oneway(
        null_data, "score", "group", posthoc = FALSE
    )
    expect_gte(null_result$statistics$p, .05)
    expect_identical(.jr_follow_up_analysis_guidance(null_result), "")

    two_by_two <- data.frame(
        row = rep(c("a", "b"), each = 100),
        column = c(
            rep(c("x", "y"), c(90, 10)),
            rep(c("x", "y"), c(10, 90))
        )
    )
    chi <- edu_chisq_independence(two_by_two, "row", "column")
    expect_lt(chi$statistics$p, .05)
    expect_identical(.jr_follow_up_analysis_guidance(chi), "")
})

test_that("factorial interactions and larger categorical tables request decisions", {
    factorial <- edu_anova_between(
        ToothGrowth, "len", c("supp", "dose")
    )
    factorial$statistics$p[] <- .001
    interaction_row <- grepl(":", factorial$statistics$term, fixed = TRUE)
    expect_true(any(interaction_row))
    guidance <- .jr_follow_up_analysis_guidance(factorial)
    expect_match(guidance, "simple-effects", fixed = TRUE)
    expect_match(guidance, "research question", fixed = TRUE)

    counts <- data.frame(
        row = rep(c("a", "b", "c"), each = 2),
        column = rep(c("x", "y"), 3),
        n = c(80, 20, 20, 80, 50, 50)
    )
    chi <- edu_chisq_independence(counts, "row", "column", "n")
    expect_lt(chi$statistics$p, .05)
    chi_guidance <- .jr_follow_up_analysis_guidance(chi)
    expect_match(chi_guidance, "standardised residuals", fixed = TRUE)
    expect_match(chi_guidance, "Agresti", fixed = TRUE)
})

test_that("automatic add-on labels follow-up guidance by analysis", {
    unresolved <- edu_anova_oneway(
        iris, "Sepal.Length", "Species", posthoc = FALSE
    )
    second <- unresolved
    second$call$outcome <- "Sepal.Width"
    html <- .jr_addon_report_html(
        list(unresolved, second),
        options = .jr_addon_reporting_options()
    )

    expect_match(html, "Follow-up analysis", fixed = TRUE)
    expect_match(html, "One-Way ANOVA:", fixed = TRUE)
})
