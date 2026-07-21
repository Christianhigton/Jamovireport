#' Guided one-way ANOVA
#'
#' @param data A data frame.
#' @param outcome Numeric outcome variable name.
#' @param group Grouping variable name with at least three levels.
#' @param method Primary omnibus test: standard ANOVA or Welch ANOVA.
#' @param ci Confidence level for effect sizes.
#' @param posthoc Whether to calculate follow-up comparisons.
#' @return An `edu_analysis` object.
#' @export
edu_anova_oneway <- function(data, outcome, group,
                             method = c("standard", "welch"),
                             ci = 0.95, posthoc = TRUE) {
    method <- match.arg(method)
    .jr_assert_columns(data, c(outcome, group))
    d <- .jr_complete(data, c(outcome, group))
    .jr_numeric(d[[outcome]], outcome)
    d$y <- d[[outcome]]
    d$g <- droplevels(factor(d[[group]]))
    if (nlevels(d$g) < 3L)
        .jr_stop("A one-way ANOVA requires a grouping variable with at least three levels.")
    fit <- stats::aov(y ~ g, data = d)
    standard <- summary(fit)[[1]]
    welch <- stats::oneway.test(y ~ g, data = d, var.equal = FALSE)
    if (method == "standard") {
        f_stat <- standard[["F value"]][1]
        df1 <- standard[["Df"]][1]
        df2 <- standard[["Df"]][2]
        p_value <- standard[["Pr(>F)"]][1]
        test_name <- "one-way ANOVA"
    } else {
        f_stat <- unname(welch$statistic)
        df1 <- unname(welch$parameter[1])
        df2 <- unname(welch$parameter[2])
        p_value <- welch$p.value
        test_name <- "Welch one-way ANOVA"
    }
    effect <- suppressMessages(effectsize::eta_squared(fit, partial = FALSE, ci = ci))
    split_y <- split(d$y, d$g)
    descriptives <- data.frame(
        group = names(split_y),
        n = vapply(split_y, length, integer(1)),
        mean = vapply(split_y, mean, numeric(1)),
        sd = vapply(split_y, stats::sd, numeric(1)),
        stringsAsFactors = FALSE
    )
    diagnostics <- rbind(
        .jr_shapiro(stats::residuals(fit), "Residual normality (Shapiro-Wilk)"),
        .jr_levene("y", "g", d)
    )
    posthoc_results <- NULL
    posthoc_text <- ""
    if (posthoc && p_value < .05) {
        if (method == "standard") {
            posthoc_results <- as.data.frame(stats::TukeyHSD(fit)$g)
            significant <- rownames(posthoc_results)[posthoc_results[["p adj"]] < .05]
            if (length(significant) > 0L)
                posthoc_text <- sprintf("Tukey-adjusted comparisons identified differences for %s.", paste(significant, collapse = ", "))
        } else {
            posthoc_results <- stats::pairwise.t.test(d$y, d$g, p.adjust.method = "holm", pool.sd = FALSE)
            posthoc_text <- "Holm-adjusted Welch pairwise comparisons are available for follow-up interpretation."
        }
    }
    eta <- effect$Eta2[1]
    eta_low <- effect$CI_low[1]
    eta_high <- effect$CI_high[1]
    sig_phrase <- if (p_value < .05) "indicated statistically significant" else "did not indicate statistically significant"
    apa <- sprintf(
        "A %s %s group differences in %s, F(%s, %s) = %s, p %s, \u03b7\u00b2 = %s, %s%% CI %s.%s",
        test_name, sig_phrase,
        outcome, .jr_num(df1, 2L), .jr_num(df2, 2L), .jr_num(f_stat),
        .jr_p(p_value), .jr_num(eta, 2L, TRUE), .jr_num(ci * 100, 0L),
        .jr_ci(eta_low, eta_high, 2L, TRUE),
        if (nzchar(posthoc_text)) paste0(" ", posthoc_text) else ""
    )
    plain <- sprintf(
        "This analysis compared mean %s scores across %s groups. %s",
        outcome, group,
        if (p_value < .05)
            "At least one group differs from another; follow-up comparisons locate the differences."
        else
            "There is no clear evidence of differences between the group means in this sample."
    )
    assumption_text <- .jr_diagnostic_text(diagnostics)
    caution <- if (any(diagnostics$status == "Caution"))
        paste("Caution:", assumption_text)
    else ""
    stats <- data.frame(test = test_name, statistic = f_stat, df1 = df1, df2 = df2,
                        p = p_value, effect = eta, ci_low = eta_low, ci_high = eta_high)
    result <- .new_edu_analysis(
        analysis = "anova_oneway", label = "One-Way ANOVA",
        question = sprintf("Do mean %s scores differ across three or more %s groups?", outcome, group),
        requirements = "A numeric outcome and one categorical grouping variable with independent observations.",
        main = stats, descriptives = descriptives, effects = effect,
        diagnostics = diagnostics, interpretation = plain, caution = caution,
        plot_data = data.frame(outcome = d$y, group = d$g),
        report_blocks = list(
            rationale = sprintf("A one-way ANOVA compares mean %s scores across independent %s groups.", outcome, group),
            descriptives = plain, apa = apa, assumptions = assumption_text, plain = plain
        ),
        statistics = stats, call = match.call()
    )
    result$posthoc <- posthoc_results
    result
}
