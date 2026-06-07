#' Guided Mann-Whitney U test
#'
#' @param data A data frame.
#' @param outcome Numeric or ordinal outcome variable name.
#' @param group Group variable name with exactly two independent groups.
#' @param ci Confidence level for the rank-biserial effect size.
#' @return An `edu_analysis` object.
#' @export
edu_mann_whitney <- function(data, outcome, group, ci = 0.95) {
    .jr_assert_columns(data, c(outcome, group))
    d <- .jr_complete(data, c(outcome, group))
    .jr_numeric(d[[outcome]], outcome)
    d$y <- d[[outcome]]
    d$g <- droplevels(factor(d[[group]]))
    if (nlevels(d$g) != 2L)
        .jr_stop("A Mann-Whitney U test requires a grouping variable with exactly two levels.")

    test <- stats::wilcox.test(y ~ g, data = d, exact = FALSE, correct = TRUE)
    effect <- suppressMessages(effectsize::rank_biserial(d$y, d$g, ci = ci))
    split_y <- split(d$y, d$g)
    descriptives <- data.frame(
        group = names(split_y),
        n = vapply(split_y, length, integer(1)),
        median = vapply(split_y, stats::median, numeric(1)),
        iqr = vapply(split_y, stats::IQR, numeric(1)),
        stringsAsFactors = FALSE
    )
    diagnostics <- data.frame(
        check = c(
            "Independence of observations",
            "Similar distribution shape for median interpretation"
        ),
        statistic = c(NA_real_, NA_real_),
        p = c(NA_real_, NA_real_),
        status = c("Not assessed", "Not assessed"),
        interpretation = c(
            "The test requires separate, independent cases in the two groups; this is determined from the research design.",
            "A rank-based difference can be interpreted as a median difference only when group distributions have broadly similar shapes."
        ),
        action = c(
            "Confirm that no participant or matched unit contributes to both groups.",
            "Inspect grouped boxplots or violin plots and describe the result as a rank-distribution difference if shapes differ."
        ),
        stringsAsFactors = FALSE
    )
    effect_value <- effect$r_rank_biserial[1]
    effect_ci <- effect[1, c("CI_low", "CI_high")]
    sig_phrase <- if (test$p.value < .05)
        "indicated a statistically significant difference"
    else
        "did not indicate a statistically significant difference"
    apa <- sprintf(
        "A Mann-Whitney U test %s in %s between %s (Mdn = %s, IQR = %s) and %s (Mdn = %s, IQR = %s), U = %s, p %s, rank-biserial r = %s, %s%% CI %s.",
        sig_phrase,
        outcome, descriptives$group[1], .jr_num(descriptives$median[1]),
        .jr_num(descriptives$iqr[1]), descriptives$group[2],
        .jr_num(descriptives$median[2]), .jr_num(descriptives$iqr[2]),
        .jr_num(test$statistic), .jr_p(test$p.value),
        .jr_num(effect_value, 2L, TRUE), .jr_num(ci * 100, 0L),
        .jr_ci(effect_ci$CI_low, effect_ci$CI_high, 2L, TRUE)
    )
    question <- sprintf("Do the %s score distributions differ between the two %s groups?", outcome, group)
    requirements <- "An ordinal or numeric outcome measured for two independent groups."
    plain <- sprintf(
        "This rank-based analysis compared %s scores for %s and %s. %s",
        outcome, descriptives$group[1], descriptives$group[2],
        if (test$p.value < .05)
            "The data provide evidence that scores tend to be higher in one group than the other."
        else
            "The data do not provide clear evidence that scores tend to differ between groups."
    )
    statistics <- data.frame(
        test = "Mann-Whitney U", statistic = unname(test$statistic),
        df = NA_real_, p = test$p.value, effect = effect_value,
        ci_low = effect_ci$CI_low, ci_high = effect_ci$CI_high
    )

    .new_edu_analysis(
        analysis = "mann_whitney", label = "Mann-Whitney U Test",
        question = question, requirements = requirements, main = statistics,
        descriptives = descriptives, effects = effect, diagnostics = diagnostics,
        interpretation = plain, caution = "", plot_data = data.frame(outcome = d$y, group = d$g),
        report_blocks = list(
            rationale = sprintf("%s This analysis is appropriate when %s", question, requirements),
            descriptives = plain, apa = apa, assumptions = .jr_diagnostic_text(diagnostics), plain = plain
        ),
        statistics = statistics, call = match.call()
    )
}

#' Guided Wilcoxon signed-rank test
#'
#' @param data A data frame.
#' @param outcome First numeric or ordinal measurement variable name.
#' @param paired_outcome Second matched measurement variable name.
#' @param ci Confidence level for the rank-biserial effect size.
#' @return An `edu_analysis` object.
#' @export
edu_wilcoxon_signed_rank <- function(data, outcome, paired_outcome, ci = 0.95) {
    .jr_assert_columns(data, c(outcome, paired_outcome))
    d <- .jr_complete(data, c(outcome, paired_outcome))
    .jr_numeric(d[[outcome]], outcome)
    .jr_numeric(d[[paired_outcome]], paired_outcome)
    first <- d[[outcome]]
    second <- d[[paired_outcome]]
    test <- stats::wilcox.test(first, second, paired = TRUE, exact = FALSE, correct = TRUE)
    effect <- suppressMessages(effectsize::rank_biserial(first, second, paired = TRUE, ci = ci))
    descriptives <- data.frame(
        condition = c(outcome, paired_outcome),
        n = c(length(first), length(second)),
        median = c(stats::median(first), stats::median(second)),
        iqr = c(stats::IQR(first), stats::IQR(second)),
        stringsAsFactors = FALSE
    )
    diagnostics <- data.frame(
        check = c(
            "Paired observations",
            "Symmetry of paired differences"
        ),
        statistic = c(NA_real_, NA_real_),
        p = c(NA_real_, NA_real_),
        status = c("Not assessed", "Not assessed"),
        interpretation = c(
            "The test requires two observations from the same participant or a valid matched pair.",
            "Interpreting the signed-rank test as a typical change assumes that paired differences are approximately symmetric."
        ),
        action = c(
            "Confirm pairing from the study design and data structure.",
            "Inspect the distribution of paired differences and use cautious wording if it is strongly asymmetric."
        ),
        stringsAsFactors = FALSE
    )
    effect_value <- effect$r_rank_biserial[1]
    effect_ci <- effect[1, c("CI_low", "CI_high")]
    sig_phrase <- if (test$p.value < .05)
        "indicated a statistically significant change"
    else
        "did not indicate a statistically significant change"
    apa <- sprintf(
        "A Wilcoxon signed-rank test %s from %s (Mdn = %s, IQR = %s) to %s (Mdn = %s, IQR = %s), W = %s, p %s, rank-biserial r = %s, %s%% CI %s.",
        sig_phrase,
        outcome, .jr_num(descriptives$median[1]), .jr_num(descriptives$iqr[1]),
        paired_outcome, .jr_num(descriptives$median[2]), .jr_num(descriptives$iqr[2]),
        .jr_num(test$statistic), .jr_p(test$p.value),
        .jr_num(effect_value, 2L, TRUE), .jr_num(ci * 100, 0L),
        .jr_ci(effect_ci$CI_low, effect_ci$CI_high, 2L, TRUE)
    )
    question <- sprintf("Did scores tend to change between %s and %s for the same cases?", outcome, paired_outcome)
    requirements <- "Two ordinal or numeric measurements from the same participants or matched cases."
    plain <- sprintf(
        "This rank-based analysis examined paired changes from %s to %s. %s",
        outcome, paired_outcome,
        if (test$p.value < .05)
            "The data provide evidence of a systematic change between measurements."
        else
            "The data do not provide clear evidence of a systematic change between measurements."
    )
    statistics <- data.frame(
        test = "Wilcoxon signed-rank", statistic = unname(test$statistic),
        df = NA_real_, p = test$p.value, effect = effect_value,
        ci_low = effect_ci$CI_low, ci_high = effect_ci$CI_high
    )

    .new_edu_analysis(
        analysis = "wilcoxon_signed_rank", label = "Wilcoxon Signed-Rank Test",
        question = question, requirements = requirements, main = statistics,
        descriptives = descriptives, effects = effect, diagnostics = diagnostics,
        interpretation = plain, caution = "", plot_data = data.frame(
            outcome = c(first, second),
            group = factor(rep(c(outcome, paired_outcome), each = nrow(d)),
                           levels = c(outcome, paired_outcome))
        ),
        report_blocks = list(
            rationale = sprintf("%s This analysis is appropriate when %s", question, requirements),
            descriptives = plain, apa = apa, assumptions = .jr_diagnostic_text(diagnostics), plain = plain
        ),
        statistics = statistics, call = match.call()
    )
}
