#' Guided independent or paired samples t-test
#'
#' @param data A data frame.
#' @param outcome Numeric outcome variable name.
#' @param group Factor/group variable name for an independent test.
#' @param paired_outcome Second numeric outcome variable name for a paired test.
#' @param type `"independent"` or `"paired"`.
#' @param var_equal Whether to use Student's equal-variance independent test.
#' @param ci Confidence level.
#' @return An `edu_analysis` object.
#' @export
edu_t_test <- function(data, outcome, group = NULL, paired_outcome = NULL,
                       type = c("independent", "paired"), var_equal = FALSE,
                       ci = 0.95) {
    type <- match.arg(type)
    if (type == "independent") {
        if (is.null(group))
            .jr_stop("`group` is required for an independent t-test.")
        .jr_assert_columns(data, c(outcome, group))
        d <- .jr_complete(data, c(outcome, group))
        .jr_numeric(d[[outcome]], outcome)
        d$y <- d[[outcome]]
        d$g <- droplevels(factor(d[[group]]))
        if (nlevels(d$g) != 2L)
            .jr_stop("An independent t-test requires a grouping variable with exactly two levels.")

        test <- stats::t.test(y ~ g, data = d, var.equal = var_equal, conf.level = ci)
        effect <- suppressMessages(effectsize::cohens_d(d$y, d$g, pooled_sd = var_equal, ci = ci))
        split_y <- split(d$y, d$g)
        descriptives <- data.frame(
            group = names(split_y),
            n = vapply(split_y, length, integer(1)),
            mean = vapply(split_y, mean, numeric(1)),
            sd = vapply(split_y, stats::sd, numeric(1)),
            stringsAsFactors = FALSE
        )
        diagnostics <- do.call(rbind, Map(
            function(values, label) .jr_shapiro(values, paste("Normality in", label)),
            split_y, names(split_y)
        ))
        diagnostics <- rbind(diagnostics, .jr_levene("y", "g", d))
        method <- if (var_equal) "Student's independent-samples t-test" else "Welch independent-samples t-test"
        difference <- unname(diff(rev(test$estimate)))
        effect_value <- effect$Cohens_d[1]
        effect_ci <- effect[1, c("CI_low", "CI_high")]
        significance <- if (test$p.value < .05) "indicated a difference" else "did not indicate a difference"
        apa <- sprintf(
            "A %s %s between %s (M = %s, SD = %s) and %s (M = %s, SD = %s), t(%s) = %s, p %s, mean difference = %s, %s%% CI %s, Cohen's d = %s, %s%% CI %s.",
            method, significance, descriptives$group[1], .jr_num(descriptives$mean[1]),
            .jr_num(descriptives$sd[1]), descriptives$group[2], .jr_num(descriptives$mean[2]),
            .jr_num(descriptives$sd[2]), .jr_num(test$parameter, 2L),
            .jr_num(test$statistic, 2L), .jr_p(test$p.value),
            .jr_num(difference), .jr_num(ci * 100, 0L), .jr_ci(test$conf.int[1], test$conf.int[2]),
            .jr_num(effect_value, 2L, TRUE), .jr_num(ci * 100, 0L),
            .jr_ci(effect_ci$CI_low, effect_ci$CI_high, 2L, TRUE)
        )
        question <- sprintf("Do the mean %s scores differ between the two %s groups?", outcome, group)
        requirements <- "A numeric outcome measured once for two independent groups."
        plain <- sprintf(
            "The analysis compared average %s scores for %s and %s. %s; the estimated mean difference was %s points.",
            outcome, descriptives$group[1], descriptives$group[2],
            if (test$p.value < .05) "The sample provides evidence that the population means differ" else "The sample does not provide clear evidence that the population means differ",
            .jr_num(abs(difference))
        )
        stats <- data.frame(
            test = if (var_equal) "Student's t" else "Welch's t",
            statistic = unname(test$statistic), df = unname(test$parameter),
            p = test$p.value, effect = effect_value,
            ci_low = test$conf.int[1], ci_high = test$conf.int[2]
        )
        plot_data <- data.frame(outcome = d$y, group = d$g)
    } else {
        if (is.null(paired_outcome))
            .jr_stop("`paired_outcome` is required for a paired t-test.")
        .jr_assert_columns(data, c(outcome, paired_outcome))
        d <- .jr_complete(data, c(outcome, paired_outcome))
        .jr_numeric(d[[outcome]], outcome)
        .jr_numeric(d[[paired_outcome]], paired_outcome)
        first <- d[[outcome]]
        second <- d[[paired_outcome]]
        differences <- first - second
        test <- stats::t.test(first, second, paired = TRUE, conf.level = ci)
        effect <- suppressMessages(effectsize::cohens_d(first, second, paired = TRUE, ci = ci))
        descriptives <- data.frame(
            condition = c(outcome, paired_outcome),
            n = c(length(first), length(second)),
            mean = c(mean(first), mean(second)),
            sd = c(stats::sd(first), stats::sd(second)),
            stringsAsFactors = FALSE
        )
        diagnostics <- .jr_shapiro(differences, "Normality of paired differences")
        effect_value <- effect$Cohens_d[1]
        effect_ci <- effect[1, c("CI_low", "CI_high")]
        apa <- sprintf(
            "A paired-samples t-test %s from %s (M = %s, SD = %s) to %s (M = %s, SD = %s), t(%s) = %s, p %s, mean change = %s, %s%% CI %s, Cohen's d = %s, %s%% CI %s.",
            if (test$p.value < .05) "indicated a change" else "did not indicate a clear change",
            outcome, .jr_num(descriptives$mean[1]), .jr_num(descriptives$sd[1]),
            paired_outcome, .jr_num(descriptives$mean[2]), .jr_num(descriptives$sd[2]),
            .jr_num(test$parameter, 0L), .jr_num(test$statistic),
            .jr_p(test$p.value), .jr_num(mean(differences)),
            .jr_num(ci * 100, 0L), .jr_ci(test$conf.int[1], test$conf.int[2]),
            .jr_num(effect_value, 2L, TRUE), .jr_num(ci * 100, 0L),
            .jr_ci(effect_ci$CI_low, effect_ci$CI_high, 2L, TRUE)
        )
        question <- sprintf("Did average scores change between %s and %s for the same cases?", outcome, paired_outcome)
        requirements <- "Two numeric measurements from the same participants or matched cases."
        plain <- sprintf(
            "This analysis examined within-person change. Scores %s by an average of %s points from %s to %s.",
            if (mean(differences) > 0) "decreased" else "increased",
            .jr_num(abs(mean(differences))), outcome, paired_outcome
        )
        stats <- data.frame(
            test = "Paired t", statistic = unname(test$statistic),
            df = unname(test$parameter), p = test$p.value,
            effect = effect_value, ci_low = test$conf.int[1], ci_high = test$conf.int[2]
        )
        plot_data <- data.frame(
            outcome = c(first, second),
            group = factor(rep(c(outcome, paired_outcome), each = nrow(d)),
                           levels = c(outcome, paired_outcome))
        )
    }

    assumption_text <- .jr_diagnostic_text(diagnostics)
    caution <- if (any(diagnostics$status == "Caution"))
        paste("Caution:", assumption_text)
    else ""
    .new_edu_analysis(
        analysis = "ttest", label = if (type == "independent") "Independent Samples T-Test" else "Paired Samples T-Test",
        question = question, requirements = requirements, main = stats,
        descriptives = descriptives, effects = effect, diagnostics = diagnostics,
        interpretation = plain, caution = caution, plot_data = plot_data,
        report_blocks = list(
            rationale = sprintf("%s This analysis is appropriate when %s", question, requirements),
            descriptives = plain,
            apa = apa,
            assumptions = assumption_text,
            plain = plain
        ),
        statistics = stats, call = match.call()
    )
}
