.jr_enable_bayesfactor_library <- function() {
    if (requireNamespace("BayesFactor", quietly = TRUE))
        return(TRUE)
    candidates <- c("/Applications/jamovi.app/Contents/Resources/modules/jmv/R")
    for (candidate in candidates) {
        if (dir.exists(file.path(candidate, "BayesFactor")))
            .libPaths(unique(c(candidate, .libPaths())))
    }
    requireNamespace("BayesFactor", quietly = TRUE)
}

.jr_bf_interpretation <- function(bf10) {
    if (bf10 >= 10)
        return("strong evidence for a difference")
    if (bf10 >= 3)
        return("moderate evidence for a difference")
    if (bf10 > 1)
        return("limited evidence for a difference")
    if (bf10 == 1)
        return("equal evidence for the difference and no-difference models")
    if (bf10 > (1 / 3))
        return("limited evidence for no difference")
    if (bf10 > .1)
        return("moderate evidence for no difference")
    "strong evidence for no difference"
}

#' Guided Bayesian independent or paired samples t-test
#'
#' @param data A data frame.
#' @param outcome Numeric outcome variable name.
#' @param group Factor/group variable name for an independent test.
#' @param paired_outcome Second numeric outcome variable name for a paired test.
#' @param type `"independent"` or `"paired"`.
#' @param prior_width Cauchy prior scale used for the standardised effect.
#' @return An `edu_analysis` object.
#' @export
edu_bayes_t_test <- function(data, outcome, group = NULL, paired_outcome = NULL,
                             type = c("independent", "paired"), prior_width = 0.707) {
    type <- match.arg(type)
    if (!.jr_enable_bayesfactor_library()) {
        .jr_stop(
            "Bayesian reporting requires package `BayesFactor`, which is supplied with jamovi's native t-test analyses but must be installed for standalone R use."
        )
    }
    if (!is.numeric(prior_width) || length(prior_width) != 1L || !is.finite(prior_width) || prior_width <= 0)
        .jr_stop("`prior_width` must be a single positive number.")

    if (type == "independent") {
        if (is.null(group))
            .jr_stop("`group` is required for an independent Bayesian t-test.")
        .jr_assert_columns(data, c(outcome, group))
        d <- .jr_complete(data, c(outcome, group))
        .jr_numeric(d[[outcome]], outcome)
        d$g <- droplevels(factor(d[[group]]))
        if (nlevels(d$g) != 2L)
            .jr_stop("An independent Bayesian t-test requires a grouping variable with exactly two levels.")
        d$y <- d[[outcome]]
        fit <- BayesFactor::ttestBF(formula = y ~ g, data = d, rscale = prior_width)
        groups <- split(d$y, d$g)
        descriptives <- data.frame(
            group = names(groups),
            n = vapply(groups, length, integer(1)),
            mean = vapply(groups, mean, numeric(1)),
            sd = vapply(groups, stats::sd, numeric(1)),
            stringsAsFactors = FALSE
        )
        comparison <- sprintf(
            "between %s (M = %s, SD = %s) and %s (M = %s, SD = %s)",
            descriptives$group[1], .jr_num(descriptives$mean[1]), .jr_num(descriptives$sd[1]),
            descriptives$group[2], .jr_num(descriptives$mean[2]), .jr_num(descriptives$sd[2])
        )
        question <- sprintf("How much does the evidence favour different mean %s scores between the two %s groups?", outcome, group)
        requirements <- "A numeric outcome measured once for two independent groups and an explicitly selected effect-size prior."
        plot_data <- data.frame(outcome = d$y, group = d$g)
        label <- "Bayesian Independent Samples T-Test"
    } else {
        if (is.null(paired_outcome))
            .jr_stop("`paired_outcome` is required for a paired Bayesian t-test.")
        .jr_assert_columns(data, c(outcome, paired_outcome))
        d <- .jr_complete(data, c(outcome, paired_outcome))
        .jr_numeric(d[[outcome]], outcome)
        .jr_numeric(d[[paired_outcome]], paired_outcome)
        first <- d[[outcome]]
        second <- d[[paired_outcome]]
        fit <- BayesFactor::ttestBF(x = first, y = second, paired = TRUE, rscale = prior_width)
        descriptives <- data.frame(
            condition = c(outcome, paired_outcome),
            n = c(length(first), length(second)),
            mean = c(mean(first), mean(second)),
            sd = c(stats::sd(first), stats::sd(second)),
            stringsAsFactors = FALSE
        )
        comparison <- sprintf(
            "from %s (M = %s, SD = %s) to %s (M = %s, SD = %s)",
            outcome, .jr_num(descriptives$mean[1]), .jr_num(descriptives$sd[1]),
            paired_outcome, .jr_num(descriptives$mean[2]), .jr_num(descriptives$sd[2])
        )
        question <- sprintf("How much does the evidence favour a mean change between %s and %s for the same cases?", outcome, paired_outcome)
        requirements <- "Two numeric measurements from the same participants or matched cases and an explicitly selected effect-size prior."
        plot_data <- data.frame(
            outcome = c(first, second),
            group = factor(rep(c(outcome, paired_outcome), each = nrow(d)),
                           levels = c(outcome, paired_outcome))
        )
        label <- "Bayesian Paired Samples T-Test"
    }

    bf10 <- as.numeric(BayesFactor::extractBF(fit, onlybf = TRUE)[1])
    evidence <- .jr_bf_interpretation(bf10)
    diagnostics <- data.frame(
        check = "Prior sensitivity and model specification",
        statistic = prior_width,
        p = NA_real_,
        status = "Not assessed",
        interpretation = sprintf(
            "The Bayes factor is conditional on a Cauchy prior width of %s for the standardised effect.",
            .jr_num(prior_width, 3L)
        ),
        action = "Check that this prior and the selected direction/model match the intended analysis; consider sensitivity checks where reporting decisions depend on the Bayes factor.",
        stringsAsFactors = FALSE
    )
    apa <- sprintf(
        "A Bayesian %s-samples t-test %s produced BF10 = %s (Cauchy prior width = %s), indicating %s.",
        if (type == "independent") "independent" else "paired",
        comparison, .jr_num(bf10, 2L), .jr_num(prior_width, 3L), evidence
    )
    plain <- sprintf(
        "The Bayes factor compares a difference model with a no-difference model. Here, BF10 = %s indicates %s; this conclusion depends on the stated prior.",
        .jr_num(bf10, 2L), evidence
    )
    statistics <- data.frame(
        test = "Bayes factor BF10", statistic = bf10, df = NA_real_,
        p = NA_real_, effect = bf10, ci_low = NA_real_, ci_high = NA_real_
    )

    .new_edu_analysis(
        analysis = "bayes_ttest", label = label, question = question,
        requirements = requirements, main = statistics, descriptives = descriptives,
        effects = statistics, diagnostics = diagnostics, interpretation = plain,
        caution = "", plot_data = plot_data,
        report_blocks = list(
            rationale = sprintf("%s This analysis is appropriate when %s", question, requirements),
            descriptives = plain, apa = apa, assumptions = .jr_diagnostic_text(diagnostics), plain = plain
        ),
        statistics = statistics, call = match.call()
    )
}
