.jr_ttest_guidance_sections <- function(result) {
    stats <- result$statistics[1, , drop = FALSE]
    desc <- result$descriptives
    independent <- is.data.frame(desc) && "group" %in% names(desc)
    effect <- .jr_primary_effect(result)
    if (independent && nrow(desc) >= 2L) {
        outcome <- .jr_call_value_text(result, "outcome", "the outcome")
        group <- .jr_call_value_text(result, "group", "the grouping variable")
        difference <- desc$mean[1] - desc$mean[2]
        higher <- if (difference > 0) desc$group[1] else if (difference < 0) desc$group[2] else "neither group"
        lower <- if (difference > 0) desc$group[2] else if (difference < 0) desc$group[1] else "neither group"
        method <- as.character(stats$test[1])
        levene <- .jr_normalize_diagnostics(result$diagnostics)
        levene <- levene[grepl("Levene", levene$check, fixed = TRUE), , drop = FALSE]
        variance <- if (nrow(levene) && is.finite(levene$p[1])) {
            sprintf(
                "Levene's test gave F = %s, p %s. It %s clear evidence that the group variances differed. The reported result is %s.",
                .jr_num(levene$statistic[1]), .jr_p(levene$p[1]),
                if (levene$p[1] < .05) "provided" else "did not provide",
                if (grepl("Welch", method, fixed = TRUE)) "Welch's t-test" else "Student's t-test"
            )
        } else {
            paste(
                "No usable Levene result was available.",
                "Welch's t-test does not require equal population variances; Student's test should be justified using the variance evidence and design."
            )
        }
        ci_includes <- .jr_interval_includes(stats$ci_low[1], stats$ci_high[1], 0)
        main <- sprintf(
            paste(
                "%s produced t(%s) = %s, p %s.",
                "The data %s sufficient evidence that the population means differed.",
                "The raw mean difference (%s minus %s) was %s."
            ),
            method, .jr_num(stats$df[1], if (grepl("Student", method, fixed = TRUE)) 0L else 2L),
            .jr_num(stats$statistic[1]), .jr_p(stats$p[1]),
            if (stats$p[1] < .05) "provided" else "did not provide",
            desc$group[1], desc$group[2], .jr_num(difference)
        )
        uncertainty <- sprintf(
            paste(
                "The confidence interval for the mean difference ranged from %s to %s and %s zero.",
                "%s"
            ),
            .jr_num(stats$ci_low[1]), .jr_num(stats$ci_high[1]),
            if (ci_includes) "included" else "excluded",
            if (ci_includes) {
                "It remains compatible with no mean difference; inspect both limits to see which directions and magnitudes remain plausible."
            } else {
                sprintf(
                    "Every value in the interval favoured %s over %s, although the interval still shows uncertainty about magnitude.",
                    if (stats$ci_low[1] > 0) desc$group[1] else desc$group[2],
                    if (stats$ci_low[1] > 0) desc$group[2] else desc$group[1]
                )
            }
        )
        missing <- if (is.finite(result$n_total %||% NA_real_) &&
                is.finite(result$n_used %||% NA_real_)) {
            sprintf(
                "%s of %s rows were complete for this analysis; %s rows were excluded because of missing outcome or group information.",
                result$n_used, result$n_total, result$n_total - result$n_used
            )
        } else {
            "The displayed group sample sizes are complete-case counts. Compare their sum with the source data and review the missing-data setting."
        }
        return(list(
            "What this analysis examines" = sprintf(
                "An independent-samples t-test examines whether mean %s differs between the two levels of %s. The observations must be independent between groups.",
                outcome, group
            ),
            "Check the variables and data" = .jr_guidance_block(
                text = missing,
                bullets = c(
                    sprintf("Confirm that %s is the intended numeric dependent variable.", outcome),
                    sprintf("Confirm that %s has exactly the intended two groups and that %s is the first group.", group, desc$group[1]),
                    "Review unusual observations and each group's distribution; do not remove a case solely because it is unusual.",
                    "Confirm from the study design that one participant does not contribute to both groups."
                )
            ),
            "Descriptive information" = sprintf(
                "%s had n = %s, M = %s and SD = %s; %s had n = %s, M = %s and SD = %s.",
                desc$group[1], .jr_num(desc$n[1], 0L), .jr_num(desc$mean[1]), .jr_num(desc$sd[1]),
                desc$group[2], .jr_num(desc$n[2], 0L), .jr_num(desc$mean[2]), .jr_num(desc$sd[2])
            ),
            "Assumptions and diagnostics" = paste(variance, .jr_diagnostic_guidance(result), sep = "\n\n"),
            "Main statistical findings" = main,
            "Direction of the finding" = sprintf(
                "%s had the higher observed mean and %s had the lower observed mean. The signed difference was calculated as %s minus %s, so its %s sign indicates that direction.",
                higher, lower, desc$group[1], desc$group[2],
                if (difference > 0) "positive" else if (difference < 0) "negative" else "zero"
            ),
            "Effect size and practical magnitude" = .jr_effect_size_guidance(result),
            "Confidence intervals and uncertainty" = paste(
                uncertainty,
                if (all(is.finite(c(effect$lower, effect$upper))))
                    .jr_effect_interval_guidance(effect$lower, effect$upper, .jr_effect_spec("ttest"))
                else "No effect-size confidence interval was available; avoid treating the point estimate as exact."
            ),
            "Follow-up analyses" = paste(
                "No automatic post-hoc comparison is needed because the grouping variable has only two levels.",
                "If this t-test is one of several related outcomes or hypotheses, define the family of tests and consider multiplicity control.",
                "If the research question concerns equivalence rather than difference, use a pre-specified equivalence test."
            ),
            "Overall interpretation" = .jr_overall_interpretation(result),
            "Check before using this result" = .jr_guidance_block(bullets = c(
                "Confirm the outcome, group coding, group order and complete-case sample sizes.",
                sprintf("Report the %s row rather than another available t-test row.", method),
                "Check t, degrees of freedom, p, the signed mean difference and both confidence intervals against the final output.",
                "Review independence, missing data, group distributions and influential observations.",
                "Do not equate statistical significance with practical importance or non-significance with equivalence.",
                "Use causal language only when allocation, timing and design justify it.",
                .jr_analysis_checklist("ttest")
            )),
            "Literature and guidance" = .jr_literature_guidance(result)
        ))
    }

    first <- if (nrow(desc)) as.character(desc$condition[1]) else
        .jr_call_value_text(result, "outcome", "the first condition")
    second <- if (nrow(desc) > 1L) as.character(desc$condition[2]) else
        .jr_call_value_text(result, "paired_outcome", "the second condition")
    difference <- if (nrow(desc) > 1L) desc$mean[1] - desc$mean[2] else NA_real_
    list(
        "What this analysis examines" = sprintf(
            "A paired-samples t-test examines the mean within-case difference between %s and %s. Pairing matters because each difference comes from the same participant or a justified matched pair.",
            first, second
        ),
        "Check the variables and data" = .jr_guidance_block(bullets = c(
            "Confirm that rows contain correctly matched observations.",
            "Confirm the subtraction order: first measurement minus second measurement.",
            "Review incomplete pairs and the distribution of difference scores.",
            "Check unusual difference scores in context; exclusion requires a substantive justification."
        )),
        "Descriptive information" = .jr_descriptive_guidance(result),
        "Assumptions and diagnostics" = .jr_diagnostic_guidance(result),
        "Main statistical findings" = sprintf(
            "The paired result was t(%s) = %s, p %s. The mean paired difference was %s.",
            .jr_num(stats$df[1], 0L), .jr_num(stats$statistic[1]),
            .jr_p(stats$p[1]), .jr_num(difference)
        ),
        "Direction of the finding" = sprintf(
            "Differences were defined as %s minus %s. The observed mean difference was %s, indicating that scores were %s on the first measurement.",
            first, second, .jr_num(difference),
            if (difference > 0) "higher" else if (difference < 0) "lower" else "the same"
        ),
        "Effect size and practical magnitude" = .jr_effect_size_guidance(result),
        "Confidence intervals and uncertainty" = sprintf(
            "The confidence interval for the paired mean difference was %s. It %s zero.",
            .jr_ci(stats$ci_low[1], stats$ci_high[1]),
            if (.jr_interval_includes(stats$ci_low[1], stats$ci_high[1], 0)) "included" else "excluded"
        ),
        "Overall interpretation" = .jr_overall_interpretation(result),
        "Check before using this result" = .jr_guidance_block(
            bullets = .jr_analysis_checklist("ttest")
        ),
        "Literature and guidance" = .jr_literature_guidance(result)
    )
}

.jr_anova_guidance_sections <- function(result) {
    stats <- result$statistics
    checklist <- if (identical(result$analysis, "anova_between"))
        .jr_anova_between_checklist_items()
    else
        .jr_analysis_checklist(result$analysis)
    order <- if (is.data.frame(stats) && "term" %in% names(stats))
        order(grepl(":", stats$term, fixed = TRUE), decreasing = TRUE) else seq_len(nrow(stats))
    findings <- if (is.data.frame(stats) && nrow(stats)) {
        vapply(order, function(i) {
            term <- if ("term" %in% names(stats)) stats$term[i] else stats$test[i]
            sprintf(
                "%s: F(%s, %s) = %s, p %s. This effect %s statistically significant.",
                term,
                .jr_num(stats$df1[i]), .jr_num(stats$df2[i]),
                .jr_num(stats$statistic[i]), .jr_p(stats$p[i]),
                if (stats$p[i] < .05) "was" else "was not"
            )
        }, character(1))
    } else "No finite omnibus result was available."
    effect_rows <- if (is.data.frame(stats) && "effect" %in% names(stats)) {
        vapply(order, function(i) {
            term <- if ("term" %in% names(stats)) stats$term[i] else stats$test[i]
            spec <- .jr_effect_spec(result$analysis)
            if (is.null(spec) || !is.finite(stats$effect[i]))
                return("")
            paste0(
                term, ": ", spec$symbol, " = ", .jr_num(stats$effect[i], 2L, TRUE),
                ", ", .jr_effect_position(stats$effect[i], spec$thresholds), "."
            )
        }, character(1))
    } else character()
    interactions <- if (is.data.frame(stats) && "term" %in% names(stats))
        stats[grepl(":", stats$term, fixed = TRUE) & stats$p < .05, , drop = FALSE]
    else data.frame()
    follow_up <- .jr_follow_up_analysis_guidance(result)
    if (!nzchar(follow_up)) {
        follow_up <- if (any(stats$p < .05, na.rm = TRUE))
            "Review any planned contrasts or selected post-hoc comparisons. A significant omnibus effect does not show that every group differs."
        else
            "No significance-driven post-hoc testing is indicated. Pre-planned contrasts may still be reported if they were specified independently of the omnibus result."
    }
    list(
        "What this analysis examines" = paste(
            result$question, result$requirements
        ),
        "Check the variables and data" = .jr_guidance_block(
            bullets = .jr_guidance_data_checks(result)
        ),
        "Descriptive information" = .jr_descriptive_guidance(result),
        "Assumptions and diagnostics" = .jr_diagnostic_guidance(result),
        "Main statistical findings" = findings,
        "Direction of the finding" = if (nrow(interactions)) {
            paste(
                "At least one interaction was significant and must be interpreted before its component main effects.",
                "Use estimated marginal means, an interaction plot and justified simple effects to establish the direction."
            )
        } else {
            "Use the displayed group or cell means to describe direction. An F test is unsigned and does not itself identify which mean is higher."
        },
        "Effect size and practical magnitude" = c(
            .jr_effect_size_guidance(result),
            effect_rows[nzchar(effect_rows)]
        ),
        "Confidence intervals and uncertainty" = paste(
            "Effect-size confidence intervals show the range of population magnitudes compatible with the model.",
            "Wide intervals or intervals crossing conventional reference regions warrant cautious magnitude claims."
        ),
        "Follow-up analyses" = follow_up,
        "Overall interpretation" = .jr_overall_interpretation(result),
        "Check before using this result" = .jr_guidance_block(
            bullets = checklist
        ),
        "Literature and guidance" = .jr_literature_guidance(result)
    )
}

.jr_correlation_guidance_sections <- function(result) {
    stats <- result$statistics[1, , drop = FALSE]
    method <- tools::toTitleCase(as.character(stats$test[1]))
    coefficient <- stats$statistic[1]
    variables <- c(
        .jr_call_value_text(result, "x", "the first variable"),
        .jr_call_value_text(result, "y", "the second variable")
    )
    list(
        "What this analysis examines" = sprintf(
            "A %s correlation summarises the %s association between %s and %s.",
            method,
            if (tolower(method) == "pearson") "linear" else "monotonic rank-based",
            variables[1], variables[2]
        ),
        "Check the variables and data" = .jr_guidance_block(
            bullets = .jr_guidance_data_checks(result)
        ),
        "Assumptions and diagnostics" = .jr_diagnostic_guidance(result),
        "Main statistical findings" = sprintf(
            "%s = %s, p %s, n = %s. The result was %s statistically significant.",
            switch(tolower(method), pearson = "r", spearman = "\u03c1", kendall = "\u03c4", "coefficient"),
            .jr_num(coefficient, 2L, TRUE), .jr_p(stats$p[1]),
            if ("n" %in% names(stats)) .jr_num(stats$n[1], 0L) else "the complete-pair sample",
            if (stats$p[1] < .05) "" else "not"
        ),
        "Direction of the finding" = sprintf(
            "The coefficient was %s: higher %s values tended to accompany %s %s values.",
            if (coefficient > 0) "positive" else if (coefficient < 0) "negative" else "zero",
            variables[1], if (coefficient > 0) "higher" else "lower", variables[2]
        ),
        "Effect size and practical magnitude" = .jr_effect_size_guidance(result),
        "Confidence intervals and uncertainty" = if (all(is.finite(c(stats$ci_low[1], stats$ci_high[1])))) {
            sprintf(
                "The confidence interval was %s and %s zero. Interpret its width as uncertainty in the population association.",
                .jr_ci(stats$ci_low[1], stats$ci_high[1], 2L, TRUE),
                if (.jr_interval_includes(stats$ci_low[1], stats$ci_high[1], 0)) "included" else "excluded"
            )
        } else {
            "No confidence interval was available for this method. The point estimate should not be treated as exact."
        },
        "Overall interpretation" = paste(
            .jr_overall_interpretation(result),
            "Correlation does not by itself establish causation; range restriction, measurement error, outliers and unmodelled variables may affect the result."
        ),
        "Check before using this result" = .jr_guidance_block(
            bullets = .jr_analysis_checklist("correlation")
        ),
        "Literature and guidance" = .jr_literature_guidance(result)
    )
}

.jr_regression_guidance_sections <- function(result) {
    stats <- result$statistics[1, , drop = FALSE]
    labels <- .jr_formula_labels(result)
    params <- result$parameters
    coefficients <- if (is.data.frame(params) && nrow(params)) {
        predictor <- if ("Parameter" %in% names(params)) params$Parameter else params$term
        estimate <- if ("Coefficient" %in% names(params)) params$Coefficient else params$B
        p <- params$p
        keep <- predictor != "(Intercept)"
        vapply(which(keep), function(i) sprintf(
            "%s had B = %s, p %s: its adjusted association was %s and %s statistically significant.",
            predictor[i], .jr_num(estimate[i]), .jr_p(p[i]),
            if (estimate[i] > 0) "positive" else "negative",
            if (p[i] < .05) "was" else "was not"
        ), character(1))
    } else character()
    list(
        "What this analysis examines" = sprintf(
            "Linear regression estimates how %s is associated with %s while holding the other included predictors constant.",
            labels$outcome, paste(labels$predictors, collapse = ", ")
        ),
        "Check the variables and data" = .jr_guidance_block(
            bullets = .jr_guidance_data_checks(result)
        ),
        "Assumptions and diagnostics" = .jr_diagnostic_guidance(result),
        "Main statistical findings" = c(
            sprintf(
                "The overall model gave F(%s, %s) = %s, p %s, R\u00b2 = %s and adjusted R\u00b2 = %s. It %s statistically significant.",
                .jr_num(stats$df1[1], 0L), .jr_num(stats$df2[1], 0L),
                .jr_num(stats$statistic[1]), .jr_p(stats$p[1]),
                .jr_num(stats$r2[1], 2L, TRUE), .jr_num(stats$adjusted_r2[1], 2L, TRUE),
                if (stats$p[1] < .05) "was" else "was not"
            ),
            coefficients
        ),
        "Direction of the finding" = paste(
            "The sign of each B coefficient gives its adjusted direction.",
            "A coefficient is conditional on the other predictors and should not be ranked in importance solely by its p-value."
        ),
        "Effect size and practical magnitude" = sprintf(
            "R\u00b2 = %s means that the fitted predictors accounted for about %s%% of the observed variation in %s. No generic small/medium/large label is imposed.",
            .jr_num(stats$r2[1], 2L, TRUE), .jr_num(100 * stats$r2[1], 1L),
            labels$outcome
        ),
        "Confidence intervals and uncertainty" = paste(
            "Coefficient confidence intervals describe uncertainty in each adjusted association.",
            "An interval including zero remains compatible with no unique linear association after adjustment."
        ),
        "Overall interpretation" = .jr_overall_interpretation(result),
        "Check before using this result" = .jr_guidance_block(
            bullets = .jr_regression_checklist_items()
        ),
        "Literature and guidance" = .jr_literature_guidance(result)
    )
}

.jr_logistic_guidance_sections <- function(result) {
    stats <- result$statistics[1, , drop = FALSE]
    params <- result$parameters
    multinomial <- identical(result$analysis, "multinomial_logistic")
    event <- result$event %||% if (multinomial) "each comparison category" else "the modelled event"
    reference <- result$reference %||% "the reference outcome"
    rows <- if (is.data.frame(params) && nrow(params)) {
        term <- if ("Parameter" %in% names(params)) params$Parameter else params$term
        comparison <- if (multinomial && "category" %in% names(params))
            paste0(params$category, ", ", term)
        else
            term
        ratio <- if ("OR" %in% names(params)) params$OR else params$RRR
        lower <- params$CI_low
        upper <- params$CI_high
        p <- params$p
        keep <- term != "(Intercept)"
        vapply(which(keep), function(i) sprintf(
            "%s: ratio = %s, %s%% CI %s, p %s. The ratio was %s 1 and its interval %s 1.",
            comparison[i], .jr_or(ratio[i]), 95,
            .jr_or_ci(lower[i], upper[i]), .jr_p(p[i]),
            if (ratio[i] > 1) "above" else "below",
            if (.jr_interval_includes(lower[i], upper[i], 1)) "included" else "excluded"
        ), character(1))
    } else character()
    list(
        "What this analysis examines" = sprintf(
            "%s models %s relative to %s using the specified predictors.",
            if (multinomial) "Multinomial logistic regression" else "Binomial logistic regression",
            event, reference
        ),
        "Check the variables and data" = .jr_guidance_block(
            bullets = .jr_guidance_data_checks(result)
        ),
        "Assumptions and diagnostics" = .jr_diagnostic_guidance(result),
        "Main statistical findings" = c(
            sprintf(
                "The overall likelihood-ratio test gave \u03c7\u00b2(%s) = %s, p %s. The model %s statistically significant.",
                .jr_num(stats$df[1], 0L), .jr_num(stats$statistic[1]),
                .jr_p(stats$p[1]), if (stats$p[1] < .05) "was" else "was not"
            ),
            rows
        ),
        "Direction of the finding" = paste(
            "A ratio above 1 indicates higher odds or relative risk of the comparison outcome; a ratio below 1 indicates lower odds or relative risk.",
            "The interpretation depends on outcome coding, predictor coding, the reference category and the unit of change."
        ),
        "Effect size and practical magnitude" = paste(
            "Odds and relative-risk ratios are not classified with generic small, medium or large thresholds.",
            "Interpret them using their coding, unit, confidence interval and substantive consequences. Pseudo-R\u00b2 is not the proportion of outcome variance explained."
        ),
        "Confidence intervals and uncertainty" = paste(
            "A ratio interval including 1 remains compatible with no adjusted association.",
            "Wide intervals indicate imprecision, which is common with sparse categories or separation."
        ),
        "Overall interpretation" = .jr_overall_interpretation(result),
        "Check before using this result" = .jr_guidance_block(
            bullets = .jr_analysis_checklist(result$analysis)
        ),
        "Literature and guidance" = .jr_literature_guidance(result)
    )
}

.jr_chisq_guidance_sections <- function(result) {
    stats <- result$statistics[1, , drop = FALSE]
    cells <- result$cells
    residuals <- if (is.data.frame(cells) && "standardised_residual" %in% names(cells)) {
        ordered <- order(abs(cells$standardised_residual), decreasing = TRUE)
        utils::head(vapply(ordered, function(i) sprintf(
            "%s: observed %s, expected %s, standardised residual %s",
            cells$category[i], .jr_num(cells$observed[i], 0L),
            .jr_num(cells$expected[i]), .jr_num(cells$standardised_residual[i])
        ), character(1)), 5L)
    } else character()
    effect <- if ("effect" %in% names(stats)) stats$effect[1] else NA_real_
    table_note <- if (identical(result$analysis, "chisq_independence") &&
            !is.null(result$observed)) {
        dims <- dim(result$observed)
        sprintf(
            "Cramer's V was %s for a %s \u00d7 %s table. Its magnitude depends on table dimensions, so no universal 0.10/0.30/0.50 label is applied (Cohen, 1988; Agresti, 2019).",
            .jr_num(effect, 2L, TRUE), dims[1], dims[2]
        )
    } else if (is.finite(effect)) {
        sprintf(
            "Cohen's w was %s. Interpret it with the expected distribution and category residuals rather than as practical importance by itself.",
            .jr_num(effect, 2L, TRUE)
        )
    } else ""
    list(
        "What this analysis examines" = paste(result$question, result$requirements),
        "Check the variables and data" = .jr_guidance_block(
            bullets = .jr_guidance_data_checks(result)
        ),
        "Descriptive information" = residuals,
        "Assumptions and diagnostics" = .jr_diagnostic_guidance(result),
        "Main statistical findings" = sprintf(
            "\u03c7\u00b2(%s) = %s, p %s. The omnibus result %s statistically significant.",
            .jr_num(stats$df[1], 0L), .jr_num(stats$statistic[1]),
            .jr_p(stats$p[1]), if (stats$p[1] < .05) "was" else "was not"
        ),
        "Direction of the finding" = paste(
            "The chi-square statistic is unsigned.",
            "Direction and the cells contributing most strongly must be identified from observed versus expected counts and standardised residuals."
        ),
        "Effect size and practical magnitude" = table_note,
        "Follow-up analyses" = .jr_follow_up_analysis_guidance(result),
        "Overall interpretation" = .jr_overall_interpretation(result),
        "Check before using this result" = .jr_guidance_block(
            bullets = .jr_analysis_checklist(result$analysis)
        ),
        "Literature and guidance" = .jr_literature_guidance(result)
    )
}

.jr_reliability_guidance_sections <- function(result) {
    stats <- result$statistics
    omega <- stats$estimate[stats$coefficient == "McDonald's omega total"][1]
    alpha <- stats$estimate[stats$coefficient == "Cronbach's alpha"][1]
    list(
        "What this analysis examines" = paste(result$question, result$requirements),
        "Check the variables and data" = .jr_guidance_block(
            bullets = .jr_guidance_data_checks(result)
        ),
        "Descriptive information" = sprintf(
            "The analysis used %s items and %s complete responses. Omega was %s and alpha was %s.",
            stats$items[1], stats$n[1], .jr_num(omega, 2L, TRUE), .jr_num(alpha, 2L, TRUE)
        ),
        "Assumptions and diagnostics" = .jr_diagnostic_guidance(result),
        "Main statistical findings" = paste(
            "McDonald's omega estimates internal consistency using common-factor loadings; Cronbach's alpha is reported for comparison.",
            "Neither coefficient proves that the scale is unidimensional or valid."
        ),
        "Effect size and practical magnitude" = paste(
            "Reliability coefficients are not effect sizes and are not classified with Cohen's small, medium and large conventions.",
            "Interpret them with the number and content of items, dimensionality, intended use and uncertainty."
        ),
        "Confidence intervals and uncertainty" = if (all(is.finite(c(stats$ci_low[1], stats$ci_high[1])))) {
            sprintf(
                "The bootstrap confidence interval for omega was %s. It communicates sampling precision but not dimensionality.",
                .jr_ci(stats$ci_low[1], stats$ci_high[1], 2L, TRUE)
            )
        } else "No stable omega interval was available; avoid treating the point estimate as exact.",
        "Overall interpretation" = .jr_overall_interpretation(result),
        "Check before using this result" = .jr_guidance_block(
            bullets = .jr_analysis_checklist("reliability_omega")
        ),
        "Literature and guidance" = .jr_literature_guidance(result)
    )
}

.jr_nonparametric_guidance_sections <- function(result) {
    stats <- result$statistics[1, , drop = FALSE]
    list(
        "What this analysis examines" = paste(result$question, result$requirements),
        "Check the variables and data" = .jr_guidance_block(
            bullets = .jr_guidance_data_checks(result)
        ),
        "Descriptive information" = .jr_descriptive_guidance(result),
        "Assumptions and diagnostics" = .jr_diagnostic_guidance(result),
        "Main statistical findings" = sprintf(
            "%s = %s, p %s. The rank-based result %s statistically significant.",
            stats$test[1], .jr_num(stats$statistic[1]), .jr_p(stats$p[1]),
            if (stats$p[1] < .05) "was" else "was not"
        ),
        "Direction of the finding" = paste(
            result$interpretation,
            "Use the observed distributions or paired differences to establish direction; do not automatically describe the result as a median difference."
        ),
        "Effect size and practical magnitude" = paste(
            "The rank-biserial correlation describes directional rank separation.",
            "No generic magnitude label is imposed here because interpretation depends on the estimand, ties and distribution shapes."
        ),
        "Confidence intervals and uncertainty" = if (all(is.finite(c(stats$ci_low[1], stats$ci_high[1])))) {
            sprintf(
                "The rank-biserial interval was %s and %s zero.",
                .jr_ci(stats$ci_low[1], stats$ci_high[1], 2L, TRUE),
                if (.jr_interval_includes(stats$ci_low[1], stats$ci_high[1], 0)) "included" else "excluded"
            )
        } else "",
        "Overall interpretation" = .jr_overall_interpretation(result),
        "Check before using this result" = .jr_guidance_block(
            bullets = .jr_analysis_checklist(result$analysis)
        ),
        "Literature and guidance" = .jr_literature_guidance(result)
    )
}

.jr_default_guidance_sections <- function(result) {
    list(
        "What this analysis examines" = paste(result$question, result$requirements),
        "Check the variables and data" = .jr_guidance_block(
            bullets = .jr_guidance_data_checks(result)
        ),
        "Descriptive information" = .jr_descriptive_guidance(result),
        "Assumptions and diagnostics" = .jr_diagnostic_guidance(result),
        "Main statistical findings" = result$report_blocks$plain %||% result$interpretation,
        "Overall interpretation" = .jr_overall_interpretation(result),
        "Check before using this result" = .jr_guidance_block(
            bullets = .jr_analysis_checklist(result$analysis)
        ),
        "Literature and guidance" = .jr_literature_guidance(result)
    )
}

.jr_interpretation_sections <- function(result) {
    switch(
        result$analysis %||% "",
        ttest = .jr_ttest_guidance_sections(result),
        anova_oneway = .jr_anova_guidance_sections(result),
        anova_between = .jr_anova_guidance_sections(result),
        ancova = .jr_anova_guidance_sections(result),
        anova_rm = .jr_anova_guidance_sections(result),
        anova_mixed = .jr_anova_guidance_sections(result),
        manova = .jr_anova_guidance_sections(result),
        correlation = .jr_correlation_guidance_sections(result),
        regression = .jr_regression_guidance_sections(result),
        logistic_regression = .jr_logistic_guidance_sections(result),
        multinomial_logistic = .jr_logistic_guidance_sections(result),
        chisq_independence = .jr_chisq_guidance_sections(result),
        chisq_gof = .jr_chisq_guidance_sections(result),
        reliability_omega = .jr_reliability_guidance_sections(result),
        mann_whitney = .jr_nonparametric_guidance_sections(result),
        wilcoxon_signed_rank = .jr_nonparametric_guidance_sections(result),
        .jr_default_guidance_sections(result)
    )
}

.jr_interpretation_guidance_html <- function(result, analysis_label = "") {
    .jr_interpretation_guidance_panel(
        .jr_interpretation_sections(result),
        analysis_label = analysis_label
    )
}
