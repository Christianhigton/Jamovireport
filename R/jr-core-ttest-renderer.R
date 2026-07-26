# Renderer for the standardised core t-test adapter object. Statistical
# extraction remains in jr-core-ttest-adapter.R.

.jr_core_ttest_config <- function(config, name, default = NULL) {
    .jr_core_ttest_option(config, name, default)
}

.jr_core_ttest_label <- function(result) {
    test <- if (identical(result$testType, "student")) "Student's t-test" else "Welch's t-test"
    paste(result$outcome, test, sep = " \u2014 ")
}

.jr_core_ttest_adjusted_p <- function(analyses, method) {
    adjusted <- rep(NA_real_, length(analyses))
    if (!method %in% c("holm", "bonferroni") || length(analyses) < 2L)
        return(adjusted)
    types <- vapply(analyses, `[[`, character(1), "testType")
    outcomes <- vapply(analyses, `[[`, character(1), "outcome")
    p <- vapply(analyses, function(x) x$statistics$p, numeric(1))
    for (type in unique(types)) {
        index <- which(types == type & is.finite(p))
        if (length(unique(outcomes[index])) < 2L)
            next
        adjusted[index] <- stats::p.adjust(p[index], method = method)
    }
    adjusted
}

.jr_core_ttest_group_details <- function(result) {
    desc <- result$descriptives
    if (!is.data.frame(desc) || nrow(desc) < 2L)
        return(NULL)
    list(
        first = as.character(desc$group[1]), second = as.character(desc$group[2]),
        n1 = as.integer(desc$n[1]), n2 = as.integer(desc$n[2]),
        mean1 = as.numeric(desc$mean[1]), mean2 = as.numeric(desc$mean[2]),
        sd1 = as.numeric(desc$sd[1]), sd2 = as.numeric(desc$sd[2])
    )
}

.jr_core_ttest_result_sentence <- function(result, adjusted_p = NA_real_) {
    details <- .jr_core_ttest_group_details(result)
    stats <- result$statistics
    interval <- result$confidenceInterval
    effect <- result$effectSize
    significant <- is.finite(stats$p) && stats$p < .05
    evidence <- if (significant)
        "indicated a statistically significant difference"
    else
        "did not indicate a statistically significant difference"
    group_text <- if (!is.null(details)) sprintf(
        "%s (n = %d, M = %s, SD = %s) and %s (n = %d, M = %s, SD = %s)",
        details$first, details$n1, .jr_num(details$mean1), .jr_num(details$sd1),
        details$second, details$n2, .jr_num(details$mean2), .jr_num(details$sd2)
    ) else "the two groups"
    statistic_text <- sprintf(
        "t(%s) = %s, p %s",
        .jr_num(stats$df, if (identical(result$testType, "student")) 0L else 2L),
        .jr_num(stats$t, 2L), .jr_p(stats$p)
    )
    difference_text <- if (all(is.finite(c(
            stats$meanDifference, interval$lower, interval$upper
        )))) sprintf(
        ", mean difference = %s, %s%% CI %s",
        .jr_num(stats$meanDifference), .jr_num(interval$level * 100, 0L),
        .jr_ci(interval$lower, interval$upper)
    ) else ""
    effect_text <- if (is.finite(effect$estimate)) sprintf(
        ". The effect size was %s, Cohen's d = %s%s",
        .jr_effect_magnitude("ttest", effect$estimate),
        .jr_num(effect$estimate, 2L, TRUE),
        if (all(is.finite(c(effect$lower, effect$upper)))) sprintf(
            ", %s%% CI %s",
            .jr_num(effect$level * 100, 0L),
            .jr_ci(effect$lower, effect$upper, 2L, TRUE)
        ) else ""
    ) else ""
    adjustment_text <- if (is.finite(adjusted_p)) sprintf(
        " The %s-adjusted p-value was %s.",
        if (adjusted_p == stats$p) "multiple-comparison" else "multiple-comparison",
        .jr_p(adjusted_p)
    ) else ""
    sprintf(
        "Results %s between %s, %s%s%s.%s",
        evidence, group_text, statistic_text, difference_text, effect_text,
        adjustment_text
    )
}

.jr_core_ttest_assumption_sentence <- function(result) {
    levene <- result$assumptions$levene
    normality <- result$assumptions$normality
    pieces <- character()
    if (isTRUE(levene$available) && all(is.finite(c(
            levene$statistic, levene$df1, levene$df2, levene$p
        )))) {
        status <- if (levene$p < .05)
            "indicated evidence that the equal-variance assumption was violated"
        else
            "did not indicate evidence that the equal-variance assumption was violated"
        action <- if (levene$p < .05 && identical(result$testType, "student"))
            "Welch's result should normally be preferred for this outcome."
        else if (identical(result$testType, "welch"))
            "Welch's test does not require equal group variances."
        else
            "This supports reporting the selected Student result."
        pieces <- c(pieces, sprintf(
            "Levene's test %s, F(%s, %s) = %s, p %s. %s",
            status, .jr_num(levene$df1, 0L), .jr_num(levene$df2, 0L),
            .jr_num(levene$statistic, 2L), .jr_p(levene$p), action
        ))
    } else {
        pieces <- c(pieces, paste(
            "A Levene result was not available in the core output.",
            "Enable the homogeneity test in the core Assumption Checks when that evidence is needed."
        ))
    }
    if (isTRUE(normality$available) && all(is.finite(c(normality$statistic, normality$p)))) {
        pieces <- c(pieces, sprintf(
            "The core Shapiro-Wilk check for %s gave W = %s, p %s. Review group distributions and outliers as well as this test, especially in small samples.",
            result$outcome, .jr_num(normality$statistic, 3L), .jr_p(normality$p)
        ))
    }
    paste(pieces, collapse = "\n\n")
}

.jr_core_ttest_suggested_wording <- function(result, style, adjusted_p = NA_real_) {
    test <- if (identical(result$testType, "student"))
        "independent-samples Student's t-test"
    else
        "Welch's independent-samples t-test"
    details <- .jr_core_ttest_group_details(result)
    groups <- if (!is.null(details))
        sprintf("%s and %s", details$first, details$second)
    else
        "the two groups"
    result_sentence <- .jr_core_ttest_result_sentence(result, adjusted_p)
    switch(
        style,
        apaDetailed = paste(
            sprintf(
                "An %s was conducted to examine whether mean %s scores differed between %s.",
                test, result$outcome, groups
            ),
            if (identical(result$testType, "student"))
                .jr_core_ttest_assumption_sentence(result) else "",
            result_sentence
        ),
        plainLanguage = {
            direction <- if (is.finite(result$statistics$meanDifference) &&
                    result$statistics$meanDifference > 0) "higher" else "lower"
            sprintf(
                "Average %s scores were %s in the first group than in the second group. %s",
                result$outcome, direction, result_sentence
            )
        },
        teaching = paste(
            sprintf(
                "This independent-samples comparison asks whether average %s scores differ between %s.",
                result$outcome, groups
            ),
            sprintf("The selected analysis uses %s.", test),
            result_sentence
        ),
        result_sentence
    )
}

.jr_core_ttest_interpretation <- function(result, tone) {
    p <- result$statistics$p
    significant <- is.finite(p) && p < .05
    switch(
        tone,
        academic = if (significant)
            "At the .05 level, the data provide evidence against the null hypothesis of equal population means. This statistical conclusion does not by itself establish practical importance or causality."
        else
            "At the .05 level, the data do not provide sufficient evidence against the null hypothesis of equal population means. This is not evidence that the population means are identical.",
        studentFriendly = if (significant)
            "The p-value is below .05, so this sample gives evidence that the two population means differ. Now use the mean difference, confidence interval, and effect size to judge how large that difference may be."
        else
            "The p-value is not below .05, so this sample does not give clear evidence of a difference in the population means. That does not prove the groups are exactly the same.",
        plainEnglish = if (significant)
            "The groups look different in this sample, and the result would be unusual if their underlying averages were the same. The size and usefulness of the difference still need context."
        else
            "This analysis did not find a clear enough difference to rule out ordinary sampling variation. The groups may still differ, but these data do not show it clearly.",
        if (significant)
            "The analysis found evidence of a difference between the group means. Interpret the estimated difference and its uncertainty alongside the p-value."
        else
            "The analysis did not find clear evidence of a difference between the group means. Avoid describing this as proof of no difference."
    )
}

.jr_core_ttest_effect_guidance <- function(result) {
    effect <- result$effectSize
    if (!is.finite(effect$estimate))
        return("A finite Cohen's d estimate was not available in the core output. Enable the core effect-size option or review the selected data.")
    paste(
        sprintf(
            "Cohen's d = %s is conventionally described as a %s standardised mean difference%s.",
            .jr_num(effect$estimate, 2L, TRUE),
            .jr_effect_magnitude("ttest", effect$estimate),
            if (all(is.finite(c(effect$lower, effect$upper)))) sprintf(
                " with a %s%% confidence interval from %s to %s",
                .jr_num(effect$level * 100, 0L),
                .jr_num(effect$lower, 2L, TRUE), .jr_num(effect$upper, 2L, TRUE)
            ) else ""
        ),
        .jr_effect_interpretation_note("ttest")
    )
}

.jr_core_ttest_practical_meaning <- function(result) {
    details <- .jr_core_ttest_group_details(result)
    difference <- result$statistics$meanDifference
    if (is.null(details) || !is.finite(difference))
        return("Judge practical importance using the outcome's measurement scale, the confidence interval, and domain-specific evidence.")
    sprintf(
        "%s averaged %s points %s than %s on %s. Whether that difference matters depends on the scale, measurement quality, study design, and consequences in this research context.",
        details$first, .jr_num(abs(difference)),
        if (difference >= 0) "higher" else "lower",
        details$second, result$outcome
    )
}

.jr_core_ttest_checklist <- function(result) {
    c(
        sprintf("Confirm that %s is the intended numeric outcome.", result$outcome),
        sprintf("Confirm that %s identifies two independent groups and that the displayed group order is correct.", result$group),
        sprintf("Report the selected %s result, not a different row from the core table.", if (result$testType == "student") "Student" else "Welch"),
        "Check sample sizes, means, standard deviations, t, degrees of freedom, p, and confidence intervals against the core output.",
        "Review missing data, unusual observations, group distributions, and study-design independence.",
        "Interpret Cohen's d and practical importance in the context of the measurement scale and literature."
    )
}

.jr_core_ttest_render <- function(adapter, config) {
    analyses <- adapter$analyses %||% list()
    style <- as.character(.jr_core_ttest_config(config, "reportStyle", "apaConcise"))[1]
    tone <- as.character(.jr_core_ttest_config(config, "explanationTone", "professional"))[1]
    adjustment <- as.character(.jr_core_ttest_config(config, "pAdjustment", "holm"))[1]
    adjusted <- .jr_core_ttest_adjusted_p(analyses, adjustment)
    report_cards <- character()
    guidance_cards <- character()
    if (!length(analyses)) {
        message <- paste(unique(c(
            adapter$warnings %||% character(),
            "Select a numeric dependent variable, a two-level grouping variable, and Student's or Welch's test in the core analysis."
        )), collapse = " ")
        report_cards <- .jr_html_card(
            "jReport add-on", "Reporting output is not yet available",
            message, accent = "#b46c21"
        )
        guidance_cards <- .jr_html_card(
            "Interpretation guidance", "Reporting output is not yet available",
            message, accent = "#b46c21"
        )
    }
    for (index in seq_along(analyses)) {
        result <- analyses[[index]]
        label <- .jr_core_ttest_label(result)
        details <- .jr_core_ttest_group_details(result)
        stats <- result$statistics
        interval <- result$confidenceInterval
        if (isTRUE(.jr_core_ttest_config(config, "showSuggestedWording", TRUE))) {
            title <- switch(
                style,
                plainLanguage = "Suggested plain-language report wording",
                teaching = "Suggested teaching report wording",
                "Suggested APA report"
            )
            report_cards <- c(report_cards, .jr_report_section_card(
                .jr_report_section_title(title, label),
                "Suggested wording only: check every value against the core jamovi output and adapt it to the study.",
                .jr_core_ttest_suggested_wording(result, style, adjusted[index]),
                accent = "#278058", background = "#f6fbf8"
            ))
        }
        sections <- list(
            "What this analysis examines" = sprintf(
                "This independent-samples t-test compares mean %s between %s and %s. The observations must be independent between groups.",
                result$outcome, details$first, details$second
            ),
            "Check the variables and data" = .jr_guidance_block(bullets = c(
                sprintf("Confirm that %s is the intended numeric outcome.", result$outcome),
                sprintf("Confirm that %s identifies exactly the intended groups and that their displayed order is correct.", result$group),
                "Review complete-case sample sizes, missing data, distributions and unusual observations.",
                "Confirm from the study design that observations are independent between groups."
            )),
            "Descriptive information" = sprintf(
                "%s had n = %s, M = %s and SD = %s; %s had n = %s, M = %s and SD = %s.",
                details$first, .jr_num(details$n1, 0L), .jr_num(details$mean1), .jr_num(details$sd1),
                details$second, .jr_num(details$n2, 0L), .jr_num(details$mean2), .jr_num(details$sd2)
            ),
            "Assumptions and diagnostics" = if (isTRUE(.jr_core_ttest_config(config, "showAssumptionGuidance", TRUE)))
                .jr_core_ttest_assumption_sentence(result) else "",
            "Main statistical findings" = if (isTRUE(.jr_core_ttest_config(config, "showInterpretation", TRUE)))
                .jr_core_ttest_interpretation(result, tone) else "",
            "Direction of the finding" = sprintf(
                "The signed mean difference is %s minus %s. Its value of %s indicates that the first group's observed mean was %s.",
                details$first, details$second, .jr_num(stats$meanDifference),
                if (stats$meanDifference > 0) "higher" else if (stats$meanDifference < 0) "lower" else "the same"
            ),
            "Effect size and practical magnitude" =
                if (isTRUE(.jr_core_ttest_config(config, "showEffectSizeGuidance", TRUE)))
                    .jr_core_ttest_effect_guidance(result) else "",
            "Confidence intervals and uncertainty" = paste(
                sprintf(
                    "The mean-difference confidence interval was %s and %s zero.",
                    .jr_ci(interval$lower, interval$upper),
                    if (.jr_interval_includes(interval$lower, interval$upper, 0)) "included" else "excluded"
                ),
                "Use its width to judge uncertainty; non-significance is not proof of equivalence."
            ),
            "Follow-up analyses" = paste(
                "No post-hoc comparison is required for a two-group test.",
                if (adjustment %in% c("holm", "bonferroni"))
                    sprintf("The displayed p-value adjustment is %s and applies only when the selected outcomes form one defensible family of tests.", adjustment)
                else "If several outcomes form one family of tests, consider a pre-specified multiplicity adjustment."
            ),
            "Overall interpretation" =
                if (isTRUE(.jr_core_ttest_config(config, "showPracticalMeaning", TRUE)))
                    .jr_core_ttest_practical_meaning(result) else "",
            "Check before using this result" =
                if (isTRUE(.jr_core_ttest_config(config, "showCheckBeforeReporting", TRUE)))
                    .jr_guidance_block(bullets = .jr_core_ttest_checklist(result)) else NULL,
            "Literature and guidance" = paste(
                "Cohen (1988), Cumming (2014) and Lakens (2013) provide general guidance on effect sizes and interval-based interpretation.",
                "Complete bibliographic entries are provided in the References output."
            )
        )
        guidance_cards <- c(
            guidance_cards,
            .jr_interpretation_guidance_panel(sections, analysis_label = label)
        )
    }
    if (!length(report_cards))
        report_cards <- .jr_html_card(
            "Suggested APA report", "No suggested wording selected",
            "Enable Suggested report wording to show the concise reporting paragraph."
        )
    reference_keys <- unique(unlist(lapply(analyses, `[[`, "references")))
    if (length(unique(vapply(analyses, `[[`, character(1), "outcome"))) > 1L &&
            adjustment %in% c("holm", "bonferroni"))
        reference_keys <- unique(c(
            reference_keys,
            if (identical(adjustment, "holm")) "Holm1979" else character()
        ))
    reference_keys <- unique(c(
        reference_keys,
        if (length(analyses)) c("Cohen1988", "Cumming2014", "Lakens2013") else character()
    ))
    references <- if (length(analyses) &&
            isTRUE(.jr_core_ttest_config(config, "showReferences", TRUE)))
        .jr_methods_references_html(keys = reference_keys)
    else ""
    list(
        report = paste0(
            "<div style='width:100%;box-sizing:border-box;display:block;'>",
            paste(report_cards, collapse = ""), "</div>"
        ),
        guidance = paste0(
            "<div style='width:100%;box-sizing:border-box;display:block;'>",
            paste(guidance_cards, collapse = ""), "</div>"
        ),
        references = references,
        referenceKeys = reference_keys
    )
}
