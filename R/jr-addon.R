#' @importFrom jmvcore .

.jr_ttest_adjusted_reporting_results <- function(results, adjustment_info) {
    if (!isTRUE(adjustment_info$active))
        return(results)
    adjusted_results <- results
    for (i in seq_along(adjusted_results)) {
        result <- adjusted_results[[i]]
        if (!inherits(result, "edu_analysis") || !identical(result$analysis, "ttest"))
            next
        test_id <- sprintf("result-%d-statistic-1", i)
        adjusted_p <- unname(adjustment_info$adjusted[test_id])
        decision <- unname(adjustment_info$decision[test_id])
        if (!is.finite(adjusted_p) || is.na(decision))
            next
        apa <- result$report_blocks$apa %||% ""
        apa <- sub(", p ", ", unadjusted p ", apa, fixed = TRUE)
        if (identical(decision, "Significant before adjustment only")) {
            apa <- sub(
                "Results indicated a statistically significant difference",
                "The unadjusted comparison indicated a statistically significant difference",
                apa, fixed = TRUE
            )
            apa <- sub(
                "A paired-samples t-test indicated a statistically significant change",
                "The unadjusted paired-samples t-test indicated a statistically significant change",
                apa, fixed = TRUE
            )
        }
        adjustment_sentence <- switch(
            decision,
            `Significant after adjustment` = sprintf(
                "This comparison remained statistically significant after %s adjustment (%s-adjusted p %s).",
                adjustment_info$label, adjustment_info$label, .jr_p(adjusted_p)
            ),
            `Significant before adjustment only` = sprintf(
                "However, it did not remain statistically significant after %s adjustment (%s-adjusted p %s).",
                adjustment_info$label, adjustment_info$label, .jr_p(adjusted_p)
            ),
            `Not significant after adjustment` = sprintf(
                "The comparison was not statistically significant after %s adjustment (%s-adjusted p %s).",
                adjustment_info$label, adjustment_info$label, .jr_p(adjusted_p)
            ),
            ""
        )
        result$report_blocks$apa <- paste(apa, adjustment_sentence)
        adjusted_results[[i]] <- result
    }
    adjusted_results
}

.jr_ttest_adjustment_summary <- function(info) {
    if (info$n_valid < 2L)
        return(info$note)
    switch(
        info$method,
        holm = sprintf(
            "A Holm correction was applied to control the familywise Type I error rate across the %d t-tests. Both unadjusted and Holm-adjusted p-values are reported.",
            info$n_valid
        ),
        bonferroni = sprintf(
            "A Bonferroni correction was applied to control the familywise Type I error rate across the %d t-tests. Both unadjusted and Bonferroni-adjusted p-values are reported.",
            info$n_valid
        ),
        none = "The p-values were not adjusted for multiple comparisons; therefore, the familywise risk of Type I error may be increased."
    )
}

.jr_ttest_adjustment_guidance <- function(info) {
    if (info$n_valid < 2L)
        return(info$note)
    general <- paste(
        "Running several t-tests as part of the same research question increases the probability of obtaining at least one false-positive result. A multiple-comparison adjustment can be used to control this familywise Type I error risk.",
        "Important: Apply one correction only when the t-tests form a meaningful family of related comparisons addressing the same overall research question. Do not group unrelated hypotheses merely because they are being analysed at the same time.",
        "Related refers to the research aims and hypothesis structure; it does not mean that the variables must necessarily be statistically correlated. Tests addressing separate research questions should normally be analysed as separate families.",
        sep = "\n\n"
    )
    method <- switch(
        info$method,
        holm = "The Holm procedure controls the familywise Type I error rate using a sequential adjustment. It is generally less conservative than the standard Bonferroni procedure. Interpret statistical significance using the Holm-adjusted p-values.",
        bonferroni = "The Bonferroni procedure controls the familywise Type I error rate by adjusting for the number of tests. It is simple and widely recognised, but it can be conservative when many tests are included. Interpret statistical significance using the Bonferroni-adjusted p-values.",
        none = "Multiple unadjusted t-tests increase the familywise probability of a Type I error. Consider Holm or Bonferroni adjustment when the tests belong to the same planned family of hypotheses."
    )
    before_only <- any(info$decision == "Significant before adjustment only", na.rm = TRUE)
    qualification <- paste(
        "Whether adjustment is appropriate depends on the study design, preregistered hypotheses, confirmatory or exploratory purpose, and the definition of the test family.",
        if (before_only)
            "A result that was significant before adjustment but not after adjustment should not be reported as statistically significant after correction. It may be described as an unadjusted finding that did not remain significant after controlling for multiple comparisons."
        else "",
        sep = if (before_only) "\n\n" else ""
    )
    paste(general, method, qualification, sep = "\n\n")
}

.jr_correlation_adjustment_summary <- function(info) {
    if (info$n_valid < 2L)
        return(info$note)
    family_sentence <- sprintf(
        "The %d correlations were treated as one comparison family for the purpose of p-value adjustment.",
        info$n_valid
    )
    introduction <- switch(
        info$method,
        holm = sprintf(
            "A series of %d correlations was examined. The Holm procedure was applied to control the familywise Type I error rate. Both unadjusted and Holm-adjusted p-values are reported.",
            info$n_valid
        ),
        bonferroni = sprintf(
            "A series of %d correlations was examined. A Bonferroni adjustment was applied to control the familywise Type I error rate. Both unadjusted and Bonferroni-adjusted p-values are reported.",
            info$n_valid
        ),
        bh = sprintf(
            "A series of %d correlations was examined. P-values were adjusted using the Benjamini-Hochberg procedure to control the false discovery rate. Both unadjusted and adjusted p-values are reported.",
            info$n_valid
        ),
        none = sprintf(
            "A series of %d correlations was examined. The p-values were not adjusted for multiple comparisons; therefore, the risk of false-positive findings across the set of tests may be increased.",
            info$n_valid
        )
    )
    if (identical(info$method, "none")) introduction else
        paste(introduction, family_sentence, sep = "\n\n")
}

.jr_correlation_adjustment_guidance <- function(info) {
    if (info$n_valid < 2L)
        return(info$note)
    general <- paste(
        "Testing several correlations increases the probability of obtaining at least one false-positive result. A multiple-comparison adjustment may be appropriate when the correlations form a planned family of hypotheses addressing the same overall research question.",
        "Important: The correlations should form a meaningful family of related tests. They should not be grouped merely because they appear in the same table or were calculated at the same time. Correlations addressing different research questions may require separate correction families.",
        "The variables do not have to be statistically correlated with one another to form a hypothesis family. The family is defined by the research aims, theoretical rationale, and planned analysis structure.",
        sep = "\n\n"
    )
    method <- switch(
        info$method,
        holm = "The Holm procedure controls the familywise Type I error rate using a sequential adjustment. It is generally less conservative than the standard Bonferroni procedure. Interpret statistical significance using the Holm-adjusted p-values.",
        bonferroni = "The Bonferroni procedure controls the familywise Type I error rate by accounting for the number of tests. It is straightforward and widely recognised but can be conservative, particularly when many correlations are tested. Interpret statistical significance using the Bonferroni-adjusted p-values.",
        bh = "The Benjamini-Hochberg procedure controls the expected proportion of false discoveries among results declared statistically significant. It is less conservative than familywise error procedures and may be appropriate for exploratory analyses involving many correlations. Interpret significance using the adjusted p-values and identify the procedure as false discovery rate control.",
        none = "These p-values have not been adjusted for multiple testing. When the correlations represent one family of related hypotheses, the probability of obtaining at least one false-positive result may be greater than the nominal alpha level."
    )
    purpose <- paste(
        "Holm is generally suitable for a planned or confirmatory family of correlations. Benjamini-Hochberg may be more suitable for a large exploratory correlation set where limiting the expected proportion of false discoveries is more important than controlling the probability of any false-positive result.",
        "Whether adjustment is appropriate depends on the study design, preregistration, confirmatory or exploratory purpose, number of tests, theoretical relationship between the hypotheses, and how the test family was defined.",
        sep = "\n\n"
    )
    before_only <- any(info$decision == "Significant before adjustment only", na.rm = TRUE)
    changed <- if (before_only) {
        "A correlation that is statistically significant before adjustment but not after adjustment should not be reported as statistically significant after correction. It may be described as an unadjusted association that did not remain significant after controlling for multiple comparisons."
    } else {
        ""
    }
    paste(.jr_nonempty_unique(c(general, method, changed, purpose)), collapse = "\n\n")
}

.jr_adjustment_summary <- function(info) {
    if (identical(info$family %||% "", "correlation"))
        .jr_correlation_adjustment_summary(info)
    else
        .jr_ttest_adjustment_summary(info)
}

.jr_adjustment_guidance <- function(info) {
    if (identical(info$family %||% "", "correlation"))
        .jr_correlation_adjustment_guidance(info)
    else
        .jr_ttest_adjustment_guidance(info)
}

.jr_addon_call_variables <- function(result, name) {
    value <- tryCatch(result$call[[name]], error = function(e) NULL)
    if (is.null(value) || identical(value, quote(NULL)))
        return(character())
    variables <- if (is.character(value)) {
        value
    } else if (is.call(value) && identical(as.character(value[[1]]), "c")) {
        unlist(lapply(as.list(value)[-1], function(item) {
            if (is.character(item)) item else all.vars(item)
        }), use.names = FALSE)
    } else {
        all.vars(value)
    }
    variables <- trimws(as.character(variables))
    unique(variables[!is.na(variables) & nzchar(variables)])
}

.jr_addon_display_variables <- function(result, variables) {
    labels <- result$variable_labels %||% character()
    vapply(variables, function(variable) {
        display <- unname(labels[variable])
        if (length(display) == 1L && !is.na(display) && nzchar(trimws(display)))
            trimws(display)
        else
            variable
    }, character(1), USE.NAMES = FALSE)
}

.jr_addon_result_title <- function(result) {
    if (!inherits(result, "edu_analysis"))
        return("Analysis")
    if (identical(result$analysis, "correlation")) {
        metadata <- .jr_correlation_metadata(result)
        if (!is.null(metadata)) {
            method <- switch(
                metadata$method,
                pearson = "Pearson",
                spearman = "Spearman",
                kendall = "Kendall"
            )
            variables <- .jr_addon_display_variables(
                result, c(metadata$variable1, metadata$variable2)
            )
            return(sprintf("%s: %s with %s", method, variables[1], variables[2]))
        }
    }
    outcome <- .jr_addon_call_variables(result, "outcome")
    paired <- .jr_addon_call_variables(result, "paired_outcome")
    group <- .jr_addon_call_variables(result, "group")
    if (!length(group))
        group <- .jr_addon_call_variables(result, "factors")
    outcome <- .jr_addon_display_variables(result, outcome)
    paired <- .jr_addon_display_variables(result, paired)
    group <- .jr_addon_display_variables(result, group)
    test <- if (is.data.frame(result$statistics) && nrow(result$statistics) > 0L &&
            "test" %in% names(result$statistics)) {
        trimws(as.character(result$statistics$test[1]))
    } else {
        ""
    }
    prefix <- result$label %||% "Analysis"
    if (identical(result$analysis, "ttest") && nzchar(test))
        prefix <- test
    if (identical(result$analysis, "anova_oneway") && nzchar(test))
        prefix <- tools::toTitleCase(test)
    if (length(outcome) && length(paired))
        return(sprintf("%s: %s with %s", prefix, outcome[1], paired[1]))
    if (length(outcome) && length(group))
        return(sprintf(
            "%s: %s by %s", prefix, outcome[1], paste(group, collapse = ", ")
        ))
    prefix
}

.jr_addon_report_html <- function(results, options = NULL, title = "Guided report", note = "",
                                  adjustment = "none", alpha = .05) {
    adjustment_info <- .jr_addon_adjustment_info(results, adjustment, alpha)
    failures <- Filter(function(x) inherits(x, "try-error"), results)
    results <- .jr_adjusted_reporting_results(results, adjustment_info)
    if (identical(adjustment_info$family %||% "", "correlation"))
        results <- .jr_correlation_unique_results(results, adjustment_info)
    results <- Filter(function(x) inherits(x, "edu_analysis"), results)
    wrap <- function(html) paste0("<div style='width:100%;box-sizing:border-box;display:block;'>", html, "</div>")
    if (length(results) == 0L && length(failures) > 0L) {
        message <- sub("^Error[^:]*:\\s*", "", as.character(failures[[1]]))
        return(wrap(.jr_html_card(
            "Automatic report", "Report could not be generated",
            paste("jReport received the analysis variables but encountered a calculation problem:", message),
            accent = "#b46c21"
        )))
    }
    if (length(results) == 0L)
        return(wrap(.jr_html_card("Report add-on", title, "Select valid analysis variables to generate report text.")))
    if (length(results) == 1L && identical(results[[1]]$analysis, "regression")) {
        return(.jr_regression_report_cards_html(
            results[[1]], options = options
        ))
    }
    if (length(results) == 1L && identical(results[[1]]$analysis, "anova_between")) {
        return(.jr_anova_between_report_sections_html(
            results[[1]], options = options,
            posthoc_text = .jr_addon_posthoc_text(results)
        ))
    }
    show_analysis_labels <- length(results) > 1L
    sections <- vapply(results, function(result) {
        report_style <- "apa7"
        apa_text <- if (is.null(options)) {
            result$report_blocks$apa %||% edu_report(result, style = "apa7", format = "paragraph")
        } else {
            report_style <- .jr_jamovi_report_args(options)$style
            .jr_jamovi_text(result, options)
        }
        .jr_build_report_sections_html(
            apa_wording = apa_text,
            report_style = report_style,
            analysis_label = if (show_analysis_labels)
                .jr_addon_result_title(result) else ""
        )
    }, character(1))
    paste0("<div style='width:100%;box-sizing:border-box;display:block;'>", paste(sections, collapse = ""), "</div>")
}

.jr_addon_heading_html <- function() {
    .jr_html_card(
        "jReport | Automatic report add-on", "jReport output starts here",
        paste(
            "The tables and narrative below were generated by jReport from the selected built-in jamovi analysis.",
            "Simplified report-style output is available only for supported analyses and will appear when those analyses are run from their respective jamovi analysis menus.",
            "Custom report style, format, tone, and included-content controls are available in the separate jReport module entries; built-in analysis add-ons use a fixed APA reporting profile."
        )
    )
}

.jr_addon_reporting_options <- function() {
    list(
        reportStyle = "apa7", reportFormat = "paragraph", reportTone = "student_friendly",
        reportDescriptives = TRUE, reportAssumptions = TRUE, reportStatistic = TRUE,
        reportDf = TRUE, reportP = TRUE, reportEffect = TRUE,
        reportCI = TRUE, reportInterpretation = TRUE, reportCautions = TRUE
    )
}

.jr_addon_interpretation_html <- function(results, note = "", adjustment = "none", alpha = .05) {
    adjustment_info <- .jr_addon_adjustment_info(results, adjustment, alpha)
    results <- .jr_adjusted_reporting_results(results, adjustment_info)
    if (identical(adjustment_info$family %||% "", "correlation"))
        results <- .jr_correlation_unique_results(results, adjustment_info)
    results <- Filter(function(x) inherits(x, "edu_analysis"), results)
    if (length(results) == 0L)
        return(.jr_html_card(
            "Interpretation guidance", "jReport",
            "Select valid analysis variables to generate interpretation guidance.",
            accent = "#b46c21"
        ))
    show_analysis_labels <- length(results) > 1L
    adjustment_guidance <- .jr_adjustment_guidance(adjustment_info)
    posthoc_text <- .jr_addon_posthoc_text(results)
    cards <- vapply(seq_along(results), function(index) {
        result <- results[[index]]
        sections <- .jr_interpretation_sections(result)
        follow_up <- .jr_nonempty_unique(c(
            sections[["Follow-up analyses"]] %||% character(),
            if (index == 1L) adjustment_guidance else "",
            if (index == 1L) posthoc_text else ""
        ))
        if (length(follow_up))
            sections[["Follow-up analyses"]] <- follow_up
        if (index == 1L && nzchar(note)) {
            checks <- sections[["Check before using this result"]]
            if (!is.list(checks))
                checks <- .jr_guidance_block(text = checks)
            checks$text <- .jr_nonempty_unique(c(checks$text, note))
            sections[["Check before using this result"]] <- checks
        }
        analysis_label <- if (show_analysis_labels)
            .jr_addon_result_title(result) else ""
        .jr_interpretation_guidance_panel(
            sections,
            analysis_label = analysis_label
        )
    }, character(1))
    paste0("<div style='width:100%;box-sizing:border-box;display:block;'>", paste(cards, collapse = ""), "</div>")
}

.jr_parent_ci <- function(parent) {
    value <- tryCatch(parent$options$ciWidth, error = function(e) NULL)
    if (is.null(value) || !is.finite(value))
        .95
    else value / 100
}

.jr_parent_model_formula <- function(outcome, fallback_terms, blocks = NULL) {
    model_terms <- list()
    if (!is.null(blocks)) {
        model_terms <- unlist(lapply(blocks, function(block) {
            if (is.null(block) || length(block) == 0L)
                return(list())
            lapply(block, as.character)
        }), recursive = FALSE)
    }
    if (length(model_terms) == 0L)
        model_terms <- fallback_terms
    unique_keys <- vapply(model_terms, paste, collapse = "\r", character(1))
    .jr_formula(outcome, model_terms[!duplicated(unique_keys)])
}

.jr_apply_reference_levels <- function(data, references) {
    if (is.null(references) || length(references) == 0L)
        return(data)
    adjusted <- data
    for (reference in references) {
        variable <- tryCatch(as.character(reference$var)[1], error = function(e) NA_character_)
        level <- tryCatch(as.character(reference$ref)[1], error = function(e) NA_character_)
        if (is.na(variable) || is.na(level) || !variable %in% names(adjusted))
            next
        values <- factor(adjusted[[variable]])
        if (level %in% levels(values))
            adjusted[[variable]] <- stats::relevel(values, ref = level)
    }
    adjusted
}

.jr_accuracy_note <- function(context) {
    paste(
        context,
        "IMPORTANT: This generated reporting output must be checked for accuracy against the final jamovi analysis before it is used in assessed, clinical, or published work.",
        "Check all selected model terms, interactions, contrasts, reference levels, correction choices, and any other settings that could change the wording or interpretation."
    )
}

.jr_module_library_path <- function(package = "jReport") {
    package_path <- suppressWarnings(system.file(package = package))
    if (!nzchar(package_path))
        return(NULL)
    module_library <- dirname(package_path)
    if (!dir.exists(module_library))
        return(NULL)
    module_library
}

.jr_require_module_namespace <- function(package, module_library = .jr_module_library_path()) {
    if (is.null(module_library) || !dir.exists(file.path(module_library, package)))
        return(requireNamespace(package, quietly = TRUE))
    requireNamespace(package, quietly = TRUE, lib.loc = module_library)
}

.jr_addon_enable_library <- function(package_path = NULL) {
    package <- if (is.null(package_path)) "jReport" else basename(package_path)
    module_library <- .jr_module_library_path(package)
    required <- c(
        "jReport", "afex", "car", "effectsize", "ggplot2", "jmvcore",
        "parameters", "performance", "psych", "R6"
    )
    loaded <- vapply(required, .jr_require_module_namespace, logical(1), module_library = module_library)
    invisible(all(loaded))
}

.jr_addon_effect_label <- function(result, row) {
    statistic <- result$statistics[row, , drop = FALSE]
    benchmark <- function(label, value) {
        sprintf("%s (%s)", label, .jr_effect_magnitude(result$analysis, value))
    }
    if (identical(result$analysis, "ttest"))
        return(benchmark(sprintf("Cohen's d = %s", .jr_num(statistic$effect, 2L, TRUE)), statistic$effect))
    if (result$analysis %in% c("mann_whitney", "wilcoxon_signed_rank"))
        return(benchmark(sprintf("Rank-biserial r = %s", .jr_num(statistic$effect, 2L, TRUE)), statistic$effect))
    if (identical(result$analysis, "bayes_ttest"))
        return(sprintf("BF10 = %s", .jr_num(statistic$statistic, 2L)))
    if (identical(result$analysis, "anova_oneway"))
        return(benchmark(sprintf("Eta-squared = %s", .jr_num(statistic$effect, 2L, TRUE)), statistic$effect))
    if (result$analysis %in% c("anova_between", "ancova", "anova_rm", "anova_mixed"))
        return(benchmark(sprintf("Partial eta-squared = %s", .jr_num(statistic$effect, 2L, TRUE)), statistic$effect))
    if (identical(result$analysis, "manova"))
        return(benchmark(sprintf("Pillai's trace = %s", .jr_num(statistic$effect, 2L, TRUE)), statistic$effect))
    if (identical(result$analysis, "correlation"))
        return(benchmark(sprintf("%s = %s", statistic$test, .jr_num(statistic$statistic, 2L, TRUE)), statistic$statistic))
    if (identical(result$analysis, "regression"))
        return(benchmark(sprintf("R-squared = %s", .jr_num(statistic$r2, 2L, TRUE)), statistic$r2))
    if (identical(result$analysis, "logistic_regression"))
        return(benchmark(sprintf("McFadden R-squared = %s", .jr_num(statistic$r2, 2L, TRUE)), statistic$r2))
    if (identical(result$analysis, "chisq_independence"))
        return(benchmark(sprintf("Cramer's V = %s", .jr_num(statistic$effect, 2L, TRUE)), statistic$effect))
    if (identical(result$analysis, "chisq_gof"))
        return(benchmark(sprintf("Cohen's w = %s", .jr_num(statistic$effect, 2L, TRUE)), statistic$effect))
    if (identical(result$analysis, "reliability_omega")) {
        coefficient <- as.character(statistic$coefficient)
        label <- if (identical(coefficient, "Cronbach's alpha")) "Cronbach's alpha" else "Omega"
        return(benchmark(sprintf("%s = %s", label, .jr_num(statistic$estimate, 2L, TRUE)), statistic$estimate))
    }
    ""
}

.jr_addon_test_label <- function(result, row) {
    statistic <- result$statistics[row, , drop = FALSE]
    if ("term" %in% names(statistic))
        return(as.character(statistic$term))
    if ("test" %in% names(statistic))
        return(as.character(statistic$test))
    if ("coefficient" %in% names(statistic))
        return(as.character(statistic$coefficient))
    result$label
}

.jr_ttest_adjustment_method <- function(method = "holm") {
    method <- tolower(as.character(method %||% "holm")[1])
    if (!method %in% c("holm", "bonferroni", "none"))
        method <- "holm"
    method
}

.jr_tag_ttest_attempt <- function(value, label) {
    if (inherits(value, "try-error")) {
        attr(value, "jr_test_family") <- "ttest"
        attr(value, "jr_test_label") <- as.character(label)[1]
    }
    value
}

.jr_ttest_adjustment_info <- function(results, method = "holm", alpha = .05) {
    method <- .jr_ttest_adjustment_method(method)
    records <- list()
    for (result_index in seq_along(results)) {
        result <- results[[result_index]]
        if (inherits(result, "try-error") &&
                identical(attr(result, "jr_test_family"), "ttest")) {
            records[[length(records) + 1L]] <- data.frame(
                test_id = sprintf("result-%d-statistic-1", result_index),
                result_index = result_index, statistic_index = 1L,
                statistic = NA_real_, p = NA_real_, valid = FALSE,
                stringsAsFactors = FALSE
            )
            next
        }
        if (!inherits(result, "edu_analysis") || !identical(result$analysis, "ttest"))
            next
        statistics <- result$statistics
        if (!is.data.frame(statistics) || nrow(statistics) == 0L)
            next
        for (statistic_index in seq_len(nrow(statistics))) {
            statistic <- statistics[statistic_index, , drop = FALSE]
            value <- suppressWarnings(as.numeric(statistic$statistic %||% NA_real_)[1])
            p <- suppressWarnings(as.numeric(statistic$p %||% NA_real_)[1])
            records[[length(records) + 1L]] <- data.frame(
                test_id = sprintf("result-%d-statistic-%d", result_index, statistic_index),
                result_index = result_index,
                statistic_index = statistic_index,
                statistic = value,
                p = p,
                valid = is.finite(value) && is.finite(p) && p >= 0 && p <= 1,
                stringsAsFactors = FALSE
            )
        }
    }
    records <- if (length(records)) do.call(rbind, records) else data.frame(
        test_id = character(), result_index = integer(), statistic_index = integer(),
        statistic = numeric(), p = numeric(), valid = logical(), stringsAsFactors = FALSE
    )
    valid <- records$valid
    n_valid <- sum(valid)
    active <- n_valid >= 2L && method != "none"
    adjusted <- stats::setNames(rep(NA_real_, nrow(records)), records$test_id)
    if (n_valid > 0L) {
        adjusted[records$test_id[valid]] <- if (active) {
            stats::p.adjust(records$p[valid], method = method)
        } else {
            records$p[valid]
        }
    }
    decision <- stats::setNames(rep("Test could not be calculated", nrow(records)), records$test_id)
    if (n_valid > 0L) {
        raw_significant <- records$p[valid] < alpha
        adjusted_significant <- adjusted[records$test_id[valid]] < alpha
        decision[records$test_id[valid]] <- ifelse(
            adjusted_significant,
            "Significant after adjustment",
            ifelse(
                raw_significant,
                "Significant before adjustment only",
                "Not significant after adjustment"
            )
        )
    }
    label <- switch(method, holm = "Holm", bonferroni = "Bonferroni", none = "None")
    heading <- if (active) paste0(label, "-adjusted p") else "Adjusted p"
    note <- if (n_valid >= 2L) {
        switch(
            method,
            holm = sprintf(
                "P-values were adjusted using the Holm procedure to control the familywise Type I error rate across the %d valid t-tests.",
                n_valid
            ),
            bonferroni = sprintf(
                "P-values were adjusted using the Bonferroni procedure to control the familywise Type I error rate across the %d valid t-tests.",
                n_valid
            ),
            none = "P-values are unadjusted. Conducting multiple unadjusted t-tests increases the familywise risk of Type I error."
        )
    } else if (nrow(records) >= 2L && n_valid == 1L) {
        "Only one valid t-test was available; therefore, no multiple-comparison adjustment was required."
    } else {
        ""
    }
    list(
        method = method, label = label, records = records, n_valid = n_valid,
        active = active, adjusted = adjusted, decision = decision,
        heading = heading, note = note, alpha = alpha
    )
}

.jr_is_correlation_family <- function(results) {
    if (length(results) == 0L)
        return(FALSE)
    is_correlation <- vapply(results, function(result) {
        (inherits(result, "edu_analysis") && identical(result$analysis, "correlation")) ||
            (inherits(result, "try-error") &&
                identical(attr(result, "jr_test_family"), "correlation"))
    }, logical(1))
    any(is_correlation) && all(is_correlation)
}

.jr_correlation_adjustment_method <- function(method = "holm") {
    method <- tolower(as.character(method %||% "holm")[1])
    if (identical(method, "fdr"))
        method <- "bh"
    if (!method %in% c("holm", "bonferroni", "bh", "none"))
        method <- "holm"
    method
}

.jr_correlation_metadata <- function(result) {
    if (inherits(result, "try-error") &&
            identical(attr(result, "jr_test_family"), "correlation")) {
        variables <- as.character(attr(result, "jr_correlation_variables") %||% character())
        method <- tolower(as.character(attr(result, "jr_correlation_method") %||% "")[1])
    } else if (inherits(result, "edu_analysis") &&
            identical(result$analysis, "correlation")) {
        variables <- as.character(attr(result, "jr_correlation_variables") %||% character())
        if (length(variables) < 2L && is.data.frame(result$plot_data) &&
                all(c("x_name", "y_name") %in% names(result$plot_data))) {
            variables <- c(
                as.character(result$plot_data$x_name[1]),
                as.character(result$plot_data$y_name[1])
            )
        }
        method <- tolower(as.character(
            attr(result, "jr_correlation_method") %||%
                result$statistics$test[1] %||% ""
        )[1])
    } else {
        return(NULL)
    }
    variables <- variables[!is.na(variables) & nzchar(variables)]
    if (length(variables) < 2L || !method %in% c("pearson", "spearman", "kendall"))
        return(NULL)
    variables <- variables[seq_len(2L)]
    canonical <- sort(enc2utf8(variables), method = "radix")
    list(
        variable1 = variables[1], variable2 = variables[2], method = method,
        test_id = paste(c("correlation", method, canonical), collapse = "::"),
        diagonal = identical(variables[1], variables[2])
    )
}

.jr_tag_correlation_attempt <- function(value, method, variables) {
    attr(value, "jr_test_family") <- "correlation"
    attr(value, "jr_correlation_method") <- tolower(as.character(method)[1])
    attr(value, "jr_correlation_variables") <- as.character(variables)[seq_len(2L)]
    value
}

.jr_correlation_adjustment_info <- function(results, method = "holm", alpha = .05) {
    method <- .jr_correlation_adjustment_method(method)
    records <- list()
    record_ids <- character()
    for (result_index in seq_along(results)) {
        result <- results[[result_index]]
        metadata <- .jr_correlation_metadata(result)
        if (is.null(metadata) || isTRUE(metadata$diagonal))
            next
        statistic <- if (inherits(result, "edu_analysis") &&
                is.data.frame(result$statistics) && nrow(result$statistics) > 0L) {
            result$statistics[1, , drop = FALSE]
        } else {
            NULL
        }
        coefficient <- if (is.null(statistic)) NA_real_ else
            suppressWarnings(as.numeric(statistic$statistic %||% NA_real_)[1])
        p <- if (is.null(statistic)) NA_real_ else
            suppressWarnings(as.numeric(statistic$p %||% NA_real_)[1])
        n <- if (!is.null(statistic) && "n" %in% names(statistic)) {
            suppressWarnings(as.numeric(statistic$n)[1])
        } else if (inherits(result, "edu_analysis") &&
                is.data.frame(result$descriptives) && "n" %in% names(result$descriptives)) {
            suppressWarnings(min(as.numeric(result$descriptives$n), na.rm = TRUE))
        } else {
            NA_real_
        }
        valid <- is.finite(coefficient) && is.finite(p) && p >= 0 && p <= 1 &&
            is.finite(n) && n >= 3L
        candidate <- data.frame(
            test_id = metadata$test_id,
            result_index = result_index,
            statistic_index = 1L,
            variable1 = metadata$variable1,
            variable2 = metadata$variable2,
            method = metadata$method,
            statistic = coefficient,
            p = p,
            n = n,
            valid = valid,
            stringsAsFactors = FALSE
        )
        duplicate_index <- match(metadata$test_id, record_ids)
        if (is.na(duplicate_index)) {
            records[[length(records) + 1L]] <- candidate
            record_ids <- c(record_ids, metadata$test_id)
        } else if (!isTRUE(records[[duplicate_index]]$valid) && isTRUE(valid)) {
            records[[duplicate_index]] <- candidate
        }
    }
    records <- if (length(records)) do.call(rbind, records) else data.frame(
        test_id = character(), result_index = integer(), statistic_index = integer(),
        variable1 = character(), variable2 = character(), method = character(),
        statistic = numeric(), p = numeric(), n = numeric(), valid = logical(),
        stringsAsFactors = FALSE
    )
    valid <- records$valid
    n_valid <- sum(valid)
    active <- n_valid >= 2L && method != "none"
    adjusted <- stats::setNames(rep(NA_real_, nrow(records)), records$test_id)
    if (n_valid > 0L) {
        adjusted[records$test_id[valid]] <- if (active) {
            stats::p.adjust(
                records$p[valid],
                method = if (identical(method, "bh")) "BH" else method
            )
        } else {
            records$p[valid]
        }
    }
    decision <- stats::setNames(rep("Test could not be calculated", nrow(records)), records$test_id)
    if (n_valid > 0L) {
        raw_significant <- records$p[valid] < alpha
        adjusted_significant <- adjusted[records$test_id[valid]] < alpha
        decision[records$test_id[valid]] <- ifelse(
            adjusted_significant,
            "Significant after adjustment",
            ifelse(raw_significant,
                "Significant before adjustment only",
                "Not significant after adjustment")
        )
    }
    label <- switch(
        method,
        holm = "Holm",
        bonferroni = "Bonferroni",
        bh = "Benjamini-Hochberg",
        none = "None"
    )
    heading <- if (active) {
        if (identical(method, "bh")) "BH-adjusted p" else paste0(label, "-adjusted p")
    } else {
        "Adjusted p"
    }
    note <- if (n_valid >= 2L) {
        switch(
            method,
            holm = sprintf(
                "P-values were adjusted using the Holm procedure to control the familywise Type I error rate across the %d unique, valid correlation tests.",
                n_valid
            ),
            bonferroni = sprintf(
                "P-values were adjusted using the Bonferroni procedure to control the familywise Type I error rate across the %d unique, valid correlation tests.",
                n_valid
            ),
            bh = sprintf(
                "P-values were adjusted using the Benjamini-Hochberg procedure to control the false discovery rate across the %d unique, valid correlation tests.",
                n_valid
            ),
            none = sprintf(
                "P-values are unadjusted across the %d unique, valid correlation tests. Conducting multiple correlation tests increases the probability of obtaining false-positive findings.",
                n_valid
            )
        )
    } else if (nrow(records) >= 2L && n_valid == 1L) {
        "Only one valid correlation test was available; therefore, no multiple-comparison adjustment was required."
    } else if (nrow(records) > 0L && n_valid == 0L) {
        "None of the requested correlations could be calculated. Review the selected variables, sample sizes, missing data, and variable variation."
    } else {
        ""
    }
    list(
        family = "correlation", method = method, label = label, records = records,
        n_valid = n_valid, active = active, adjusted = adjusted,
        decision = decision, heading = heading, note = note, alpha = alpha
    )
}

.jr_addon_adjustment_info <- function(results, method = "holm", alpha = .05) {
    if (.jr_is_correlation_family(results))
        .jr_correlation_adjustment_info(results, method, alpha)
    else
        .jr_ttest_adjustment_info(results, method, alpha)
}

.jr_adjustment_reference_keys <- function(info) {
    if (!isTRUE(info$active))
        return(character())
    if (identical(info$family %||% "", "correlation")) {
        return(switch(
            info$method,
            holm = c("Holm1979", "Vickerstaff2019"),
            bonferroni = "Vickerstaff2019",
            bh = "BenjaminiHochberg1995",
            character()
        ))
    }
    c(if (identical(info$method, "holm")) "Holm1979", "Vickerstaff2019")
}

.jr_correlation_unique_results <- function(results, info) {
    if (!identical(info$family %||% "", "correlation") || nrow(info$records) == 0L)
        return(results)
    indices <- info$records$result_index[info$records$valid]
    results[indices]
}

.jr_correlation_adjusted_reporting_results <- function(results, info) {
    if (!isTRUE(info$active))
        return(results)
    adjusted_results <- results
    for (i in seq_len(nrow(info$records))) {
        record <- info$records[i, , drop = FALSE]
        if (!isTRUE(record$valid))
            next
        result <- adjusted_results[[record$result_index]]
        if (!inherits(result, "edu_analysis"))
            next
        coefficient <- record$statistic
        adjusted_p <- unname(info$adjusted[record$test_id])
        decision <- unname(info$decision[record$test_id])
        statistic <- result$statistics[1, , drop = FALSE]
        method_title <- switch(
            record$method,
            pearson = "Pearson",
            spearman = "Spearman",
            kendall = "Kendall"
        )
        symbol <- switch(record$method, pearson = "r", spearman = "\u03c1", kendall = "\u03c4")
        statistic_text <- if (identical(record$method, "pearson")) {
            sprintf("%s(%d) = %s", symbol, as.integer(record$n - 2L), .jr_num(coefficient, 2L, TRUE))
        } else {
            sprintf("%s = %s", symbol, .jr_num(coefficient, 2L, TRUE))
        }
        ci_text <- if (is.finite(statistic$ci_low) && is.finite(statistic$ci_high)) {
            sprintf(", 95%% CI %s", .jr_ci(statistic$ci_low, statistic$ci_high, 2L, TRUE))
        } else {
            ""
        }
        adjusted_label <- if (identical(info$method, "bh")) "BH" else info$label
        direction <- if (coefficient >= 0) "positively" else "negatively"
        apa <- switch(
            decision,
            `Significant after adjustment` = sprintf(
                "%s was significantly %s correlated with %s, %s %s, unadjusted p %s, %s-adjusted p %s%s, n = %d.",
                record$variable1, direction, record$variable2, method_title,
                statistic_text, .jr_p(record$p), adjusted_label,
                .jr_p(adjusted_p), ci_text, as.integer(record$n)
            ),
            `Significant before adjustment only` = sprintf(
                "%s was %s correlated with %s, %s %s, unadjusted p %s%s, n = %d; however, the association did not remain statistically significant following %s adjustment, adjusted p %s.",
                record$variable1, direction, record$variable2, method_title,
                statistic_text, .jr_p(record$p), ci_text, as.integer(record$n),
                info$label, .jr_p(adjusted_p)
            ),
            sprintf(
                "%s was not significantly correlated with %s after %s adjustment, %s %s, unadjusted p %s, %s-adjusted p %s%s, n = %d.",
                record$variable1, record$variable2, info$label, method_title,
                statistic_text, .jr_p(record$p), adjusted_label,
                .jr_p(adjusted_p), ci_text, as.integer(record$n)
            )
        )
        interpretation <- switch(
            decision,
            `Significant after adjustment` = sprintf(
                "The association remained statistically significant after %s adjustment.", info$label
            ),
            `Significant before adjustment only` = sprintf(
                "The unadjusted association did not remain statistically significant after %s adjustment and should not be reported as significant after correction.", info$label
            ),
            sprintf(
                "After %s adjustment, the sample did not provide clear evidence that this association differs from zero.", info$label
            )
        )
        result$report_blocks$apa <- apa
        result$report_blocks$plain <- interpretation
        result$report_blocks$descriptives <- interpretation
        result$interpretation <- interpretation
        adjusted_results[[record$result_index]] <- result
    }
    adjusted_results
}

.jr_adjusted_reporting_results <- function(results, info) {
    if (identical(info$family %||% "", "correlation"))
        .jr_correlation_adjusted_reporting_results(results, info)
    else
        .jr_ttest_adjusted_reporting_results(results, info)
}

.jr_correlation_apa_rows <- function(results, adjustment = "none", alpha = .05) {
    info <- .jr_correlation_adjustment_info(results, adjustment, alpha)
    if (nrow(info$records) == 0L) {
        rows <- data.frame()
        attr(rows, "adjustment") <- info
        return(rows)
    }
    rows <- lapply(seq_len(nrow(info$records)), function(i) {
        record <- info$records[i, , drop = FALSE]
        result <- results[[record$result_index]]
        valid_result <- isTRUE(record$valid) && inherits(result, "edu_analysis")
        statistic <- if (valid_result) result$statistics[1, , drop = FALSE] else NULL
        method_title <- switch(
            record$method,
            pearson = "Pearson",
            spearman = "Spearman",
            kendall = "Kendall"
        )
        lower <- if (!is.null(statistic) && "ci_low" %in% names(statistic))
            as.numeric(statistic$ci_low) else NA_real_
        upper <- if (!is.null(statistic) && "ci_high" %in% names(statistic))
            as.numeric(statistic$ci_high) else NA_real_
        data.frame(
            test_id = record$test_id,
            analysis = "Correlation",
            test = sprintf("%s: %s with %s", method_title, record$variable1, record$variable2),
            variable1 = record$variable1,
            variable2 = record$variable2,
            method = method_title,
            statistic = if (valid_result) record$statistic else NA_real_,
            df1 = if (valid_result && identical(record$method, "pearson"))
                record$n - 2L else NA_real_,
            df2 = "",
            p = if (valid_result) record$p else NA_real_,
            p_adjusted = unname(info$adjusted[record$test_id]),
            adjustment_result = unname(info$decision[record$test_id]),
            effect = if (valid_result) .jr_addon_effect_label(result, 1L) else "",
            ci = if (is.finite(lower) && is.finite(upper))
                .jr_ci(lower, upper, 2L, TRUE) else "",
            n = if (valid_result) as.integer(record$n) else NA_integer_,
            stringsAsFactors = FALSE
        )
    })
    rows <- do.call(rbind, rows)
    attr(rows, "adjustment") <- info
    rows
}

.jr_addon_apa_rows <- function(results, adjustment = "none", alpha = .05) {
    if (.jr_is_correlation_family(results))
        return(.jr_correlation_apa_rows(results, adjustment, alpha))
    adjustment_info <- .jr_ttest_adjustment_info(results, adjustment, alpha)
    indexed_results <- lapply(
        seq_along(results), function(i) list(result = results[[i]], index = i)
    )
    if (length(indexed_results) == 0L)
        return(data.frame())
    rows <- lapply(indexed_results, function(indexed) {
        result <- indexed$result
        if (inherits(result, "try-error") &&
                identical(attr(result, "jr_test_family"), "ttest")) {
            test_id <- sprintf("result-%d-statistic-1", indexed$index)
            return(data.frame(
                test_id = test_id, analysis = "T-Test",
                test = attr(result, "jr_test_label") %||% "Requested t-test",
                statistic = NA_real_, df1 = NA_real_, df2 = "", p = NA_real_,
                p_adjusted = NA_real_,
                adjustment_result = "Test could not be calculated",
                effect = "", ci = "", stringsAsFactors = FALSE
            ))
        }
        if (!inherits(result, "edu_analysis"))
            return(NULL)
        statistics <- result$statistics
        do.call(rbind, lapply(seq_len(nrow(statistics)), function(i) {
            statistic <- statistics[i, , drop = FALSE]
            test_id <- sprintf("result-%d-statistic-%d", indexed$index, i)
            value <- if ("statistic" %in% names(statistic)) statistic$statistic else statistic$estimate
            df1 <- if ("df1" %in% names(statistic)) statistic$df1 else statistic$df %||% NA_real_
            df2 <- if ("df2" %in% names(statistic)) statistic$df2 else NA_real_
            if (identical(result$analysis, "ttest")) {
                lower <- result$effects$CI_low[1]
                upper <- result$effects$CI_high[1]
            } else {
                lower <- if ("ci_low" %in% names(statistic)) statistic$ci_low else NA_real_
                upper <- if ("ci_high" %in% names(statistic)) statistic$ci_high else NA_real_
            }
            data.frame(
                test_id = test_id,
                analysis = result$label,
                test = .jr_addon_test_label(result, i),
                statistic = as.numeric(value),
                df1 = as.numeric(df1),
                df2 = if (is.finite(as.numeric(df2))) .jr_num(df2, 2L) else "",
                p = if ("p" %in% names(statistic)) as.numeric(statistic$p) else NA_real_,
                p_adjusted = unname(adjustment_info$adjusted[test_id]),
                adjustment_result = unname(adjustment_info$decision[test_id]),
                effect = .jr_addon_effect_label(result, i),
                ci = if (is.finite(lower) && is.finite(upper))
                    .jr_ci(lower, upper, 2L, TRUE)
                else "",
                stringsAsFactors = FALSE
            )
        }))
    })
    rows <- Filter(Negate(is.null), rows)
    if (!length(rows))
        return(data.frame())
    rows <- do.call(rbind, rows)
    attr(rows, "adjustment") <- adjustment_info
    rows
}

.jr_addon_cell_rows <- function(results) {
    results <- Filter(function(x) inherits(x, "edu_analysis") &&
        x$analysis %in% c("chisq_independence", "chisq_gof"), results)
    if (length(results) == 0L)
        return(data.frame())
    rows <- lapply(results, function(result) {
        data.frame(
            analysis = result$label,
            category = result$cells$category,
            observed = result$cells$observed,
            expected = result$cells$expected,
            standardised_residual = result$cells$standardised_residual,
            stringsAsFactors = FALSE
        )
    })
    do.call(rbind, rows)
}

.jr_addon_coefficient_rows <- function(results) {
    results <- Filter(function(x) inherits(x, "edu_analysis") &&
        identical(x$analysis, "logistic_regression"), results)
    if (length(results) == 0L)
        return(data.frame())
    rows <- lapply(results, function(result) {
        coefficients <- result$parameters[result$parameters$Parameter != "(Intercept)", , drop = FALSE]
        data.frame(
            predictor = coefficients$Parameter,
            estimate = coefficients$Coefficient,
            se = coefficients$SE,
            statistic = coefficients$z,
            p = coefficients$p,
            odds_ratio = coefficients$OR,
            confidence_interval = vapply(
                seq_len(nrow(coefficients)),
                function(i) .jr_or_ci(coefficients$CI_low[i], coefficients$CI_high[i]),
                character(1)
            ),
            stringsAsFactors = FALSE
        )
    })
    do.call(rbind, rows)
}

.jr_addon_assumption_rows <- function(results) {
    show_analysis_labels <- length(results) > 1L
    results <- Filter(function(x) inherits(x, "edu_analysis"), results)
    if (length(results) == 0L)
        return(data.frame())
    rows <- lapply(results, function(result) {
        diagnostics <- .jr_normalize_diagnostics(result$diagnostics)
        data.frame(
            analysis = if (show_analysis_labels)
                .jr_addon_result_title(result) else result$label,
            assumption = diagnostics$check,
            tested = diagnostics$tested,
            statistic = diagnostics$statistic,
            p = diagnostics$p,
            met = unname(c(
                Acceptable = "Yes",
                Caution = "Concern",
                Serious = "No",
                `Not required` = "Not required",
                `Not assessed` = "Check manually"
            )[diagnostics$status]),
            interpretation = diagnostics$interpretation,
            action = diagnostics$action,
            stringsAsFactors = FALSE
        )
    })
    do.call(rbind, rows)
}

.jr_addon_followup_rows <- function(results) {
    results <- Filter(function(x) inherits(x, "edu_analysis") &&
        identical(x$analysis, "manova") && is.data.frame(x$followups), results)
    if (length(results) == 0L)
        return(data.frame())
    rows <- lapply(results, function(result) {
        followups <- result$followups
        if (nrow(followups) == 0L)
            return(data.frame())
        data.frame(
            analysis = result$label,
            term = followups$term,
            outcome = followups$outcome,
            statistic = followups$statistic,
            df1 = followups$df1,
            df2 = followups$df2,
            p = followups$p,
            p_holm = followups$p_holm,
            effect = followups$effect,
            stringsAsFactors = FALSE
        )
    })
    do.call(rbind, rows)
}

.jr_addon_fill_table <- function(tbl, rows, optional = FALSE) {
    tbl$deleteRows()
    if (nrow(rows) > 0L)
        for (i in seq_len(nrow(rows)))
            tbl$addRow(rowKey = i, values = as.list(rows[i, ]))
    if (optional)
        tbl$setVisible(nrow(rows) > 0L)
}

.jr_addon_get <- function(group, name) {
    tryCatch(group$get(name), error = function(e) NULL)
}

.jr_addon_set_tables <- function(self, results, adjustment = "none", alpha = .05) {
    apa_rows        <- .jr_addon_apa_rows(results, adjustment = adjustment, alpha = alpha)
    adjustment_info <- attr(apa_rows, "adjustment") %||%
        .jr_addon_adjustment_info(results, adjustment, alpha)
    assumption_rows <- .jr_addon_assumption_rows(results)
    optional_rows <- list(
        jReportPostHoc      = .jr_addon_posthoc_rows(results),
        jReportCoefficients = .jr_addon_coefficient_rows(results),
        jReportCells        = .jr_addon_cell_rows(results),
        jReportFollowUps    = .jr_addon_followup_rows(results)
    )
    tbl <- .jr_addon_get(self$parent$results, "jReportApaTable")
    if (!is.null(tbl)) {
        correlation_multiple <- identical(adjustment_info$family %||% "", "correlation") &&
            adjustment_info$n_valid >= 2L
        for (column_name in c("variable1", "variable2", "method", "n")) {
            column <- tryCatch(tbl$getColumn(column_name), error = function(e) NULL)
            if (!is.null(column))
                column$setVisible(correlation_multiple)
        }
        raw_p_column <- tryCatch(tbl$getColumn("p"), error = function(e) NULL)
        if (!is.null(raw_p_column))
            raw_p_column$setTitle(if (isTRUE(adjustment_info$active)) "Unadjusted p" else "p")
        adjusted_column <- tryCatch(tbl$getColumn("p_adjusted"), error = function(e) NULL)
        decision_column <- tryCatch(tbl$getColumn("adjustment_result"), error = function(e) NULL)
        if (!is.null(adjusted_column)) {
            adjusted_column$setTitle(adjustment_info$heading)
            adjusted_column$setVisible(isTRUE(adjustment_info$active))
        }
        if (!is.null(decision_column))
            decision_column$setVisible(isTRUE(adjustment_info$active))
        tbl$setNote("multiple-comparisons", adjustment_info$note)
        display_rows <- apa_rows[, setdiff(names(apa_rows), "test_id"), drop = FALSE]
        .jr_addon_fill_table(tbl, display_rows)
        .jr_set_effect_notes(tbl, display_rows)
    }
    tbl <- .jr_addon_get(self$parent$results, "jReportAssumptions")
    if (!is.null(tbl)) {
        applies_to <- tryCatch(tbl$getColumn("analysis"), error = function(e) NULL)
        if (!is.null(applies_to))
            applies_to$setTitle(if (length(results) > 1L) "Applies to" else "Analysis")
        .jr_addon_fill_table(tbl, assumption_rows)
        .jr_set_guidance_notes(tbl, assumption_rows, "assumption")
    }
    for (nm in names(optional_rows)) {
        tbl <- .jr_addon_get(self$parent$results, nm)
        if (!is.null(tbl)) .jr_addon_fill_table(tbl, optional_rows[[nm]], optional = TRUE)
    }
}

.jr_addon_add_result_if_missing <- function(self, name, result) {
    existing <- tryCatch(self$parent$results$get(name), error = function(e) NULL)
    if (is.null(existing))
        self$parent$results$add(result)
}

.jr_addon_insert_tables <- function(self, posthoc = FALSE, coefficients = FALSE,
                                    cells = FALSE, followups = FALSE, refs = character()) {
    .jr_addon_add_result_if_missing(self, "jReportApaTable", jmvcore::Table$new(
        options = self$options, name = "jReportApaTable",
        title = "APA Results Summary (jReport)",
        columns = list(
            list(name = "analysis", title = "Analysis", type = "text"),
            list(name = "test", title = "Test / Effect", type = "text"),
            list(name = "statistic", title = "Statistic", type = "number"),
            list(name = "df1", title = "df1", type = "number"),
            list(name = "df2", title = "df2", type = "text"),
            list(name = "p", title = "p", type = "number", format = "zto,pvalue"),
            list(name = "p_adjusted", title = "Adjusted p", type = "number", format = "zto,pvalue", visible = FALSE),
            list(name = "adjustment_result", title = "Result after adjustment", type = "text", visible = FALSE),
            list(name = "effect", title = "Effect Size", type = "text", visible = FALSE),
            list(name = "ci", title = "Effect 95% CI", type = "text", visible = FALSE)
        )
    ))
    .jr_addon_add_result_if_missing(self, "jReportAssumptions", jmvcore::Table$new(
        options = self$options, name = "jReportAssumptions",
        title = "Assumptions and Recommended Actions (jReport)",
        columns = list(
            list(name = "analysis", title = "Analysis", type = "text"),
            list(name = "assumption", title = "Assumption / Check", type = "text"),
            list(name = "tested", title = "Tested?", type = "text"),
            list(name = "statistic", title = "Statistic", type = "number"),
            list(name = "p", title = "p", type = "number", format = "zto,pvalue"),
            list(name = "met", title = "Met?", type = "text"),
            list(name = "interpretation", title = "What This Means", type = "text", visible = FALSE),
            list(name = "action", title = "Recommended Action", type = "text", visible = FALSE)
        )
    ))
    if (isTRUE(posthoc))
        .jr_addon_add_result_if_missing(self, "jReportPostHoc", jmvcore::Table$new(
            options = self$options, name = "jReportPostHoc",
            title = "APA Post Hoc Comparisons (jReport)", visible = FALSE,
            columns = list(
                list(name = "analysis", title = "Analysis", type = "text"),
                list(name = "term", title = "Factor / Term", type = "text"),
                list(name = "comparison", title = "Comparison", type = "text"),
                list(name = "mean_difference", title = "Mean Difference", type = "number"),
                list(name = "se", title = "SE", type = "number"),
                list(name = "df", title = "df", type = "number"),
                list(name = "statistic", title = "t", type = "number"),
                list(name = "p", title = "Adjusted p", type = "number", format = "zto,pvalue"),
                list(name = "adjustment", title = "Adjustment", type = "text"),
                list(name = "significant", title = "Significant?", type = "text")
            )
        ))
    if (isTRUE(coefficients))
        .jr_addon_add_result_if_missing(self, "jReportCoefficients", jmvcore::Table$new(
            options = self$options, name = "jReportCoefficients",
            title = "Odds Ratios and Coefficients (jReport)", visible = FALSE,
            columns = list(
                list(name = "predictor", title = "Predictor", type = "text"),
                list(name = "estimate", title = "B", type = "number"),
                list(name = "se", title = "SE", type = "number"),
                list(name = "statistic", title = "z", type = "number"),
                list(name = "p", title = "p", type = "number", format = "zto,pvalue"),
                list(name = "odds_ratio", title = "Odds Ratio", type = "number"),
                list(name = "confidence_interval", title = "Odds Ratio 95% CI", type = "text")
            )
        ))
    if (isTRUE(cells))
        .jr_addon_add_result_if_missing(self, "jReportCells", jmvcore::Table$new(
            options = self$options, name = "jReportCells",
            title = "Observed and Expected Counts (jReport)", visible = FALSE,
            columns = list(
                list(name = "analysis", title = "Analysis", type = "text"),
                list(name = "category", title = "Cell / Category", type = "text"),
                list(name = "observed", title = "Observed", type = "number"),
                list(name = "expected", title = "Expected", type = "number"),
                list(name = "standardised_residual", title = "Standardised Residual", type = "number")
            )
        ))
    if (isTRUE(followups))
        .jr_addon_add_result_if_missing(self, "jReportFollowUps", jmvcore::Table$new(
            options = self$options, name = "jReportFollowUps",
            title = "MANOVA/MANCOVA Follow-up Analyses (jReport)", visible = FALSE,
            columns = list(
                list(name = "analysis", title = "Analysis", type = "text"),
                list(name = "term", title = "Effect", type = "text"),
                list(name = "outcome", title = "Outcome", type = "text"),
                list(name = "statistic", title = "F", type = "number"),
                list(name = "df1", title = "df1", type = "number"),
                list(name = "df2", title = "df2", type = "number"),
                list(name = "p", title = "p", type = "number", format = "zto,pvalue"),
                list(name = "p_holm", title = "Holm-adjusted p", type = "number", format = "zto,pvalue"),
                list(name = "effect", title = "Partial eta-squared", type = "number")
            )
        ))
}

.jr_addon_insert_card <- function(self, posthoc = FALSE, coefficients = FALSE, cells = FALSE,
                                  followups = FALSE, refs = character()) {
    .jr_addon_enable_library()

    placeholder <- .jr_html_card(
        "Automatic report", "jReport",
        "The report will appear after the standard analysis variables have been selected."
    )
    .jr_addon_add_result_if_missing(
        self, "jReportHeading",
        jmvcore::Html$new(options = self$options, name = "jReportHeading",
            title = "jReport: Automatic Reporting", content = .jr_addon_heading_html())
    )
    .jr_addon_insert_tables(
        self, posthoc = posthoc, coefficients = coefficients,
        cells = cells, followups = followups, refs = refs
    )
    .jr_addon_add_result_if_missing(
        self, "jReportCard",
        jmvcore::Html$new(options = self$options, name = "jReportCard",
            title = "Suggested APA Report (jReport)", content = placeholder)
    )
    .jr_addon_add_result_if_missing(
        self, "jReportInterpretation",
        jmvcore::Html$new(options = self$options, name = "jReportInterpretation",
            title = "Interpretation Guidance (jReport)",
            content = .jr_addon_interpretation_html(list()))
    )
    .jr_addon_add_result_if_missing(
        self, "methodsReferences",
        jmvcore::Html$new(options = self$options, name = "methodsReferences",
            title = "References",
            content = .jr_methods_references_html(keys = refs))
    )
}

.jr_addon_set_card <- function(self, results, note = "", adjustment = "none", alpha = .05) {
    initial_adjustment_info <- .jr_addon_adjustment_info(results, adjustment, alpha)
    if (identical(initial_adjustment_info$family %||% "", "correlation") &&
            initial_adjustment_info$n_valid == 0L) {
        .jr_addon_set_tables(self, results, adjustment = adjustment, alpha = alpha)
        message <- initial_adjustment_info$note %||% ""
        if (!nzchar(message)) {
            message <- paste(
                "None of the requested correlations could be calculated.",
                "Review the selected variables, sample sizes, missing data, and variable variation."
            )
        }
        card <- .jr_addon_get(self$parent$results, "jReportCard")
        if (!is.null(card))
            card$setContent(.jr_html_card("Automatic report", "Correlation results unavailable", message, accent = "#b46c21"))
        interpretation <- .jr_addon_get(self$parent$results, "jReportInterpretation")
        if (!is.null(interpretation))
            interpretation$setContent(.jr_html_card("Interpretation guidance", "Correlation results unavailable", message, accent = "#b46c21"))
        return(invisible(NULL))
    }
    if (!any(vapply(results, inherits, logical(1), what = "edu_analysis"))) {
        .jr_addon_message(self, paste(
            "The analysis could not be completed.",
            "Check that the selected variables have enough complete cases,",
            "appropriate measurement levels, and no singular or perfectly collinear model terms."
        ))
        return(invisible(NULL))
    }
    results <- lapply(results, function(result) {
        if (inherits(result, "edu_analysis"))
            .jr_apply_variable_descriptions(result, self$data)
        else
            result
    })
    .jr_addon_set_tables(self, results, adjustment = adjustment, alpha = alpha)
    report_html <- .jr_addon_report_html(
        results, options = .jr_addon_reporting_options(), note = note,
        adjustment = adjustment, alpha = alpha
    )
    valid_results <- Filter(function(x) inherits(x, "edu_analysis"), results)
    ref_keys <- unique(unlist(lapply(
        valid_results, .jr_text_reference_keys, include_effect_note = TRUE
    )))
    card <- .jr_addon_get(self$parent$results, "jReportCard")
    if (!is.null(card))
        card$setContent(report_html)
    interpretation <- .jr_addon_get(self$parent$results, "jReportInterpretation")
    if (!is.null(interpretation))
        interpretation$setContent(.jr_addon_interpretation_html(
            results, note = note, adjustment = adjustment, alpha = alpha
        ))
    adjustment_info <- .jr_addon_adjustment_info(results, adjustment, alpha)
    ref_keys <- c(ref_keys, .jr_adjustment_reference_keys(adjustment_info))
    ref_keys <- unique(ref_keys)
    methods <- .jr_addon_get(self$parent$results, "methodsReferences")
    if (!is.null(methods))
        methods$setContent(.jr_methods_references_html(keys = ref_keys))
}

.jr_addon_message <- function(self, message) {
    .jr_addon_set_tables(self, list())
    card <- .jr_addon_get(self$parent$results, "jReportCard")
    if (!is.null(card))
        card$setContent(.jr_html_card("Automatic report", "jReport", message, accent = "#b46c21"))
    interpretation <- .jr_addon_get(self$parent$results, "jReportInterpretation")
    if (!is.null(interpretation))
        interpretation$setContent(.jr_html_card("Interpretation guidance", "jReport", message, accent = "#b46c21"))
    methods <- .jr_addon_get(self$parent$results, "methodsReferences")
    if (!is.null(methods))
        methods$setContent(.jr_methods_references_html(keys = "jReport"))
}
