.jr_jamovi_report_args <- function(options) {
    include <- character()
    mapping <- c(
        reportDescriptives = "descriptives",
        reportAssumptions = "assumptions",
        reportStatistic = "test_statistic",
        reportDf = "df",
        reportP = "p",
        reportEffect = "effect_size",
        reportCI = "ci",
        reportInterpretation = "interpretation",
        reportCautions = "cautions"
    )
    for (name in names(mapping)) {
        value <- tryCatch(options[[name]], error = function(e) NULL)
        if (isTRUE(value))
            include <- c(include, mapping[[name]])
    }
    include <- c(include, "test")
    list(
        style = options$reportStyle,
        format = options$reportFormat,
        tone = options$reportTone,
        include = include
    )
}

.jr_jamovi_text <- function(result, options) {
    args <- .jr_jamovi_report_args(options)
    do.call(edu_report, c(list(x = result), args))
}

.jr_variable_display_name <- function(data, variable) {
    if (is.null(data) || length(variable) != 1L || !variable %in% names(data))
        return(variable)
    values <- data[[variable]]
    description <- attr(values, "description", exact = TRUE)
    if (is.null(description) || !length(description) || !nzchar(trimws(as.character(description)[1])))
        return(variable)
    description <- trimws(as.character(description)[1])
    if (identical(description, variable))
        return(variable)
    description
}

.jr_variable_display_labels <- function(data) {
    if (is.null(data) || is.null(names(data)))
        return(stats::setNames(character(), character()))
    stats::setNames(
        vapply(names(data), function(variable) .jr_variable_display_name(data, variable), character(1)),
        names(data)
    )
}

.jr_regex_escape <- function(text) {
    gsub("([][{}()+*^$|\\\\?.])", "\\\\\\1", text)
}

.jr_replace_variable_names <- function(text, labels) {
    if (!is.character(text) || !length(labels))
        return(text)
    variables <- names(labels)[labels != names(labels) & nzchar(labels)]
    variables <- variables[order(nchar(variables), decreasing = TRUE)]
    for (variable in variables) {
        pattern <- paste0(
            "(?<![[:alnum:]_.])", .jr_regex_escape(variable),
            "(?![[:alnum:]_.])"
        )
        text <- gsub(pattern, labels[[variable]], text, perl = TRUE)
    }
    text
}

.jr_apply_variable_descriptions <- function(result, data) {
    if (!inherits(result, "edu_analysis"))
        return(result)
    labels <- .jr_variable_display_labels(data)
    if (!any(labels != names(labels)))
        return(result)
    replace <- function(value) .jr_replace_variable_names(value, labels)
    for (field in c("question", "requirements", "interpretation", "caution"))
        result[[field]] <- replace(result[[field]])
    result$report_blocks <- lapply(result$report_blocks, replace)
    for (field in c("main", "descriptives", "effects", "diagnostics", "statistics",
                    "parameters", "cells", "posthoc_report", "followups")) {
        table <- result[[field]]
        if (!is.data.frame(table))
            next
        character_columns <- vapply(table, is.character, logical(1))
        table[character_columns] <- lapply(table[character_columns], replace)
        result[[field]] <- table
    }
    result$variable_labels <- labels
    result
}

.jr_expected_ratio_values <- function(ratios) {
    if (is.null(ratios) || length(ratios) == 0L)
        return(NULL)
    values <- vapply(seq_along(ratios), function(i) {
        value <- ratios[[i]]
        if (is.list(value)) {
            if (is.null(value$ratio))
                return(NA_real_)
            value <- value$ratio
        }
        value <- suppressWarnings(as.numeric(value))
        if (length(value) == 0L || !is.finite(value[1]))
            return(NA_real_)
        value[1]
    }, numeric(1))
    if (anyNA(values))
        return(NULL)
    values
}

.jr_html_escape <- function(text) {
    text <- gsub("&", "&amp;", text, fixed = TRUE)
    text <- gsub("<", "&lt;", text, fixed = TRUE)
    text <- gsub(">", "&gt;", text, fixed = TRUE)
    text <- gsub("\"", "&quot;", text, fixed = TRUE)
    text
}

.jr_html_paragraphs <- function(text) {
    parts <- strsplit(.jr_html_escape(text), "\n\n", fixed = TRUE)[[1]]
    paste0("<p style='margin:0 0 10px 0; line-height:1.45;'>", parts, "</p>", collapse = "")
}

.jr_html_card <- function(eyebrow, title, content, accent = "#237f86") {
    sprintf(
        paste0(
            "<div style='border:1px solid #dfe6ea; border-left:5px solid %s;",
            "border-radius:6px; padding:14px 16px; margin:4px 0 10px 0; background:#fbfcfd;'>",
            "<div style='font-size:11px; font-weight:600; letter-spacing:.08em; color:#536472;",
            "text-transform:uppercase; margin-bottom:6px;'>%s</div>",
            "<div style='font-size:16px; font-weight:600; color:#18242d; margin-bottom:10px;'>%s</div>",
            "%s</div>"
        ),
        accent, .jr_html_escape(eyebrow), .jr_html_escape(title), .jr_html_paragraphs(content)
    )
}

.jr_jamovi_overview_html <- function(result) {
    .jr_html_card(
        "jReport", "Guided report controls are available here",
        paste(
            "This output was generated by jReport. Use the Reporting controls in this jReport panel to choose style, format, tone, and included content.",
            "Simplified report-style output is available only for supported analyses and will appear when those analyses are run from their respective jamovi analysis menus.",
            paste(result$question, result$requirements, sep = "\n\n"),
            sep = "\n\n"
        )
    )
}

.jr_jamovi_report_html <- function(result, options) {
    .jr_html_card(
        "Copy-ready reporting", "Report text",
        .jr_jamovi_text(result, options), accent = "#4b66a2"
    )
}

.jr_jamovi_interpretation_html <- function(result) {
    content <- result$interpretation
    accent <- "#278058"
    if (nzchar(result$caution)) {
        content <- paste(content, result$caution, sep = "\n\n")
        accent <- "#b46c21"
    }
    .jr_html_card("Interpretation", "What does this mean?", content, accent = accent)
}

.jr_addon_report_html <- function(results, options = NULL, title = "Guided report", note = "") {
    failures <- Filter(function(x) inherits(x, "try-error"), results)
    results <- Filter(function(x) inherits(x, "edu_analysis"), results)
    if (length(results) == 0L && length(failures) > 0L) {
        message <- sub("^Error[^:]*:\\s*", "", as.character(failures[[1]]))
        return(.jr_html_card(
            "Automatic report", "Report could not be generated",
            paste("jReport received the analysis variables but encountered a calculation problem:", message),
            accent = "#b46c21"
        ))
    }
    if (length(results) == 0L)
        return(.jr_html_card("Report add-on", title, "Select valid analysis variables to generate report text."))
    cards <- vapply(results, function(result) {
        if (is.null(options)) {
            text <- edu_report(result, style = "apa7", format = "paragraph")
        } else {
            text <- .jr_jamovi_text(result, options)
        }
        .jr_html_card("Report add-on", result$label, text, accent = "#4b66a2")
    }, character(1))
    include_note <- is.null(options) ||
        isTRUE(tryCatch(options$reportCautions, error = function(e) FALSE))
    if (nzchar(note) && include_note) {
        cards <- c(
            cards,
            .jr_html_card("Check before reporting", "Model alignment", note, accent = "#b46c21")
        )
    }
    posthoc_text <- .jr_addon_posthoc_text(results)
    if (nzchar(posthoc_text)) {
        cards <- c(
            cards,
            .jr_html_card("Follow-up comparisons", "Post hoc interpretation", posthoc_text, accent = "#4b66a2")
        )
    }
    paste(cards, collapse = "")
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
        reportStyle = "apa7",
        reportFormat = "paragraph",
        reportTone = "student_friendly",
        reportDescriptives = TRUE,
        reportAssumptions = TRUE,
        reportStatistic = TRUE,
        reportDf = TRUE,
        reportP = TRUE,
        reportEffect = TRUE,
        reportCI = TRUE,
        reportInterpretation = TRUE,
        reportCautions = TRUE
    )
}

.jr_parent_ci <- function(parent) {
    value <- tryCatch(parent$options$ciWidth, error = function(e) NULL)
    if (is.null(value) || !is.finite(value))
        .95
    else value / 100
}

.jr_parent_model_formula <- function(outcome, fallback_terms, blocks = NULL) {
    model_terms <- character()
    if (!is.null(blocks)) {
        model_terms <- unlist(lapply(blocks, function(block) {
            if (is.null(block) || length(block) == 0L)
                return(character())
            vapply(block, function(term) paste(term, collapse = ":"), character(1))
        }), use.names = FALSE)
    }
    if (length(model_terms) == 0L)
        model_terms <- fallback_terms
    stats::reformulate(unique(model_terms), response = outcome)
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
    if (identical(result$analysis, "reliability_omega"))
        return(benchmark(sprintf("Omega = %s", .jr_num(statistic$estimate, 2L, TRUE)), statistic$estimate))
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

.jr_addon_apa_rows <- function(results) {
    results <- Filter(function(x) inherits(x, "edu_analysis"), results)
    if (length(results) == 0L)
        return(data.frame())
    rows <- lapply(results, function(result) {
        statistics <- result$statistics
        do.call(rbind, lapply(seq_len(nrow(statistics)), function(i) {
            statistic <- statistics[i, , drop = FALSE]
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
                analysis = result$label,
                test = .jr_addon_test_label(result, i),
                statistic = as.numeric(value),
                df1 = as.numeric(df1),
                df2 = if (is.finite(as.numeric(df2))) .jr_num(df2, 2L) else "",
                p = if ("p" %in% names(statistic)) as.numeric(statistic$p) else NA_real_,
                effect = .jr_addon_effect_label(result, i),
                ci = if (is.finite(lower) && is.finite(upper))
                    .jr_ci(lower, upper, 2L, TRUE)
                else "",
                stringsAsFactors = FALSE
            )
        }))
    })
    do.call(rbind, rows)
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
    results <- Filter(function(x) inherits(x, "edu_analysis"), results)
    if (length(results) == 0L)
        return(data.frame())
    rows <- lapply(results, function(result) {
        diagnostics <- .jr_normalize_diagnostics(result$diagnostics)
        data.frame(
            analysis = result$label,
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

.jr_addon_insert_tables <- function(self, posthoc = FALSE, coefficients = FALSE, cells = FALSE,
                                    followups = FALSE) {
    self$parent$results$add(jmvcore::Table$new(
        options = self$options,
        name = "jReportApaTable",
        title = "APA Results Summary (jReport)",
        columns = list(
            list(name = "analysis", title = "Analysis", type = "text"),
            list(name = "test", title = "Test / Effect", type = "text"),
            list(name = "statistic", title = "Statistic", type = "number"),
            list(name = "df1", title = "df1", type = "number"),
            list(name = "df2", title = "df2", type = "text"),
            list(name = "p", title = "p", type = "number", format = "zto,pvalue"),
            list(name = "effect", title = "Effect Size", type = "text"),
            list(name = "ci", title = "Effect 95% CI", type = "text")
        )
    ))
    self$parent$results$add(jmvcore::Table$new(
        options = self$options,
        name = "jReportAssumptions",
        title = "Assumptions and Recommended Actions (jReport)",
        columns = list(
            list(name = "analysis", title = "Analysis", type = "text"),
            list(name = "assumption", title = "Assumption / Check", type = "text"),
            list(name = "tested", title = "Tested?", type = "text"),
            list(name = "statistic", title = "Statistic", type = "number"),
            list(name = "p", title = "p", type = "number", format = "zto,pvalue"),
            list(name = "met", title = "Met?", type = "text"),
            list(name = "interpretation", title = "What This Means", type = "text"),
            list(name = "action", title = "Recommended Action", type = "text")
        )
    ))
    if (isTRUE(posthoc)) {
        self$parent$results$add(jmvcore::Table$new(
            options = self$options,
            name = "jReportPostHoc",
            title = "APA Post Hoc Comparisons (jReport)",
            visible = FALSE,
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
    }
    if (isTRUE(coefficients)) {
        self$parent$results$add(jmvcore::Table$new(
            options = self$options,
            name = "jReportCoefficients",
            title = "Odds Ratios and Coefficients (jReport)",
            visible = FALSE,
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
    }
    if (isTRUE(cells)) {
        self$parent$results$add(jmvcore::Table$new(
            options = self$options,
            name = "jReportCells",
            title = "Observed and Expected Counts (jReport)",
            visible = FALSE,
            columns = list(
                list(name = "analysis", title = "Analysis", type = "text"),
                list(name = "category", title = "Cell / Category", type = "text"),
                list(name = "observed", title = "Observed", type = "number"),
                list(name = "expected", title = "Expected", type = "number"),
                list(name = "standardised_residual", title = "Standardised Residual", type = "number")
            )
        ))
    }
    if (isTRUE(followups)) {
        self$parent$results$add(jmvcore::Table$new(
            options = self$options,
            name = "jReportFollowUps",
            title = "MANOVA/MANCOVA Follow-up Analyses (jReport)",
            visible = FALSE,
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
}

.jr_addon_set_tables <- function(self, results) {
    apa_table <- self$parent$results$get("jReportApaTable")
    assumption_table <- self$parent$results$get("jReportAssumptions")
    apa_table$deleteRows()
    assumption_table$deleteRows()
    apa_rows <- .jr_addon_apa_rows(results)
    if (nrow(apa_rows) > 0L) {
        for (i in seq_len(nrow(apa_rows)))
            apa_table$addRow(rowKey = i, values = as.list(apa_rows[i, ]))
    }
    assumption_rows <- .jr_addon_assumption_rows(results)
    if (nrow(assumption_rows) > 0L) {
        for (i in seq_len(nrow(assumption_rows)))
            assumption_table$addRow(rowKey = i, values = as.list(assumption_rows[i, ]))
    }
    posthoc_table <- tryCatch(
        self$parent$results$get("jReportPostHoc"),
        error = function(e) NULL
    )
    if (!is.null(posthoc_table)) {
        posthoc_table$deleteRows()
        posthoc_rows <- .jr_addon_posthoc_rows(results)
        posthoc_table$setVisible(nrow(posthoc_rows) > 0L)
        if (nrow(posthoc_rows) > 0L) {
            for (i in seq_len(nrow(posthoc_rows)))
                posthoc_table$addRow(rowKey = i, values = as.list(posthoc_rows[i, ]))
        }
    }
    coefficient_table <- tryCatch(
        self$parent$results$get("jReportCoefficients"),
        error = function(e) NULL
    )
    if (!is.null(coefficient_table)) {
        coefficient_table$deleteRows()
        coefficient_rows <- .jr_addon_coefficient_rows(results)
        coefficient_table$setVisible(nrow(coefficient_rows) > 0L)
        if (nrow(coefficient_rows) > 0L) {
            for (i in seq_len(nrow(coefficient_rows)))
                coefficient_table$addRow(rowKey = i, values = as.list(coefficient_rows[i, ]))
        }
    }
    cell_table <- tryCatch(
        self$parent$results$get("jReportCells"),
        error = function(e) NULL
    )
    if (!is.null(cell_table)) {
        cell_table$deleteRows()
        cell_rows <- .jr_addon_cell_rows(results)
        cell_table$setVisible(nrow(cell_rows) > 0L)
        if (nrow(cell_rows) > 0L) {
            for (i in seq_len(nrow(cell_rows)))
                cell_table$addRow(rowKey = i, values = as.list(cell_rows[i, ]))
        }
    }
    followup_table <- tryCatch(
        self$parent$results$get("jReportFollowUps"),
        error = function(e) NULL
    )
    if (!is.null(followup_table)) {
        followup_table$deleteRows()
        followup_rows <- .jr_addon_followup_rows(results)
        followup_table$setVisible(nrow(followup_rows) > 0L)
        if (nrow(followup_rows) > 0L) {
            for (i in seq_len(nrow(followup_rows)))
                followup_table$addRow(rowKey = i, values = as.list(followup_rows[i, ]))
        }
    }
}

.jr_addon_insert_card <- function(self, posthoc = FALSE, coefficients = FALSE, cells = FALSE,
                                  followups = FALSE) {
    .jr_addon_enable_library()
    heading <- jmvcore::Html$new(
        options = self$options,
        name = "jReportHeading",
        title = "jReport: Automatic Reporting",
        content = .jr_addon_heading_html()
    )
    self$parent$results$add(heading)
    .jr_addon_insert_tables(
        self, posthoc = posthoc, coefficients = coefficients,
        cells = cells, followups = followups
    )
    card <- jmvcore::Html$new(
        options = self$options,
        name = "jReportCard",
        title = "Automatic Report (jReport)",
        content = .jr_html_card(
            "Automatic report", "jReport",
            "The report will appear after the standard analysis variables have been selected."
        )
    )
    self$parent$results$add(card)
}

.jr_addon_set_card <- function(self, results, note = "") {
    results <- lapply(results, function(result) {
        .jr_apply_variable_descriptions(result, self$data)
    })
    .jr_addon_set_tables(self, results)
    self$parent$results$get("jReportCard")$setContent(
        .jr_addon_report_html(results, options = .jr_addon_reporting_options(), note = note)
    )
}

.jr_addon_message <- function(self, message) {
    .jr_addon_set_tables(self, list())
    self$parent$results$get("jReportCard")$setContent(
        .jr_html_card("Automatic report", "jReport", message, accent = "#b46c21")
    )
}

.jr_guided_error_message <- function(error) {
    original <- conditionMessage(error)
    if (grepl("logistic regression requires an outcome with exactly two levels", original, ignore.case = TRUE))
        return("Logistic regression requires a binary outcome variable.")
    paste(
        "The analysis could not be completed.",
        "Check that the selected variables have enough complete cases, appropriate measurement levels, and no singular or perfectly collinear model terms."
    )
}

.jr_guided_computation <- function(expr, code = "analysisFailed") {
    tryCatch(
        force(expr),
        error = function(e) jmvcore::reject(.jr_guided_error_message(e), code = code)
    )
}

.jr_diagnostic_row_values <- function(diagnostics, i) {
    list(
        check = diagnostics$check[i],
        tested = diagnostics$tested[i],
        statistic = diagnostics$statistic[i],
        p = diagnostics$p[i],
        status = diagnostics$status[i],
        interpretation = diagnostics$interpretation[i],
        action = diagnostics$action[i]
    )
}

.jr_prefill_diagnostic_rows <- function(table, n) {
    existing <- table$rowKeys
    for (i in seq_len(n)) {
        key <- as.character(i)
        if (!key %in% existing) {
            table$addRow(rowKey = i, values = list(
                check = "",
                tested = "",
                statistic = NA_real_,
                p = NA_real_,
                status = "",
                interpretation = "",
                action = ""
            ))
            existing <- table$rowKeys
        }
    }
}

.jr_populate_diagnostics <- function(table, diagnostics, fixed = FALSE) {
    diagnostics <- .jr_normalize_diagnostics(diagnostics)
    if (isTRUE(fixed))
        .jr_prefill_diagnostic_rows(table, nrow(diagnostics))
    for (i in seq_len(nrow(diagnostics))) {
        values <- .jr_diagnostic_row_values(diagnostics, i)
        if (isTRUE(fixed))
            table$setRow(rowKey = i, values = values)
        else
            table$addRow(rowKey = i, values = values)
    }
}
