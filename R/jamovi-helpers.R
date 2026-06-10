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
    parts <- strsplit(text, "\n\n", fixed = TRUE)[[1]]
    rendered <- vapply(parts, function(part) {
        part <- trimws(part)
        is_note <- grepl("^\\*Interpretation note:", part) && grepl("\\*$", part)
        if (is_note)
            part <- sub("^\\*", "", sub("\\*$", "", part))
        part <- gsub("\n", "<br>", .jr_html_escape(part), fixed = TRUE)
        if (is_note) {
            return(sprintf(
                paste0(
                    "<div style='border:1px solid #d9e3f2; border-left:4px solid #4b66a2;",
                    "border-radius:4px; background:#f4f7fb; color:#25364a;",
                    "padding:10px 12px; margin:8px 0 12px 0; line-height:1.45;'>%s</div>"
                ),
                part
            ))
        }
        sprintf("<p style='margin:0 0 10px 0; line-height:1.45;'>%s</p>", part)
    }, character(1))
    paste0(rendered, collapse = "")
}

.jr_html_card <- function(eyebrow, title, content = "", accent = "#237f86",
                          content_html = NULL) {
    body <- if (is.null(content_html)) .jr_html_paragraphs(content) else content_html
    sprintf(
        paste0(
            "<div style='width:100%%;box-sizing:border-box;border:1px solid #dfe6ea;border-left:5px solid %s;",
            "border-radius:6px;padding:14px 16px;margin:4px 0 10px 0;background:#fbfcfd;'>",
            "<div style='font-size:11px;font-weight:600;letter-spacing:.08em;color:#536472;",
            "text-transform:uppercase;margin-bottom:6px;'>%s</div>",
            "<div style='font-size:16px;font-weight:600;color:#18242d;margin-bottom:10px;'>%s</div>",
            "%s</div>"
        ),
        accent, .jr_html_escape(eyebrow), .jr_html_escape(title), body
    )
}

.jr_report_card <- function(eyebrow, title, subtitle = "", content = "",
                            accent = "#237f86", background = "#fbfcfd",
                            content_html = NULL) {
    subtitle_html <- ""
    if (nzchar(subtitle)) {
        subtitle_html <- sprintf(
            "<div style='font-size:13px;font-weight:600;color:#536472;margin:-3px 0 10px 0;'>%s</div>",
            .jr_html_escape(subtitle)
        )
    }
    body <- if (is.null(content_html)) .jr_html_paragraphs(content) else content_html
    sprintf(
        paste0(
            "<div style='width:100%%;box-sizing:border-box;border:1px solid #dfe6ea;border-left:5px solid %s;",
            "border-radius:6px;padding:14px 16px;margin:4px 0 12px 0;background:%s;'>",
            "<div style='font-size:11px;font-weight:700;letter-spacing:0;color:#536472;",
            "text-transform:uppercase;margin-bottom:6px;'>%s</div>",
            "<div style='font-size:17px;font-weight:700;color:#18242d;margin-bottom:8px;'>%s</div>",
            "%s%s</div>"
        ),
        accent, background, .jr_html_escape(eyebrow), .jr_html_escape(title),
        subtitle_html, body
    )
}

.jr_html_bullets <- function(items) {
    items <- items[nzchar(items)]
    if (!length(items))
        return("")
    rows <- paste0(
        "<li style='margin:0 0 7px 0; padding-left:2px;'>",
        .jr_html_escape(items),
        "</li>",
        collapse = ""
    )
    paste0("<ul style='margin:0; padding-left:22px; line-height:1.45;'>", rows, "</ul>")
}

.jr_html_numbered <- function(items) {
    items <- items[nzchar(items)]
    if (!length(items))
        return("")
    rows <- paste0(
        "<li style='margin:0 0 8px 0; padding-left:2px;'>",
        .jr_html_escape(items),
        "</li>",
        collapse = ""
    )
    paste0("<ol style='margin:0; padding-left:22px; line-height:1.45;'>", rows, "</ol>")
}

.jr_build_report_cards_html <- function(analysis_title, copy_ready_text,
                                        diagnostic_text = NULL,
                                        guidance_text = NULL,
                                        checklist_items = NULL,
                                        references_text = NULL,
                                        checklist_note = "") {
    cards <- .jr_report_card(
        "Copy-ready report text", analysis_title,
        "Select and copy this paragraph into your report.",
        copy_ready_text, accent = "#278058", background = "#f6fbf8"
    )
    if (!is.null(diagnostic_text) && nzchar(diagnostic_text)) {
        cards <- paste0(cards, .jr_report_card(
            "Optional assumptions / diagnostic note", "Assumptions and diagnostics",
            "Include this only if relevant to your study.",
            diagnostic_text, accent = "#2f6fa3", background = "#f5f9fd"
        ))
    }
    if (!is.null(guidance_text) && nzchar(guidance_text)) {
        cards <- paste0(cards, .jr_report_card(
            "Interpretation guidance", "How to read this result",
            "For understanding only - do not copy directly.",
            guidance_text, accent = "#b46c21", background = "#fff9ef"
        ))
    }
    checklist_html <- .jr_html_bullets(checklist_items %||% character())
    if (nzchar(checklist_note)) {
        checklist_html <- paste0(
            checklist_html,
            "<div style='border-top:1px solid #dfe6ea;margin-top:12px;padding-top:10px;'>",
            .jr_html_paragraphs(checklist_note),
            "</div>"
        )
    }
    if (nzchar(checklist_html)) {
        cards <- paste0(cards, .jr_report_card(
            "Check before reporting", "Verification checklist",
            "", accent = "#6d5a8a", background = "#faf8fc",
            content_html = checklist_html
        ))
    }
    if (!is.null(references_text) && length(references_text) > 0L) {
        ref_html <- .jr_references_html_from_entries(references_text)
        if (nzchar(ref_html))
            cards <- paste0(cards, ref_html)
    }
    cards
}

.jr_reference_entries <- function(results, include_effect_note = TRUE) {
    keys <- unique(unlist(lapply(results, .jr_text_reference_keys, include_effect_note = include_effect_note)))
    entries <- vapply(keys, .jr_reference_entry_text, character(1))
    entries[nzchar(entries)]
}

.jr_report_section_card <- function(title, subtitle = "", content = "",
                                    accent = "#237f86", background = "#fbfcfd",
                                    content_html = NULL) {
    subtitle_html <- ""
    if (nzchar(subtitle)) {
        subtitle_html <- sprintf(
            "<div style='font-size:13px;font-weight:600;color:#536472;margin:-2px 0 10px 0;'>%s</div>",
            .jr_html_escape(subtitle)
        )
    }
    body <- if (is.null(content_html)) .jr_html_paragraphs(content) else content_html
    sprintf(
        paste0(
            "<div style='width:100%%;box-sizing:border-box;border:1px solid #dfe6ea;border-left:5px solid %s;",
            "border-radius:6px;padding:14px 16px;margin:4px 0 12px 0;background:%s;'>",
            "<div style='font-size:17px;font-weight:700;color:#18242d;margin-bottom:8px;'>%s</div>",
            "%s%s</div>"
        ),
        accent, background, .jr_html_escape(title), subtitle_html, body
    )
}

.jr_build_report_sections_html <- function(apa_wording = NULL,
                                           diagnostic_note = NULL,
                                           interpretation_guidance = NULL,
                                           checklist_items = NULL,
                                           checklist_note = "",
                                           references = NULL) {
    sections <- character()
    if (!is.null(apa_wording) && nzchar(apa_wording)) {
        sections <- c(sections, .jr_report_section_card(
            "Suggested APA-style report wording",
            "This is suggested wording only. Check all values against your jamovi output and adapt the text for your own study before using it.",
            apa_wording, accent = "#278058", background = "#f6fbf8"
        ))
    }
    if (!is.null(diagnostic_note) && nzchar(diagnostic_note)) {
        sections <- c(sections, .jr_report_section_card(
            "Optional assumptions / diagnostic note",
            "Include this only if relevant to your study.",
            diagnostic_note, accent = "#2f6fa3", background = "#f5f9fd"
        ))
    }
    if (!is.null(interpretation_guidance) && nzchar(interpretation_guidance)) {
        sections <- c(sections, .jr_report_section_card(
            "Interpretation guidance",
            "For understanding only - do not copy directly into your report.",
            interpretation_guidance, accent = "#b46c21", background = "#fff9ef"
        ))
    }
    checklist_html <- .jr_html_bullets(checklist_items %||% character())
    if (nzchar(checklist_note))
        checklist_html <- paste0(checklist_html, .jr_html_paragraphs(checklist_note))
    if (nzchar(checklist_html)) {
        sections <- c(sections, .jr_report_section_card(
            "Check before reporting",
            "", accent = "#6d5a8a", background = "#faf8fc",
            content_html = checklist_html
        ))
    }
    if (!is.null(references) && length(references) > 0L) {
        ref_html <- .jr_references_html_from_entries(references)
        if (nzchar(ref_html))
            sections <- c(sections, ref_html)
    }
    paste(sections, collapse = "")
}

.jr_reference_entry_text <- function(key) {
    refs <- tryCatch(get(".jmvrefs", envir = asNamespace("jReport")), error = function(e) NULL)
    ref <- refs[[key]]
    if (is.null(ref)) {
        return(switch(
            key,
            Cohen1988 = "Cohen, J. (1988). Statistical power analysis for the behavioral sciences (2nd ed.). Lawrence Erlbaum Associates.",
            Cumming2014 = "Cumming, G. (2014). The new statistics: Why and how. Psychological Science, 25(1), 7-29.",
            ""
        ))
    }
    author <- ref$author %||% ""
    year <- ref$year %||% "n.d."
    title <- ref$title %||% key
    publisher <- ref$publisher %||% ""
    url <- ref$url %||% ""
    tail <- paste(c(publisher, url), collapse = ". ")
    tail <- sub("\\. $", "", tail)
    if (nzchar(tail))
        sprintf("%s (%s). %s. %s.", author, year, title, tail)
    else
        sprintf("%s (%s). %s.", author, year, title)
}

.jr_text_reference_keys <- function(result, include_effect_note = TRUE) {
    analysis_keys <- switch(
        result$analysis,
        ttest = c("effectsize", "ggplot2", "BayesFactor"),
        bayes_ttest = c("BayesFactor"),
        anova_oneway = c("afex", "effectsize", "emmeans"),
        anova_between = c("afex", "effectsize", "emmeans"),
        anova_rm = c("afex", "effectsize"),
        anova_mixed = c("afex", "effectsize"),
        ancova = c("car", "effectsize", "emmeans"),
        manova = c("car", "effectsize"),
        correlation = c("effectsize", "ggplot2"),
        regression = c("parameters", "performance", "effectsize", "ggplot2"),
        logistic_regression = c("parameters", "performance", "effectsize", "ggplot2"),
        chisq_independence = c("effectsize", "ggplot2"),
        chisq_gof = c("effectsize", "ggplot2"),
        reliability_omega = c("psych", "McDonald1999", "RevelleCondon2019", "ggplot2"),
        character(0)
    )
    keys <- c("jmvcore", "RCore", "jReport", analysis_keys)
    if (isTRUE(include_effect_note) && !identical(result$analysis, "reliability_omega"))
        keys <- c(keys, "Cohen1988", "Cumming2014")
    unique(keys)
}

.jr_references_html_from_entries <- function(entries) {
    entries <- entries[nzchar(entries)]
    if (!length(entries))
        return("")
    rows <- paste0(
        "<li style='margin:0 0 8px 0;padding-left:2px;'>",
        .jr_html_escape(entries),
        "</li>",
        collapse = ""
    )
    paste0(
        "<div style='width:100%;box-sizing:border-box;border:1px solid #dfe6ea;border-left:5px solid #536472;",
        "border-radius:6px;padding:14px 16px;margin:4px 0 12px 0;background:#f9fafb;'>",
        "<div style='font-size:11px;font-weight:700;letter-spacing:0;color:#536472;",
        "text-transform:uppercase;margin-bottom:6px;'>References</div>",
        "<ol style='margin:0;padding-left:22px;line-height:1.55;font-size:13px;color:#25364a;'>",
        rows,
        "</ol></div>"
    )
}

.jr_references_html <- function(results, include_effect_note = TRUE) {
    entries <- .jr_reference_entries(results, include_effect_note = include_effect_note)
    .jr_references_html_from_entries(entries)
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
    text <- .jr_jamovi_text(result, options)
    include_effect <- isTRUE(tryCatch(options$reportEffect, error = function(e) TRUE))
    refs_html <- .jr_references_html(list(result), include_effect_note = include_effect)
    body_html <- paste0(.jr_html_paragraphs(text), refs_html)
    .jr_html_card(
        "Copy-ready reporting", "Report text",
        accent = "#4b66a2", content_html = body_html
    )
}

.jr_jamovi_interpretation_html <- function(result) {
    content <- result$interpretation
    accent <- "#278058"
    if (nzchar(result$caution)) {
        content <- paste(content, result$caution, sep = "\n\n")
        accent <- "#b46c21"
    }
    rm_guidance <- .jr_rm_ges_guidance(result)
    if (nzchar(rm_guidance))
        content <- paste(content, rm_guidance, sep = "\n\n")
    .jr_html_card("Interpretation", "What does this mean?", content, accent = accent)
}

.jr_nonempty_unique <- function(items) {
    items <- trimws(items[nzchar(items)])
    items[!duplicated(items)]
}

.jr_anova_between_checklist_items <- function() {
    c(
        "The correct dependent variable is reported.",
        "The correct between-subjects factor or factors are reported.",
        "Group labels are correct.",
        "Descriptive statistics match the analysis.",
        "The ANOVA table values are correct.",
        "Degrees of freedom are correct.",
        "F values are correct.",
        "p-values are correct.",
        "Effect sizes are correct.",
        "Confidence intervals are correct, where reported.",
        "Post hoc tests or planned comparisons are described accurately.",
        "Correction methods are named correctly.",
        "Assumptions and diagnostics have been reviewed.",
        "Any assumption violations are reported or justified appropriately.",
        "The interpretation matches the research question.",
        "The wording has been adapted to the user's actual study."
    )
}

.jr_anova_between_guidance_text <- function(result, include, posthoc_text = "") {
    guidance <- .jr_nonempty_unique(c(
        result$report_blocks$rationale %||% "",
        result$report_blocks$descriptives %||% "",
        if ("interpretation" %in% include) result$report_blocks$plain %||% "" else "",
        if (nzchar(posthoc_text))
            paste(
                "Post hoc tests are follow-up comparisons used to identify which group means differ after an omnibus ANOVA effect.",
                "Interpret the named correction method with the post hoc output, because Tukey, Bonferroni, Holm, and Games-Howell control error rates in different ways."
            )
        else "",
        if ("effect_size" %in% include) .jr_effect_benchmark_text(result) else "",
        if ("effect_size" %in% include) .jr_effect_interpretation_note(result$analysis) else "",
        paste(
            "A significant omnibus effect indicates evidence that at least one group mean differs, but it does not by itself identify which groups differ.",
            "A non-significant omnibus effect means the analysis did not provide clear evidence of group mean differences in this sample.",
            "Statistical significance should be interpreted alongside effect size, assumptions, sample size, and the research context."
        )
    ))
    paste(guidance, collapse = "\n\n")
}

.jr_anova_between_diagnostic_text <- function(result, include) {
    diagnostics <- character()
    if ("assumptions" %in% include) {
        rows <- .jr_normalize_diagnostics(result$diagnostics)
        detail <- vapply(seq_len(nrow(rows)), function(i) {
            statistic <- if (is.finite(rows$statistic[i]))
                sprintf(" statistic = %s,", .jr_num(rows$statistic[i]))
            else ""
            p <- if (is.finite(rows$p[i]))
                sprintf(" p %s.", .jr_p(rows$p[i]))
            else "."
            paste(
                sprintf("%s: %s.%s%s", rows$check[i], rows$status[i], statistic, p),
                rows$interpretation[i],
                rows$action[i]
            )
        }, character(1))
        diagnostics <- c(diagnostics, result$report_blocks$assumptions %||% "", detail)
    }
    if ("cautions" %in% include && nzchar(result$caution)) {
        diagnostics <- c(
            diagnostics,
            "Caution: At least one assumption or diagnostic check was flagged for review."
        )
    }
    paste(.jr_nonempty_unique(diagnostics), collapse = "\n\n")
}

.jr_anova_between_call_value <- function(result, name, fallback = "") {
    value <- tryCatch(result$call[[name]], error = function(e) NULL)
    if (is.null(value))
        return(fallback)
    if (is.character(value))
        return(paste(value, collapse = ", "))
    if (is.call(value) && identical(as.character(value[[1]]), "c")) {
        values <- unlist(lapply(as.list(value)[-1], function(item) {
            if (is.character(item)) item else all.vars(item)
        }), use.names = FALSE)
        values <- values[nzchar(values)]
        if (length(values))
            return(paste(values, collapse = ", "))
    }
    variables <- all.vars(value)
    if (length(variables))
        return(paste(variables, collapse = ", "))
    fallback
}

.jr_anova_between_descriptive_sentence <- function(result) {
    descriptives <- result$descriptives
    if (!is.data.frame(descriptives) || nrow(descriptives) == 0L)
        return("")
    descriptives <- descriptives[order(descriptives$mean, decreasing = TRUE), , drop = FALSE]
    group_text <- vapply(seq_len(nrow(descriptives)), function(i) {
        sprintf(
            "%s (M = %s, SD = %s)",
            descriptives$group[i],
            .jr_num(descriptives$mean[i]),
            .jr_num(descriptives$sd[i])
        )
    }, character(1))
    if (length(group_text) == 1L)
        return(sprintf("Descriptive statistics indicated that the group mean was %s.", group_text))
    if (length(group_text) == 2L) {
        return(sprintf(
            "Descriptive statistics indicated that scores were higher in %s than in %s.",
            group_text[1], group_text[2]
        ))
    }
    sprintf(
        "Descriptive statistics indicated that scores were highest in %s, followed by %s, and lowest in %s.",
        group_text[1],
        paste(group_text[seq.int(2L, length(group_text) - 1L)], collapse = ", "),
        group_text[length(group_text)]
    )
}

.jr_anova_between_assumption_sentence <- function(result) {
    diagnostics <- .jr_normalize_diagnostics(result$diagnostics)
    tested <- diagnostics[diagnostics$tested == "Yes", , drop = FALSE]
    relevant <- tested[grepl("normality|homogeneity|levene", tested$check, ignore.case = TRUE), , drop = FALSE]
    if (nrow(relevant) == 0L)
        return("")
    if (all(relevant$status == "Acceptable")) {
        return(
            "Assumption checks did not indicate substantial violations of residual normality or homogeneity of variance."
        )
    }
    ""
}

.jr_anova_between_omega <- function(statistic) {
    omega <- tryCatch(
        as.data.frame(effectsize::F_to_omega2(
            f = statistic$statistic,
            df = statistic$df1,
            df_error = statistic$df2
        )),
        error = function(e) NULL
    )
    if (is.null(omega) || !"Omega2_partial" %in% names(omega))
        return(NA_real_)
    as.numeric(omega$Omega2_partial[1])
}

.jr_anova_between_effect_phrase <- function(statistic, one_way, include_ci = TRUE) {
    eta <- as.numeric(statistic$effect)
    omega <- .jr_anova_between_omega(statistic)
    eta_text <- if (is.finite(eta)) {
        if (isTRUE(include_ci) && is.finite(statistic$ci_low) && is.finite(statistic$ci_high)) {
            sprintf(
                "\u03b7p\u00b2 = %s, 95%% CI %s",
                .jr_num(eta, 2L, TRUE),
                .jr_ci(statistic$ci_low, statistic$ci_high, 2L, TRUE)
            )
        } else {
            sprintf("\u03b7p\u00b2 = %s", .jr_num(eta, 2L, TRUE))
        }
    } else {
        ""
    }
    if (isTRUE(one_way) && is.finite(omega) && nzchar(eta_text)) {
        return(sprintf(
            "The effect size was \u03c9\u00b2 = %s, with %s also provided for comparison",
            .jr_num(omega, 2L, TRUE),
            eta_text
        ))
    }
    if (isTRUE(one_way) && is.finite(omega))
        return(sprintf("The effect size was \u03c9\u00b2 = %s", .jr_num(omega, 2L, TRUE)))
    if (is.finite(omega) && nzchar(eta_text)) {
        return(sprintf(
            "%s, with partial omega squared, \u03c9p\u00b2 = %s, also provided as a less biased estimate",
            eta_text,
            .jr_num(omega, 2L, TRUE)
        ))
    }
    eta_text
}

.jr_anova_between_omnibus_sentences <- function(result, include) {
    statistics <- result$statistics
    if (!is.data.frame(statistics) || nrow(statistics) == 0L)
        return("")
    one_way <- nrow(statistics) == 1L
    include_ci <- "ci" %in% include
    include_effect <- "effect_size" %in% include
    sentences <- vapply(seq_len(nrow(statistics)), function(i) {
        statistic <- statistics[i, , drop = FALSE]
        effect_phrase <- if (include_effect)
            .jr_anova_between_effect_phrase(statistic, one_way, include_ci)
        else ""
        effect <- if (nzchar(effect_phrase)) paste0(", ", effect_phrase) else ""
        conclusion <- if (is.finite(statistic$p) && statistic$p < .05)
            "This provides evidence of mean differences for this effect."
        else
            "This did not provide clear evidence of mean differences for this effect."
        sprintf(
            "%s %s, F(%s, %s) = %s, p %s%s. %s",
            statistic$term,
            if (is.finite(statistic$p) && statistic$p < .05) "was statistically significant" else "was not statistically significant",
            .jr_num(statistic$df1, 2L),
            .jr_num(statistic$df2, 2L),
            .jr_num(statistic$statistic),
            .jr_p(statistic$p),
            effect,
            conclusion
        )
    }, character(1))
    paste(sentences, collapse = " ")
}

.jr_anova_between_apa_text <- function(result, include, posthoc_text = "") {
    outcome <- .jr_anova_between_call_value(result, "outcome", "the dependent variable")
    factors <- .jr_anova_between_call_value(result, "factors", "the between-subjects factor or factors")
    opening <- sprintf(
        "A between-subjects ANOVA examined %s as a function of %s.",
        outcome,
        factors
    )
    parts <- .jr_nonempty_unique(c(
        opening,
        .jr_anova_between_descriptive_sentence(result),
        .jr_anova_between_assumption_sentence(result),
        .jr_anova_between_omnibus_sentences(result, include),
        posthoc_text
    ))
    paste(parts, collapse = " ")
}

.jr_anova_between_report_sections_html <- function(result, options = NULL, note = "",
                                                   posthoc_text = "",
                                                   include_references = TRUE) {
    include <- if (is.null(options)) {
        .jr_addon_reporting_options()
    } else {
        .jr_jamovi_report_args(options)$include
    }
    if (is.list(include))
        include <- .jr_jamovi_report_args(include)$include
    apa <- .jr_anova_between_apa_text(result, include, posthoc_text)
    .jr_build_report_sections_html(
        apa_wording = apa,
        diagnostic_note = .jr_anova_between_diagnostic_text(result, include),
        interpretation_guidance = .jr_anova_between_guidance_text(result, include, posthoc_text),
        checklist_items = .jr_anova_between_checklist_items(),
        checklist_note = note,
        references = .jr_reference_entries(list(result), include_effect_note = "effect_size" %in% include)
    )
}

.jr_regression_guidance_text <- function(result) {
    stats <- result$statistics[1, , drop = FALSE]
    outcome <- tryCatch(
        all.vars(stats::formula(result$model))[1],
        error = function(e) "the outcome"
    )
    fit_text <- sprintf(
        "The F statistic tests whether the set of predictors explains more variation in %s than an intercept-only model. R-squared is the proportion of variation explained by the predictors; adjusted R-squared applies a penalty for the number of predictors and is usually better for comparing models with different numbers of predictors.",
        outcome
    )
    if ("r2" %in% names(stats) && "adjusted_r2" %in% names(stats)) {
        fit_text <- paste(
            fit_text,
            sprintf(
                "Here, R-squared is %s and adjusted R-squared is %s.",
                .jr_num(stats$r2, 2L, TRUE), .jr_num(stats$adjusted_r2, 2L, TRUE)
            )
        )
    }
    coefficient_text <- paste(
        "Each unstandardized coefficient, B, estimates the expected change in the outcome for a one-unit increase in that predictor while the other predictors are held constant.",
        "SE describes uncertainty around B, beta is the standardized coefficient for comparing predictors measured on different scales, t and p test whether the coefficient differs from zero, and the confidence interval shows the plausible range of the coefficient."
    )
    significance_text <- paste(
        "A statistically significant predictor provides evidence of an association with the outcome after adjusting for the other predictors in the model.",
        "A non-significant predictor should usually be described as not showing clear evidence of an adjusted association in this sample, rather than as proof that no relationship exists."
    )
    paste(result$interpretation, fit_text, coefficient_text, significance_text, sep = "\n\n")
}

.jr_regression_checklist_items <- function() {
    c(
        "Outcome variable is the intended dependent variable.",
        "Predictors are the intended covariates and factors.",
        "Categorical predictors are coded with the correct groups and reference levels.",
        "Model fit statistics are copied from the final jamovi output.",
        "R-squared and adjusted R-squared match the final model.",
        "F statistic and degrees of freedom match the final model.",
        "b, SE, beta, t, p, and confidence intervals match the coefficient table.",
        "Assumption checks have been reviewed.",
        "Interpretation matches the research question."
    )
}

.jr_regression_report_cards_html <- function(result, options = NULL, note = "",
                                            include_effect_note = TRUE) {
    copy_ready_text <- result$report_blocks$apa %||% ""
    if (!is.null(options)) {
        include <- .jr_jamovi_report_args(options)$include
        copy_ready_text <- .jr_apply_inclusions(copy_ready_text, result$analysis, include)
    }
    diagnostic_text <- result$report_blocks$assumptions %||% ""
    if (nzchar(result$caution))
        diagnostic_text <- paste(diagnostic_text, result$caution, sep = "\n\n")
    .jr_build_report_cards_html(
        analysis_title = result$label,
        copy_ready_text = copy_ready_text,
        diagnostic_text = diagnostic_text,
        guidance_text = .jr_regression_guidance_text(result),
        checklist_items = .jr_regression_checklist_items(),
        references_text = .jr_reference_entries(list(result), include_effect_note = include_effect_note),
        checklist_note = note
    )
}

.jr_rm_ges_guidance <- function(result) {
    if (!result$analysis %in% c("anova_rm", "anova_mixed"))
        return("")
    stats <- result$statistics
    if (!is.data.frame(stats) || !"ges" %in% names(stats))
        return("")
    ges_vals <- stats$ges[is.finite(stats$ges)]
    eta_vals <- stats$effect[is.finite(stats$effect)]
    if (!length(ges_vals) || !length(eta_vals))
        return("")
    base <- paste(
        "Generalised eta squared (ηG²) estimates the proportion of total variance",
        "explained by an effect and allows comparison across different experimental designs.",
        "Partial eta squared (ηp²) estimates the proportion of variance explained after",
        "accounting for other sources of variance in the model and is therefore often larger."
    )
    max_ges <- max(ges_vals, na.rm = TRUE)
    max_eta <- max(eta_vals, na.rm = TRUE)
    discrepancy <- if (is.finite(max_ges) && is.finite(max_eta) && max_ges > 0 &&
                       max_eta > 2 * max_ges) {
        paste(
            "The difference between ηG² and ηp² suggests that a substantial proportion of",
            "variability is attributable to individual differences between participants.",
            "This pattern is common in repeated measures designs where participant characteristics",
            "account for a large amount of variance."
        )
    } else {
        ""
    }
    if (nzchar(discrepancy))
        paste(base, discrepancy, sep = "\n\n")
    else
        base
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
    include_effect_note <- is.null(options) ||
        isTRUE(tryCatch(options$reportEffect, error = function(e) TRUE))
    if (length(results) == 1L && identical(results[[1]]$analysis, "regression")) {
        return(.jr_regression_report_cards_html(
            results[[1]], options = options, note = note,
            include_effect_note = include_effect_note
        ))
    }
    if (length(results) == 1L && identical(results[[1]]$analysis, "anova_between")) {
        return(.jr_anova_between_report_sections_html(
            results[[1]], options = options, note = note,
            posthoc_text = .jr_addon_posthoc_text(results)
        ))
    }
    include_note <- is.null(options) ||
        isTRUE(tryCatch(options$reportCautions, error = function(e) FALSE))
    include_effect_note <- is.null(options) ||
        isTRUE(tryCatch(options$reportEffect, error = function(e) TRUE))
    posthoc_text <- .jr_addon_posthoc_text(results)
    sections <- vapply(results, function(result) {
        apa_text <- if (is.null(options)) {
            result$report_blocks$apa %||% edu_report(result, style = "apa7", format = "paragraph")
        } else {
            .jr_jamovi_text(result, options)
        }
        diagnostic_text <- result$report_blocks$assumptions %||% ""
        if (nzchar(result$caution %||% ""))
            diagnostic_text <- paste(diagnostic_text, result$caution, sep = if (nzchar(diagnostic_text)) "\n\n" else "")
        guidance_text <- .jr_rm_ges_guidance(result)
        guidance_text <- paste(
            c(
                result$report_blocks$rationale %||% "",
                if (nzchar(guidance_text)) guidance_text else result$interpretation %||% ""
            ),
            collapse = "\n\n"
        )
        checklist <- .jr_analysis_checklist(result$analysis)
        refs <- .jr_reference_entries(list(result), include_effect_note = include_effect_note)
        .jr_build_report_sections_html(
            apa_wording = apa_text,
            diagnostic_note = diagnostic_text,
            interpretation_guidance = guidance_text,
            checklist_items = checklist,
            checklist_note = if (nzchar(note) && include_note) note else "",
            references = refs
        )
    }, character(1))
    if (nzchar(posthoc_text)) {
        sections <- c(sections, .jr_report_section_card(
            "Post hoc interpretation",
            "Follow-up comparisons",
            posthoc_text, accent = "#4b66a2", background = "#f5f9fd"
        ))
    }
    paste(sections, collapse = "")
}

.jr_analysis_checklist <- function(analysis_type) {
    switch(
        analysis_type,
        ttest = c(
            "Group variable has exactly two levels.",
            "Outcome variable is numeric and continuous.",
            "t statistic, degrees of freedom, and p value match jamovi output.",
            "Mean difference and confidence interval match jamovi output.",
            "Cohen's d effect size matches jamovi output.",
            "Assumption checks (normality, variance equality) have been reviewed.",
            "Interpretation matches the research question."
        ),
        correlation = c(
            "Both variables are the intended variables.",
            "Correlation method (Pearson/Spearman/Kendall) matches the analysis.",
            "Correlation coefficient and p value match jamovi output.",
            "Sample size n matches the number of complete pairs.",
            "Scatterplot has been inspected for outliers and non-linearity.",
            "Interpretation matches the research question."
        ),
        chisq_independence = c(
            "Row and column variables are the intended categorical variables.",
            "Chi-square statistic, df, N, and p value match jamovi output.",
            "Cramer's V matches jamovi output.",
            "Expected cell frequencies have been checked (all >= 5).",
            "Interpretation matches the research question."
        ),
        chisq_gof = c(
            "Variable and expected proportions are correctly specified.",
            "Chi-square statistic, df, N, and p value match jamovi output.",
            "Cohen's w effect size matches jamovi output.",
            "Expected cell frequencies have been checked (all >= 5).",
            "Interpretation matches the research question."
        ),
        anova_oneway = c(
            "Outcome variable is numeric and grouping variable has the correct levels.",
            "F statistic, df, and p value match jamovi output.",
            "Effect size (η²) matches jamovi output.",
            "Post hoc comparisons (if run) match jamovi output.",
            "Assumption checks (normality, homogeneity of variance) have been reviewed.",
            "Interpretation matches the research question."
        ),
        ancova = c(
            "Outcome, grouping factor, and covariate(s) are correctly specified.",
            "F statistics, df, and p values match jamovi output.",
            "Effect sizes (ηp²) match jamovi output.",
            "Covariate(s) are measured before the intervention or are not affected by group.",
            "Assumption checks have been reviewed.",
            "Interpretation matches the research question."
        ),
        anova_rm = c(
            "Within-subjects factor levels and outcome columns are correctly mapped.",
            "F statistic, df (with Greenhouse-Geisser correction if applied), and p value match jamovi output.",
            "Effect size (ηp²) matches jamovi output.",
            "Sphericity assumption and correction have been noted if applicable.",
            "Post hoc comparisons (if run) match jamovi output.",
            "Interpretation matches the research question."
        ),
        anova_mixed = c(
            "Between-subjects and within-subjects factors are correctly specified.",
            "F statistics, df, and p values for all effects match jamovi output.",
            "Effect sizes (ηp²) match jamovi output.",
            "Sphericity and homogeneity of variance assumptions have been reviewed.",
            "Interpretation of interaction (if significant) takes precedence.",
            "Interpretation matches the research question."
        ),
        manova = c(
            "All dependent variables and the grouping factor are correctly specified.",
            "Multivariate test statistic (Pillai's trace/Wilks' lambda) and p value match jamovi output.",
            "Univariate follow-up results match jamovi output.",
            "Assumption checks (multivariate normality, homogeneity of covariance) have been reviewed.",
            "Interpretation matches the research question."
        ),
        mann_whitney = c(
            "Group variable has exactly two independent levels.",
            "Outcome variable is at least ordinal.",
            "U statistic and p value match jamovi output.",
            "Effect size (r) matches jamovi output.",
            "Interpretation matches the research question."
        ),
        wilcoxon_signed_rank = c(
            "Both conditions or time points are the intended measurements.",
            "W statistic and p value match jamovi output.",
            "Effect size (r) matches jamovi output.",
            "Interpretation matches the research question."
        ),
        logistic_regression = c(
            "Outcome variable is binary with the correct reference category.",
            "Predictors are the intended covariates and factors.",
            "Chi-square model fit statistic and p value match jamovi output.",
            "McFadden's R² matches jamovi output.",
            "Odds ratios and confidence intervals match the coefficient table.",
            "Assumption checks (linearity of log-odds, multicollinearity) have been reviewed.",
            "Interpretation matches the research question."
        ),
        reliability_omega = c(
            "All intended items are included in the analysis.",
            "McDonald's omega and Cronbach's alpha values match jamovi output.",
            "Number of items and N match jamovi output.",
            "Item-total correlations and any flagged items have been reviewed.",
            "Interpretation matches the research question."
        ),
        bayes_ttest = c(
            "Prior specification matches the intended analysis.",
            "Bayes Factor (BF10 or BF01) matches jamovi output.",
            "Direction of evidence is correctly described.",
            "Interpretation matches the research question."
        ),
        c(
            "All reported statistics match jamovi output.",
            "Assumption checks have been reviewed.",
            "Interpretation matches the research question."
        )
    )
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


# Injects jReport's ref definitions into a parent module's .jmvrefs so that
# refs set on items in self$parent$results are resolved when the parent calls
# asProtoBuf() against its own namespace.
.jr_addon_inject_refs <- function(parent_pkg) {
    if (is.null(parent_pkg) || !nzchar(parent_pkg)) return(invisible(NULL))
    jr_ns <- tryCatch(getNamespace("jReport"), error = function(e) NULL)
    if (is.null(jr_ns) || !(".jmvrefs" %in% names(jr_ns))) return(invisible(NULL))
    jr_refs <- jr_ns[[".jmvrefs"]]

    parent_ns <- tryCatch(getNamespace(parent_pkg), error = function(e) NULL)
    if (is.null(parent_ns) || !(".jmvrefs" %in% names(parent_ns))) return(invisible(NULL))

    parent_refs <- parent_ns[[".jmvrefs"]]
    new_keys <- setdiff(names(jr_refs), names(parent_refs))
    if (length(new_keys) == 0L) return(invisible(NULL))

    for (key in new_keys)
        parent_refs[[key]] <- jr_refs[[key]]

    tryCatch({
        if (bindingIsLocked(".jmvrefs", parent_ns))
            unlockBinding(".jmvrefs", parent_ns)
        parent_ns[[".jmvrefs"]] <- parent_refs
        lockBinding(".jmvrefs", parent_ns)
    }, error = function(e) invisible(NULL))

    invisible(NULL)
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

.jr_addon_set_tables <- function(self, results) {
    apa_rows        <- .jr_addon_apa_rows(results)
    assumption_rows <- .jr_addon_assumption_rows(results)
    optional_rows <- list(
        jReportPostHoc      = .jr_addon_posthoc_rows(results),
        jReportCoefficients = .jr_addon_coefficient_rows(results),
        jReportCells        = .jr_addon_cell_rows(results),
        jReportFollowUps    = .jr_addon_followup_rows(results)
    )
    tbl <- .jr_addon_get(self$parent$results, "jReportApaTable")
    if (!is.null(tbl)) .jr_addon_fill_table(tbl, apa_rows)
    tbl <- .jr_addon_get(self$parent$results, "jReportAssumptions")
    if (!is.null(tbl)) .jr_addon_fill_table(tbl, assumption_rows)
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
        title = "APA Results Summary (jReport)", refs = refs,
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
    .jr_addon_add_result_if_missing(self, "jReportAssumptions", jmvcore::Table$new(
        options = self$options, name = "jReportAssumptions",
        title = "Assumptions and Recommended Actions (jReport)", refs = refs,
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
    if (isTRUE(posthoc))
        .jr_addon_add_result_if_missing(self, "jReportPostHoc", jmvcore::Table$new(
            options = self$options, name = "jReportPostHoc",
            title = "APA Post Hoc Comparisons (jReport)", visible = FALSE, refs = refs,
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
            title = "Odds Ratios and Coefficients (jReport)", visible = FALSE, refs = refs,
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
            title = "Observed and Expected Counts (jReport)", visible = FALSE, refs = refs,
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
            title = "MANOVA/MANCOVA Follow-up Analyses (jReport)", visible = FALSE, refs = refs,
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
    # Inject jReport ref definitions into the parent module's namespace so refs
    # appear in jamovi's native References panel after the parent's asProtoBuf() runs.
    if (!is.null(self$parent))
        .jr_addon_inject_refs(self$parent$package)

    placeholder <- .jr_html_card(
        "Automatic report", "jReport",
        "The report will appear after the standard analysis variables have been selected."
    )
    .jr_addon_add_result_if_missing(
        self, "jReportHeading",
        jmvcore::Html$new(options = self$options, name = "jReportHeading",
            title = "jReport: Automatic Reporting", content = .jr_addon_heading_html(),
            refs = refs)
    )
    .jr_addon_insert_tables(
        self, posthoc = posthoc, coefficients = coefficients,
        cells = cells, followups = followups, refs = refs
    )
    .jr_addon_add_result_if_missing(
        self, "jReportCard",
        jmvcore::Html$new(options = self$options, name = "jReportCard",
            title = "Automatic Report (jReport)", content = placeholder,
            refs = refs)
    )
}

.jr_addon_set_card <- function(self, results, note = "") {
    results <- Filter(function(r) !inherits(r, "try-error"), results)
    if (length(results) == 0L) {
        .jr_addon_message(self, paste(
            "The analysis could not be completed.",
            "Check that the selected variables have enough complete cases,",
            "appropriate measurement levels, and no singular or perfectly collinear model terms."
        ))
        return(invisible(NULL))
    }
    results <- lapply(results, function(result) {
        .jr_apply_variable_descriptions(result, self$data)
    })
    .jr_addon_set_tables(self, results)
    report_html <- .jr_addon_report_html(results, options = .jr_addon_reporting_options(), note = note)
    ref_keys <- unique(unlist(lapply(results, .jr_text_reference_keys, include_effect_note = TRUE)))
    card <- .jr_addon_get(self$parent$results, "jReportCard")
    if (!is.null(card)) {
        card$setContent(report_html)
        if (length(ref_keys) > 0L) card$setRefs(ref_keys)
    }
}

.jr_addon_message <- function(self, message) {
    .jr_addon_set_tables(self, list())
    card <- .jr_addon_get(self$parent$results, "jReportCard")
    if (!is.null(card))
        card$setContent(.jr_html_card("Automatic report", "jReport", message, accent = "#b46c21"))
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

