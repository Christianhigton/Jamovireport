#' @importFrom jmvcore .

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

.jr_copy_button_html <- function(label = "Copy text") {
    script <- paste0(
        "(function(b){",
        "var s=b.closest('[data-jr-copy-section]');",
        "var e=s?s.querySelector('[data-jr-copy-content]'):null;",
        "var t=e?(e.innerText||e.textContent||''):'';",
        "var done=function(ok){var old=b.textContent;b.textContent=ok?'Copied':'Copy failed';",
        "b.setAttribute('aria-live','polite');setTimeout(function(){b.textContent=old;},1500);};",
        "var legacy=function(){var a=document.createElement('textarea');a.value=t;",
        "a.setAttribute('readonly','');a.style.position='fixed';a.style.opacity='0';",
        "document.body.appendChild(a);a.select();var ok=false;",
        "try{ok=document.execCommand('copy');}catch(err){}document.body.removeChild(a);done(ok);};",
        "if(navigator.clipboard&&navigator.clipboard.writeText){",
        "navigator.clipboard.writeText(t).then(function(){done(true);}).catch(legacy);",
        "}else{legacy();}})(this);return false;"
    )
    sprintf(
        paste0(
            "<button type='button' aria-label='%s' onclick=\"%s\" ",
            "style='float:right;margin:0 0 8px 12px;padding:5px 10px;border:1px solid #aebbc4;",
            "border-radius:4px;background:#fff;color:#263944;font-size:12px;font-weight:600;",
            "cursor:pointer;'>%s</button>"
        ),
        .jr_html_escape(label), script, .jr_html_escape(label)
    )
}

.jr_copyable_body_html <- function(body, label = "Copy text") {
    paste0(
        .jr_copy_button_html(label),
        "<div data-jr-copy-content='true'>", body, "</div>",
        "<div style='clear:both;'></div>"
    )
}

.jr_html_card <- function(eyebrow, title, content, accent = "#237f86") {
    sprintf(
        paste0(
            "<div data-jr-copy-section='true' style='width:100%%;box-sizing:border-box;border:1px solid #dfe6ea; border-left:5px solid %s;",
            "border-radius:6px; padding:14px 16px; margin:2px 0 6px 0; background:#fbfcfd;'>",
            "<div style='font-size:11px; font-weight:600; letter-spacing:.08em; color:#536472;",
            "text-transform:uppercase; margin-bottom:6px;'>%s</div>",
            "<div style='font-size:16px; font-weight:600; color:#18242d; margin-bottom:10px;'>%s</div>",
            "%s</div>"
        ),
        accent, .jr_html_escape(eyebrow), .jr_html_escape(title),
        .jr_copyable_body_html(.jr_html_paragraphs(content))
    )
}

.jr_report_card <- function(eyebrow, title, subtitle = "", content = "",
                            accent = "#237f86", background = "#fbfcfd",
                            content_html = NULL, collapsed = FALSE) {
    subtitle_html <- ""
    if (nzchar(subtitle)) {
        subtitle_html <- sprintf(
            "<div style='font-size:13px; font-weight:600; color:#536472; margin:-3px 0 10px 0;'>%s</div>",
            .jr_html_escape(subtitle)
        )
    }
    body <- if (is.null(content_html)) .jr_html_paragraphs(content) else content_html
    if (isTRUE(collapsed)) {
        return(sprintf(
            paste0(
                "<details data-jr-copy-section='true' style='width:100%%;box-sizing:border-box;border:1px solid #dfe6ea; border-left:5px solid %s;",
                "border-radius:6px; padding:0; margin:2px 0 6px 0; background:%s;'>",
                "<summary style='cursor:pointer; list-style-position:inside; padding:14px 16px;",
                "font-size:17px; font-weight:700; color:#18242d;'>%s</summary>",
                "<div style='padding:0 16px 14px 16px;'>",
                "<div style='font-size:11px; font-weight:700; letter-spacing:0; color:#536472;",
                "text-transform:uppercase; margin-bottom:6px;'>%s</div>",
                "%s%s</div></details>"
            ),
            accent, background, .jr_html_escape(title), .jr_html_escape(eyebrow),
            subtitle_html, .jr_copyable_body_html(body)
        ))
    }
    sprintf(
        paste0(
            "<div data-jr-copy-section='true' style='width:100%%;box-sizing:border-box;border:1px solid #dfe6ea; border-left:5px solid %s;",
            "border-radius:6px; padding:14px 16px; margin:2px 0 6px 0; background:%s;'>",
            "<div style='font-size:11px; font-weight:700; letter-spacing:0; color:#536472;",
            "text-transform:uppercase; margin-bottom:6px;'>%s</div>",
            "<div style='font-size:17px; font-weight:700; color:#18242d; margin-bottom:8px;'>%s</div>",
            "%s%s</div>"
        ),
        accent, background, .jr_html_escape(eyebrow), .jr_html_escape(title),
        subtitle_html, .jr_copyable_body_html(body)
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
            diagnostic_text, accent = "#2f6fa3", background = "#f5f9fd",
            collapsed = FALSE
        ))
    }
    if (!is.null(guidance_text) && nzchar(guidance_text)) {
        cards <- paste0(cards, .jr_report_card(
        "Interpretation guidance", "How to read this result",
        "For understanding only - do not copy directly.",
        guidance_text, accent = "#b46c21", background = "#fff9ef",
        collapsed = FALSE
        ))
    }
    checklist_html <- .jr_html_bullets(checklist_items %||% character())
    if (nzchar(checklist_note)) {
        checklist_html <- paste0(
            checklist_html,
            "<div style='border-top:1px solid #dfe6ea; margin-top:12px; padding-top:10px;'>",
            .jr_html_paragraphs(checklist_note),
            "</div>"
        )
    }
    if (nzchar(checklist_html)) {
        cards <- paste0(cards, .jr_report_card(
            "Check before reporting", "Verification checklist",
            "", accent = "#6d5a8a", background = "#faf8fc",
            content_html = checklist_html, collapsed = FALSE
        ))
    }
    paste0("<div style='width:100%;box-sizing:border-box;display:block;'>", cards, "</div>")
}

.jr_report_section_card <- function(title, subtitle = "", content = "",
                                    accent = "#237f86", background = "#fbfcfd",
                                    content_html = NULL, collapsed = FALSE) {
    subtitle_html <- ""
    if (nzchar(subtitle)) {
        subtitle_html <- sprintf(
            "<div style='font-size:13px; font-weight:600; color:#536472; margin:-2px 0 10px 0;'>%s</div>",
            .jr_html_escape(subtitle)
        )
    }
    body <- if (is.null(content_html)) .jr_html_paragraphs(content) else content_html
    if (isTRUE(collapsed)) {
        return(sprintf(
            paste0(
                "<details data-jr-copy-section='true' style='width:100%%;box-sizing:border-box;border:1px solid #dfe6ea; border-left:5px solid %s;",
                "border-radius:6px; padding:0; margin:2px 0 6px 0; background:%s;'>",
                "<summary style='cursor:pointer; list-style-position:inside; padding:14px 16px;",
                "font-size:17px; font-weight:700; color:#18242d;'>%s</summary>",
                "<div style='padding:0 16px 14px 16px;'>%s%s</div></details>"
            ),
            accent, background, .jr_html_escape(title), subtitle_html,
            .jr_copyable_body_html(body)
        ))
    }
    sprintf(
        paste0(
            "<div data-jr-copy-section='true' style='width:100%%;box-sizing:border-box;border:1px solid #dfe6ea; border-left:5px solid %s;",
            "border-radius:6px; padding:14px 16px; margin:2px 0 6px 0; background:%s;'>",
            "<div style='font-size:17px; font-weight:700; color:#18242d; margin-bottom:8px;'>%s</div>",
            "%s%s</div>"
        ),
        accent, background, .jr_html_escape(title), subtitle_html,
        .jr_copyable_body_html(body)
    )
}

.jr_report_wording_title <- function(style = "apa7") {
    style <- as.character(style %||% "apa7")[1]
    switch(
        style,
        plain = "Suggested plain-language report wording",
        journal = "Suggested journal-style report wording",
        dissertation = "Suggested dissertation-style report wording",
        "Suggested APA-style report wording"
    )
}

.jr_report_wording_subtitle <- function(style = "apa7") {
    style <- as.character(style %||% "apa7")[1]
    switch(
        style,
        plain = "This is suggested explanatory wording only. Check all values against your jamovi output and adapt the text for your own study before using it.",
        journal = "This is suggested journal-style wording only. Check all values against your jamovi output and adapt the text for the target journal before using it.",
        dissertation = "This is suggested dissertation-style wording only. Check all values against your jamovi output and adapt the text for your research question before using it.",
        "This is suggested wording only. Check all values against your jamovi output and adapt the text for your own study before using it."
    )
}

.jr_report_section_title <- function(title, analysis_label = "") {
    analysis_label <- trimws(as.character(analysis_label %||% "")[1])
    if (!nzchar(analysis_label))
        return(title)
    paste(title, analysis_label, sep = " \u2014 ")
}

.jr_build_report_sections_html <- function(apa_wording = NULL,
                                           diagnostic_note = NULL,
                                           interpretation_guidance = NULL,
                                           checklist_items = NULL,
                                           checklist_note = "",
                                           references = NULL,
                                           report_style = "apa7",
                                           analysis_label = "") {
    sections <- character()
    if (!is.null(apa_wording) && nzchar(apa_wording)) {
        sections <- c(sections, .jr_report_section_card(
            .jr_report_section_title(
                .jr_report_wording_title(report_style), analysis_label
            ),
            .jr_report_wording_subtitle(report_style),
            apa_wording, accent = "#278058", background = "#f6fbf8"
        ))
    }
    if (!is.null(diagnostic_note) && nzchar(diagnostic_note)) {
        sections <- c(sections, .jr_report_section_card(
            .jr_report_section_title(
                "Optional assumptions / diagnostic note", analysis_label
            ),
            "Include this only if relevant to your study.",
            diagnostic_note, accent = "#2f6fa3", background = "#f5f9fd",
            collapsed = FALSE
        ))
    }
    if (!is.null(interpretation_guidance) && nzchar(interpretation_guidance)) {
        sections <- c(sections, .jr_report_section_card(
            .jr_report_section_title("Interpretation guidance", analysis_label),
            "For understanding only - do not copy directly into your report.",
            interpretation_guidance, accent = "#b46c21", background = "#fff9ef",
            collapsed = FALSE
        ))
    }
    checklist_html <- .jr_html_bullets(checklist_items %||% character())
    if (nzchar(checklist_note))
        checklist_html <- paste0(checklist_html, .jr_html_paragraphs(checklist_note))
    if (nzchar(checklist_html)) {
        sections <- c(sections, .jr_report_section_card(
            .jr_report_section_title("Check before reporting", analysis_label),
            "", accent = "#6d5a8a", background = "#faf8fc",
            content_html = checklist_html, collapsed = FALSE
        ))
    }
    paste0("<div style='width:100%;box-sizing:border-box;display:block;'>", paste(sections, collapse = ""), "</div>")
}

.jr_references_html <- function(results, include_effect_note = TRUE) {
    entries <- .jr_reference_entries(results, include_effect_note = include_effect_note)
    if (!length(entries))
        return("")
    rows <- paste0(
        "<li style='margin:0 0 8px 0; padding-left:2px;'>",
        .jr_html_escape(entries),
        "</li>",
        collapse = ""
    )
    paste0(
        "<div style='border-top:1px solid #dfe6ea; margin:14px 0 0 0; padding-top:12px;'>",
        "<div style='font-size:13px; font-weight:600; color:#18242d; margin-bottom:8px;'>References</div>",
        "<ol style='margin:0; padding-left:22px; line-height:1.45;'>",
        rows,
        "</ol></div>"
    )
}

.jr_reference_entries <- function(results, include_effect_note = TRUE) {
    keys <- unique(unlist(lapply(results, .jr_text_reference_keys, include_effect_note = include_effect_note)))
    entries <- vapply(keys, .jr_reference_entry_text, character(1))
    entries[nzchar(entries)]
}

.jr_reference_definitions <- function() {
    tryCatch(get(".jmvrefs", envir = asNamespace("jReport")), error = function(e) list())
}

.jr_reference_display_name <- function(key) {
    switch(
        key,
        RCore = "R",
        jmvcore = "jmvcore",
        jReport = "jReport",
        afex = "afex",
        car = "car",
        effectsize = "effectsize",
        parameters = "parameters",
        performance = "performance",
        psych = "psych",
        ggplot2 = "ggplot2",
        BayesFactor = "BayesFactor",
        emmeans = "emmeans",
        McDonald1999 = "McDonald (1999)",
        RevelleCondon2019 = "Revelle & Condon (2019)",
        Cohen1988 = "Cohen (1988)",
        Cumming2014 = "Cumming (2014)",
        Holm1979 = "Holm (1979)",
        Vickerstaff2019 = "Vickerstaff et al. (2019)",
        BenjaminiHochberg1995 = "Benjamini & Hochberg (1995)",
        key
    )
}

.jr_reference_role_text <- function(key) {
    switch(
        key,
        RCore = "the statistical computing environment used to run the analysis.",
        jmvcore = "the jamovi framework package used to build and display module results.",
        jReport = "the module that generates the guided tables, interpretation, and report wording.",
        afex = "supports ANOVA-style modelling and repeated-measures model handling.",
        car = "supports regression, ANOVA, ANCOVA, MANOVA, and diagnostic calculations.",
        effectsize = "computes effect-size estimates and confidence intervals.",
        parameters = "extracts and formats model coefficients and related model parameters.",
        performance = "supports model-fit and diagnostic checks such as collinearity and heteroscedasticity.",
        psych = "estimates reliability coefficients and item-level reliability diagnostics.",
        ggplot2 = "creates visual summaries shown in the output.",
        BayesFactor = "computes Bayes factors for Bayesian t-test reporting.",
        emmeans = "computes estimated marginal means and post hoc comparisons.",
        McDonald1999 = "provides the psychometric basis for omega reliability.",
        RevelleCondon2019 = "provides reliability-reporting guidance for omega and alpha.",
        Cohen1988 = "provides conventional effect-size benchmark language.",
        Cumming2014 = "supports cautious interpretation of effect sizes and confidence intervals.",
        Holm1979 = "introduces the sequential Holm procedure for controlling the familywise Type I error rate.",
        Vickerstaff2019 = "provides guidance on multiple-comparison adjustment for families of outcomes.",
        BenjaminiHochberg1995 = "introduces the false discovery rate and its sequential p-value adjustment procedure.",
        "is used by this analysis."
    )
}

.jr_reference_is_software <- function(key) {
    refs <- .jr_reference_definitions()
    ref <- refs[[key]]
    identical(ref$type %||% "", "software")
}

.jr_methods_reference_items_html <- function(keys) {
    if (!length(keys))
        return("<p style='margin:0;color:#536472;'>No additional references are used by this panel.</p>")
    rows <- vapply(keys, function(key) {
        reference <- .jr_reference_entry_text(key)
        if (!nzchar(reference))
            reference <- .jr_reference_display_name(key)
        sprintf(
            paste0(
                "<li style='margin:0 0 10px 0; padding-left:2px;'>",
                "<div><strong>%s.</strong> %s</div>",
                "<div style='color:#536472; margin-top:3px;'>Used for: %s</div>",
                "</li>"
            ),
            .jr_html_escape(.jr_reference_display_name(key)),
            .jr_html_escape(reference),
            .jr_html_escape(.jr_reference_role_text(key))
        )
    }, character(1))
    paste0("<ul style='margin:0; padding-left:22px; line-height:1.45;'>", paste(rows, collapse = ""), "</ul>")
}

.jr_methods_references_html <- function(results = NULL, keys = NULL, include_effect_note = TRUE) {
    if (!is.null(results) && inherits(results, "edu_analysis"))
        results <- list(results)
    if (is.null(keys))
        keys <- unique(unlist(lapply(results, .jr_text_reference_keys, include_effect_note = include_effect_note)))
    keys <- unique(keys[!is.na(keys) & nzchar(keys)])
    software <- keys[vapply(keys, .jr_reference_is_software, logical(1))]
    literature <- setdiff(keys, software)
    body <- paste0(
        .jr_report_section_card(
            "R packages and software",
            "These are tools used to compute, format, or display the results.",
            accent = "#2f6fa3", background = "#f5f9fd",
            content_html = .jr_methods_reference_items_html(software),
            collapsed = FALSE
        ),
        .jr_report_section_card(
            "Literature and reporting guidance",
            "These are books or articles used for statistical interpretation and reporting guidance.",
            accent = "#6d5a8a", background = "#faf8fc",
            content_html = .jr_methods_reference_items_html(literature),
            collapsed = FALSE
        )
    )
    paste0("<div style='width:100%;box-sizing:border-box;display:block;'>", body, "</div>")
}

.jr_jamovi_overview_html <- function(result) {
    html <- .jr_html_card(
        "jReport", "Guided report controls are available here",
        paste(
            "This output was generated by jReport. Use the Reporting controls in this jReport panel to choose style, format, tone, and included content.",
            "Simplified report-style output is available only for supported analyses and will appear when those analyses are run from their respective jamovi analysis menus.",
            paste(result$question, result$requirements, sep = "\n\n"),
            sep = "\n\n"
        )
    )
    paste0("<div style='width:100%;box-sizing:border-box;display:block;'>", html, "</div>")
}

.jr_jamovi_report_html <- function(result, options) {
    html <- .jr_html_card(
        "Copy-ready reporting", "Report text",
        .jr_jamovi_text(result, options), accent = "#4b66a2"
    )
    paste0("<div style='width:100%;box-sizing:border-box;display:block;'>", html, "</div>")
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
        "Generalised eta squared (\u03b7G\u00b2) estimates the proportion of total variance",
        "explained by an effect and allows comparison across different experimental designs.",
        "Partial eta squared (\u03b7p\u00b2) estimates the proportion of variance explained after",
        "accounting for other sources of variance in the model and is therefore often larger."
    )
    max_ges <- max(ges_vals, na.rm = TRUE)
    max_eta <- max(eta_vals, na.rm = TRUE)
    discrepancy <- if (is.finite(max_ges) && is.finite(max_eta) && max_ges > 0 &&
                       max_eta > 2 * max_ges) {
        paste(
            "The difference between \u03b7G\u00b2 and \u03b7p\u00b2 suggests that a substantial proportion of",
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
    html <- .jr_html_card("Interpretation", "What does this mean?", content, accent = accent)
    paste0("<div style='width:100%;box-sizing:border-box;display:block;'>", html, "</div>")
}
