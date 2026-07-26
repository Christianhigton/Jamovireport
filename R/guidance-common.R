.jr_guidance_block <- function(text = character(), bullets = character()) {
    list(
        text = trimws(as.character(text %||% character())),
        bullets = trimws(as.character(bullets %||% character()))
    )
}

.jr_guidance_nonempty <- function(value) {
    if (is.null(value))
        return(FALSE)
    if (is.list(value))
        return(any(nzchar(c(value$text %||% character(), value$bullets %||% character()))))
    any(nzchar(trimws(as.character(value))))
}

.jr_guidance_sections_html <- function(sections) {
    if (!length(sections))
        return("")
    keep <- vapply(sections, .jr_guidance_nonempty, logical(1))
    sections <- sections[keep]
    if (!length(sections))
        return("")
    rows <- vapply(names(sections), function(title) {
        value <- sections[[title]]
        if (!is.list(value))
            value <- .jr_guidance_block(text = value)
        text <- value$text %||% character()
        text <- text[nzchar(text)]
        bullets <- value$bullets %||% character()
        bullets <- bullets[nzchar(bullets)]
        body <- paste0(
            if (length(text)) .jr_html_paragraphs(paste(text, collapse = "\n\n")) else "",
            if (length(bullets)) .jr_html_bullets(bullets) else ""
        )
        sprintf(
            paste0(
                "<section style='margin:0 0 16px 0;'>",
                "<h4 style='font-size:15px;line-height:1.3;color:#18242d;",
                "margin:0 0 7px 0;'>%s</h4>%s</section>"
            ),
            .jr_html_escape(title), body
        )
    }, character(1))
    paste(rows, collapse = "")
}

.jr_interpretation_guidance_panel <- function(sections, analysis_label = "") {
    title <- .jr_report_section_title("Interpretation guidance", analysis_label)
    body <- .jr_guidance_sections_html(sections)
    if (!nzchar(body))
        return("")
    .jr_report_section_card(
        title,
        "Explanation of the findings, effect sizes, assumptions and checks",
        accent = "#b46c21", background = "#fff9ef",
        content_html = body, collapsed = TRUE
    )
}

.jr_call_value_text <- function(result, name, fallback = "") {
    value <- result$call[[name]]
    if (is.null(value))
        return(fallback)
    value <- paste(deparse(value, width.cutoff = 500L), collapse = "")
    value <- sub("^['\"]", "", sub("['\"]$", "", value))
    if (nzchar(value)) value else fallback
}

.jr_formula_labels <- function(result) {
    model <- result$model
    formula <- tryCatch(stats::formula(model), error = function(e) NULL)
    if (is.null(formula))
        return(list(outcome = "the outcome", predictors = character()))
    list(
        outcome = all.vars(formula)[1] %||% "the outcome",
        predictors = attr(stats::terms(formula), "term.labels") %||% character()
    )
}

.jr_guidance_data_checks <- function(result) {
    analysis <- result$analysis %||% ""
    common <- c(
        "Confirm that the selected variables and their measurement levels match the research question.",
        "Check coding, reference levels, usable sample size and the analysis-specific treatment of missing values.",
        "Review distributions, sparse categories and unusual observations before relying on the model."
    )
    design <- switch(
        analysis,
        correlation = "Inspect the bivariate plot and confirm that the selected correlation method matches the form of association.",
        regression = "Confirm that categorical predictors use the intended reference levels and that the model formula contains the intended terms.",
        logistic_regression = "Confirm the modelled event, outcome reference category and predictor reference levels.",
        multinomial_logistic = "Confirm the outcome reference category and each categorical predictor's reference level.",
        chisq_independence = "Confirm that categories are mutually exclusive and that each case contributes to one cell.",
        chisq_gof = "Confirm that the expected proportions were specified independently of the observed counts.",
        reliability_omega = "Confirm that all items measure the intended construct, use compatible response directions and have been reverse-scored where required.",
        anova_rm = "Confirm that repeated-measures columns are in the intended condition order and belong to the same cases.",
        anova_mixed = "Confirm both the repeated-measures order and the between-subjects group coding.",
        manova = "Confirm that every dependent variable is intended and measured on a suitable scale.",
        "Confirm that the grouping factors, covariates and outcome are assigned to their intended roles."
    )
    c(common, design)
}

.jr_descriptive_guidance <- function(result) {
    values <- result$descriptives
    if (!is.data.frame(values) || nrow(values) == 0L)
        return("No descriptive table was available. Interpret inferential results only after checking the underlying observations and usable sample size.")
    group_col <- intersect(c("group", "condition", "occasion", "item"), names(values))
    group_col <- if (length(group_col)) group_col[1] else ""
    if (nzchar(group_col) && all(c("n", "mean", "sd") %in% names(values))) {
        rows <- vapply(seq_len(nrow(values)), function(i) sprintf(
            "%s: n = %s, M = %s, SD = %s",
            values[[group_col]][i], .jr_num(values$n[i], 0L),
            .jr_num(values$mean[i]), .jr_num(values$sd[i])
        ), character(1))
        return(paste(
            paste(rows, collapse = "; "),
            "Compare the observed centres, spreads and sample sizes before interpreting the model."
        ))
    }
    if (nzchar(group_col) && all(c("n", "median", "iqr") %in% names(values))) {
        rows <- vapply(seq_len(nrow(values)), function(i) sprintf(
            "%s: n = %s, Mdn = %s, IQR = %s",
            values[[group_col]][i], .jr_num(values$n[i], 0L),
            .jr_num(values$median[i]), .jr_num(values$iqr[i])
        ), character(1))
        return(paste(
            paste(rows, collapse = "; "),
            "These summaries describe the observed distributions; a rank test is not automatically a test of medians."
        ))
    }
    result$report_blocks$descriptives %||%
        sprintf("The analysis used %s rows of descriptive output.", nrow(values))
}

.jr_diagnostic_guidance <- function(result) {
    rows <- .jr_normalize_diagnostics(result$diagnostics)
    if (!nrow(rows))
        return("No automatic diagnostic result was available. Design assumptions and data quality still require review.")
    text <- vapply(seq_len(nrow(rows)), function(i) {
        tested <- if (identical(rows$tested[i], "Yes") &&
                is.finite(rows$statistic[i])) {
            paste0(
                " The observed statistic was ", .jr_num(rows$statistic[i]),
                if (is.finite(rows$p[i])) paste0(", p ", .jr_p(rows$p[i])) else "",
                "."
            )
        } else {
            ""
        }
        paste0(
            rows$check[i], " \u2014 ", rows$status[i], ".", tested, " ",
            rows$interpretation[i], " ", rows$action[i]
        )
    }, character(1))
    paste(
        text,
        collapse = "\n\n"
    )
}

.jr_interval_includes <- function(lower, upper, null = 0) {
    all(is.finite(c(lower, upper, null))) && lower <= null && upper >= null
}

.jr_effect_spec <- function(analysis) {
    switch(
        analysis,
        ttest = list(
            name = "Cohen's d", symbol = "d", null = 0,
            thresholds = c(small = .20, medium = .50, large = .80),
            citation = "Cohen (1988)", keys = c("Cohen1988", "Cumming2014", "Lakens2013")
        ),
        correlation = list(
            name = "correlation coefficient", symbol = "r", null = 0,
            thresholds = c(small = .10, medium = .30, large = .50),
            citation = "Cohen (1988)", keys = c("Cohen1988", "Cumming2014")
        ),
        anova_oneway = list(
            name = "eta squared", symbol = "\u03b7\u00b2", null = 0,
            thresholds = c(small = .01, medium = .06, large = .14),
            citation = "Cohen (1988)", keys = c("Cohen1988", "Cumming2014", "Lakens2013")
        ),
        anova_between = list(
            name = "partial eta squared", symbol = "\u03b7p\u00b2", null = 0,
            thresholds = c(small = .01, medium = .06, large = .14),
            citation = "Cohen (1988)", keys = c("Cohen1988", "Cumming2014", "Lakens2013"),
            caution = paste(
                "These are approximate Cohen-style reference values.",
                "Partial eta squared and eta squared use different denominators and are not directly interchangeable across designs."
            )
        ),
        ancova = list(
            name = "partial eta squared", symbol = "\u03b7p\u00b2", null = 0,
            thresholds = c(small = .01, medium = .06, large = .14),
            citation = "Cohen (1988)", keys = c("Cohen1988", "Cumming2014", "Lakens2013"),
            caution = paste(
                "These are approximate Cohen-style reference values.",
                "Partial eta squared and eta squared use different denominators and are not directly interchangeable across designs."
            )
        ),
        anova_rm = list(
            name = "partial eta squared", symbol = "\u03b7p\u00b2", null = 0,
            thresholds = c(small = .01, medium = .06, large = .14),
            citation = "Cohen (1988)", keys = c("Cohen1988", "Cumming2014", "Lakens2013"),
            caution = paste(
                "These are approximate Cohen-style reference values.",
                "Generalised eta squared should be preferred when comparison across designs is important."
            )
        ),
        anova_mixed = list(
            name = "partial eta squared", symbol = "\u03b7p\u00b2", null = 0,
            thresholds = c(small = .01, medium = .06, large = .14),
            citation = "Cohen (1988)", keys = c("Cohen1988", "Cumming2014", "Lakens2013"),
            caution = paste(
                "These are approximate Cohen-style reference values.",
                "Generalised eta squared should be preferred when comparison across designs is important."
            )
        ),
        NULL
    )
}

.jr_effect_position <- function(value, thresholds) {
    value <- abs(value)
    small <- thresholds[["small"]]
    medium <- thresholds[["medium"]]
    large <- thresholds[["large"]]
    if (value < small)
        return(sprintf("below the conventional small reference value of %s", .jr_num(small, 2L, TRUE)))
    if (value == small)
        return("at the conventional small reference value")
    if (value < medium) {
        closer <- if ((value - small) <= (medium - value)) "small" else "medium"
        return(sprintf(
            "between the conventional small and medium reference values and closer to %s",
            closer
        ))
    }
    if (value == medium)
        return("at the conventional medium reference value")
    if (value < large) {
        closer <- if ((value - medium) <= (large - value)) "medium" else "large"
        return(sprintf(
            "between the conventional medium and large reference values and closer to %s",
            closer
        ))
    }
    if (value == large)
        return("at the conventional large reference value")
    "above the conventional large reference value"
}

.jr_effect_interval_guidance <- function(lower, upper, spec) {
    if (!all(is.finite(c(lower, upper))) || is.null(spec))
        return("")
    includes <- .jr_interval_includes(lower, upper, spec$null %||% 0)
    endpoints <- c(
        .jr_effect_position(lower, spec$thresholds),
        .jr_effect_position(upper, spec$thresholds)
    )
    paste(
        sprintf(
            "The confidence interval ranged from %s to %s and %s the null value of %s.",
            .jr_num(lower, 2L, TRUE), .jr_num(upper, 2L, TRUE),
            if (includes) "included" else "excluded",
            .jr_num(spec$null %||% 0, 0L)
        ),
        if (!identical(endpoints[1], endpoints[2])) {
            paste(
                "It was compatible with magnitudes ranging from",
                endpoints[1], "to", endpoints[2],
                "so the population effect's magnitude remains uncertain."
            )
        } else {
            paste("Both interval limits were", endpoints[1], "on the conventional scale.")
        }
    )
}

.jr_primary_effect <- function(result) {
    analysis <- result$analysis %||% ""
    if (identical(analysis, "ttest")) {
        effects <- result$effects
        if (is.data.frame(effects) && nrow(effects)) {
            value_name <- intersect(c("Cohens_d", "Hedges_g"), names(effects))
            if (length(value_name)) {
                return(list(
                    value = as.numeric(effects[[value_name[1]]][1]),
                    lower = as.numeric(effects$CI_low[1] %||% NA_real_),
                    upper = as.numeric(effects$CI_high[1] %||% NA_real_)
                ))
            }
        }
    }
    statistics <- result$statistics
    if (!is.data.frame(statistics) || !nrow(statistics))
        return(list(value = NA_real_, lower = NA_real_, upper = NA_real_))
    value <- if ("effect" %in% names(statistics)) statistics$effect[1] else
        if (identical(analysis, "correlation") && "statistic" %in% names(statistics))
            statistics$statistic[1] else NA_real_
    list(
        value = as.numeric(value),
        lower = if ("ci_low" %in% names(statistics)) as.numeric(statistics$ci_low[1]) else NA_real_,
        upper = if ("ci_high" %in% names(statistics)) as.numeric(statistics$ci_high[1]) else NA_real_
    )
}

.jr_effect_size_guidance <- function(result) {
    spec <- .jr_effect_spec(result$analysis %||% "")
    effect <- .jr_primary_effect(result)
    if (is.null(spec) || !is.finite(effect$value))
        return("")
    direction <- if (effect$value > 0) "positive" else if (effect$value < 0) "negative" else "zero"
    benchmark_values <- paste(
        vapply(spec$thresholds, .jr_num, character(1), digits = 2L, omit_zero = TRUE),
        collapse = ", "
    )
    text <- paste(
        sprintf(
            "%s (%s) was %s. This measure represents %s; its sign indicates a %s direction where direction is meaningful.",
            spec$name, spec$symbol, .jr_num(effect$value, 2L, TRUE),
            switch(
                result$analysis,
                ttest = "the mean difference in standard-deviation units",
                correlation = "the direction and strength of the standardised association",
                "the proportion of variation attributed to the model effect"
            ),
            direction
        ),
        sprintf(
            "The absolute estimate was %s. The conventional small, medium and large reference values are %s, respectively (%s).",
            .jr_effect_position(effect$value, spec$thresholds),
            benchmark_values, spec$citation
        ),
        spec$caution %||% "",
        paste(
            "These values are broad reference points rather than universal boundaries.",
            "Practical importance depends on the outcome scale, design, measurement quality and research context."
        )
    )
    interval <- .jr_effect_interval_guidance(effect$lower, effect$upper, spec)
    paste(c(text, interval)[nzchar(c(text, interval))], collapse = "\n\n")
}

.jr_guidance_reference_keys <- function(result) {
    analysis <- result$analysis %||% ""
    spec <- .jr_effect_spec(analysis)
    keys <- spec$keys %||% character()
    keys <- c(
        keys,
        if (analysis %in% c("anova_oneway", "anova_between", "ancova", "anova_rm", "anova_mixed"))
            c("Maxwell2018", "Lenth2016") else character(),
        if (identical(analysis, "manova"))
            c("Huberty2006", "Holm1979") else character(),
        if (analysis %in% c("chisq_independence", "chisq_gof"))
            "Agresti2019" else character(),
        if (identical(analysis, "reliability_omega"))
            c("McDonald1999", "RevelleCondon2019") else character(),
        if (analysis %in% c("mann_whitney", "wilcoxon_signed_rank"))
            "Lakens2013" else character()
    )
    if (nzchar(.jr_follow_up_analysis_guidance(result)))
        keys <- c(keys, "Holm1979", "Maxwell2018", "Lenth2016")
    unique(keys)
}

.jr_literature_guidance <- function(result) {
    keys <- .jr_guidance_reference_keys(result)
    if (!length(keys))
        return("Interpretation is based on the displayed estimates, diagnostics and study-design requirements; no numerical benchmark is imposed.")
    names <- vapply(keys, .jr_reference_display_name, character(1))
    paste(
        "Key interpretive guidance used here:",
        paste(names, collapse = "; "),
        "Complete bibliographic entries are provided in the References output."
    )
}

.jr_overall_interpretation <- function(result) {
    text <- result$interpretation %||% ""
    statistics <- result$statistics
    if (is.data.frame(statistics) && "p" %in% names(statistics) &&
            any(is.finite(statistics$p)) && !any(statistics$p < .05, na.rm = TRUE)) {
        text <- paste(
            text,
            "A non-significant result is not proof of no effect, no association or equivalence.",
            "The confidence interval describes effects still compatible with the data; an equivalence design and test would be required to support equivalence."
        )
    }
    text
}
