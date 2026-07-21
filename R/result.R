#' Select reporting preferences
#'
#' @param style Report style: APA 7, plain English, journal, or dissertation.
#' @param format Report length and presentation format.
#' @param include Components to include in an expanded report.
#' @param tone Level of explanatory detail.
#' @return A list of reporting options used by [edu_report()].
#' @export
edu_reporting_options <- function(
    style = c("apa7", "plain", "journal", "dissertation"),
    format = c("short", "paragraph", "bullets", "table_paragraph", "copy_ready"),
    include = c("descriptives", "assumptions", "test", "test_statistic", "df", "p", "effect_size",
                "ci", "posthoc", "interpretation", "cautions"),
    tone = c("student_friendly", "concise", "detailed", "critical")) {
    list(
        style = match.arg(style),
        format = match.arg(format),
        include = unique(include),
        tone = match.arg(tone)
    )
}

.jr_report_tone_note <- function(tone) {
    switch(
        tone,
        detailed = paste(
            "Reporting detail: Include the named test, test statistic, degrees of freedom, exact p value,",
            "effect size, and confidence interval when these are available and relevant."
        ),
        critical = paste(
            "Critical reporting note: Interpret this result alongside the study design, sample size,",
            "assumption checks, missing data, outliers, multiplicity, and the practical size of the effect."
        ),
        ""
    )
}

.jr_report_style_note <- function(style) {
    switch(
        style,
        journal = paste(
            "Journal style note: Keep the report concise and place additional assumption checks,",
            "sensitivity analyses, or exploratory details in the surrounding manuscript text when needed."
        ),
        dissertation = paste(
            "Dissertation style note: Link this statistical result back to the research question,",
            "hypothesis, assumptions, and any planned follow-up decisions."
        ),
        ""
    )
}

.jr_nonempty_text <- function(...) {
    text <- unlist(list(...), use.names = FALSE)
    text <- text[!is.na(text)]
    text[nzchar(text)]
}

.new_edu_analysis <- function(analysis, label, question, requirements, main,
                              descriptives, effects, diagnostics, interpretation,
                              caution, plot_data, report_blocks, statistics,
                              call = NULL) {
    diagnostics <- .jr_normalize_diagnostics(diagnostics)
    diagnostics <- .jr_normalize_diagnostics(rbind(
        .jr_default_assumptions(analysis),
        diagnostics
    ))
    structure(
        list(
            analysis = analysis,
            label = label,
            question = question,
            requirements = requirements,
            main = main,
            descriptives = descriptives,
            effects = effects,
            diagnostics = diagnostics,
            interpretation = interpretation,
            caution = caution,
            plot_data = plot_data,
            report_blocks = report_blocks,
            statistics = statistics,
            call = call
        ),
        class = "edu_analysis"
    )
}

#' Generate explanatory and reporting text
#'
#' @param x An `edu_analysis` result.
#' @param style Report style.
#' @param format Report format.
#' @param include Components to include in expanded outputs.
#' @param tone Explanation tone.
#' @return Character text ready for reports or jamovi result panels.
#' @export
edu_report <- function(
    x,
    style = c("apa7", "plain", "journal", "dissertation"),
    format = c("short", "paragraph", "bullets", "table_paragraph", "copy_ready"),
    include = c("descriptives", "assumptions", "test", "test_statistic", "df", "p", "effect_size",
                "ci", "posthoc", "interpretation", "cautions"),
    tone = c("student_friendly", "concise", "detailed", "critical")) {
    if (!inherits(x, "edu_analysis"))
        .jr_stop("`x` must be an educational analysis result.")
    options <- edu_reporting_options(style, format, include, tone)
    blocks <- x$report_blocks
    blocks$apa <- .jr_apply_inclusions(blocks$apa, x$analysis, options$include)
    independent_t <- identical(x$analysis, "ttest") &&
        is.data.frame(x$statistics) && nrow(x$statistics) == 1L &&
        x$statistics$test[1] %in% c("Student's t", "Welch's t")
    note <- ""
    if ("effect_size" %in% options$include) {
        effect_text <- .jr_effect_benchmark_text(x)
        if (nzchar(effect_text))
            blocks$apa <- paste(blocks$apa, effect_text, sep = " ")
        note <- .jr_effect_interpretation_note(x$analysis)
    }
    if (independent_t && options$tone == "concise")
        note <- ""

    if (independent_t && options$style != "plain") {
        selected <- blocks$apa
        if (options$style == "journal" && options$tone != "concise")
            selected <- c(selected, .jr_report_style_note(options$style))
        if (options$style == "dissertation")
            selected <- c(selected, .jr_report_style_note(options$style))
    } else if (options$format == "copy_ready") {
        selected <- if (options$style == "plain") blocks$plain else blocks$apa
    } else if (options$format == "short") {
        if (options$style == "plain") {
            selected <- c(sprintf("What this analysis asks: %s", x$question), blocks$plain)
        } else {
            selected <- blocks$apa
        }
    } else if (options$style == "plain") {
        opening <- sprintf("What this analysis asks: %s", x$question)
        selected <- c(opening, blocks$plain)
        if ("assumptions" %in% options$include)
            selected <- c(selected, blocks$assumptions)
        if ("cautions" %in% options$include && nzchar(x$caution))
            selected <- c(selected, x$caution)
    } else if (options$style == "journal") {
        selected <- c(
            if ("descriptives" %in% options$include) blocks$descriptives else "",
            blocks$apa
        )
        if (options$tone != "concise")
            selected <- c(selected, .jr_report_style_note(options$style))
    } else if (options$style == "dissertation") {
        selected <- c(blocks$rationale)
        if ("descriptives" %in% options$include)
            selected <- c(selected, blocks$descriptives)
        selected <- c(selected, blocks$apa)
        if ("assumptions" %in% options$include)
            selected <- c(selected, blocks$assumptions)
        if ("interpretation" %in% options$include)
            selected <- c(selected, blocks$plain)
        if ("cautions" %in% options$include && nzchar(x$caution))
            selected <- c(selected, x$caution)
        selected <- c(selected, .jr_report_style_note(options$style))
    } else {
        selected <- c(blocks$rationale)
        if ("descriptives" %in% options$include)
            selected <- c(selected, blocks$descriptives)
        selected <- c(selected, blocks$apa)
        if ("assumptions" %in% options$include)
            selected <- c(selected, blocks$assumptions)
        if ("interpretation" %in% options$include)
            selected <- c(selected, blocks$plain)
        if ("cautions" %in% options$include && nzchar(x$caution))
            selected <- c(selected, x$caution)
    }

    if (options$tone == "concise" && options$format %in% c("paragraph", "table_paragraph", "bullets")) {
        selected <- if (options$style == "plain") {
            c(sprintf("What this analysis asks: %s", x$question), blocks$plain)
        } else {
            blocks$apa
        }
    } else if (options$format %in% c("paragraph", "table_paragraph", "bullets")) {
        selected <- c(selected, .jr_report_tone_note(options$tone))
    }

    if (options$format == "table_paragraph") {
        selected <- c(
            "Use the accompanying results table for the exact values, then report the result in text as follows:",
            selected
        )
    }

    selected <- .jr_nonempty_text(selected)
    if (nzchar(note) && options$format != "copy_ready") {
        note <- paste0("*", note, "*")
        selected <- c(selected, note)
    }
    if (options$format != "copy_ready" && !is.null(blocks$note) && nzchar(blocks$note))
        selected <- c(selected, paste0("*", blocks$note, "*"))
    if (options$format == "bullets")
        return(paste0("- ", selected, collapse = "\n"))
    paste(selected, collapse = "\n\n")
}

.jr_effect_interpretation_note <- function(analysis = NULL) {
    if (identical(analysis, "reliability_omega")) {
        return(paste(
            "Interpretation note: Reliability coefficient benchmarks are rough descriptive aids, not pass/fail rules.",
            "Interpret omega and alpha with item content, dimensionality, sample characteristics, and the intended use of the scale."
        ))
    }
    paste(
        "Interpretation note: Conventional benchmarks for effect sizes (e.g., Cohen's small, medium, and large guidelines) are intended as rough aids to interpretation rather than strict cut-offs.",
        "Values close to a boundary should not be interpreted differently simply because they fall on one side of a threshold.",
        "The practical importance of this effect should be considered in the context of the research area, measurement scale, and existing literature (Cohen, 1988; Cumming, 2014)."
    )
}

.jr_effect_size_name <- function(analysis) {
    switch(
        analysis,
        ttest = "Cohen's d",
        mann_whitney = "rank-biserial r",
        wilcoxon_signed_rank = "rank-biserial r",
        anova_oneway = "eta-squared",
        anova_between = "partial eta-squared",
        ancova = "partial eta-squared",
        anova_rm = "partial eta-squared",
        anova_mixed = "partial eta-squared",
        manova = "Pillai's trace",
        correlation = "correlation coefficient",
        regression = "R-squared",
        logistic_regression = "McFadden's R-squared",
        chisq_independence = "Cramer's V",
        chisq_gof = "Cohen's w",
        reliability_omega = "omega",
        ""
    )
}

.jr_effect_values <- function(x) {
    statistics <- x$statistics
    if (!is.data.frame(statistics) || nrow(statistics) == 0L)
        return(numeric())
    if (x$analysis == "correlation" && "statistic" %in% names(statistics))
        return(abs(statistics$statistic))
    if (x$analysis == "reliability_omega" && "estimate" %in% names(statistics))
        return(statistics$estimate)
    if (x$analysis %in% c("regression", "logistic_regression") && "r2" %in% names(statistics))
        return(statistics$r2)
    if ("effect" %in% names(statistics))
        return(abs(statistics$effect))
    numeric()
}

.jr_effect_magnitude <- function(analysis, value) {
    value <- abs(as.numeric(value))
    if (!is.finite(value))
        return("not benchmarked")
    cutoffs <- switch(
        analysis,
        ttest = c(.2, .5, .8),
        mann_whitney = c(.1, .3, .5),
        wilcoxon_signed_rank = c(.1, .3, .5),
        correlation = c(.1, .3, .5),
        anova_oneway = c(.01, .06, .14),
        anova_between = c(.01, .06, .14),
        ancova = c(.01, .06, .14),
        anova_rm = c(.01, .06, .14),
        anova_mixed = c(.01, .06, .14),
        chisq_gof = c(.1, .3, .5),
        chisq_independence = c(.1, .3, .5),
        regression = c(.02, .13, .26),
        logistic_regression = c(.02, .13, .26),
        reliability_omega = c(.5, .7, .8),
        manova = c(.01, .06, .14),
        NULL
    )
    if (is.null(cutoffs))
        return("not benchmarked")
    if (value < cutoffs[1]) "below small" else if (value < cutoffs[2]) "small" else if (value < cutoffs[3]) "medium" else "large"
}

.jr_effect_benchmark_text <- function(x) {
    if (identical(x$analysis, "ttest") && is.data.frame(x$statistics) &&
            nrow(x$statistics) == 1L &&
            x$statistics$test[1] %in% c("Student's t", "Welch's t"))
        return("")
    values <- .jr_effect_values(x)
    name <- .jr_effect_size_name(x$analysis)
    if (!length(values) || !nzchar(name))
        return("")
    magnitudes <- vapply(values, function(value) .jr_effect_magnitude(x$analysis, value), character(1))
    values <- vapply(values, .jr_num, character(1), digits = 2L, omit_zero = TRUE)
    if (identical(x$analysis, "reliability_omega") && "coefficient" %in% names(x$statistics)) {
        labels <- ifelse(
            x$statistics$coefficient == "Cronbach's alpha",
            "Cronbach's alpha",
            "omega"
        )
        summary <- paste(sprintf("%s = %s (%s)", labels, values, magnitudes), collapse = ", ")
        return(sprintf("Reliability benchmarks: %s.", summary))
    }
    if (length(values) == 1L)
        return(sprintf("Effect-size benchmark: %s = %s is conventionally interpreted as %s.", name, values, magnitudes))
    summary <- paste(sprintf("%s (%s)", values, magnitudes), collapse = ", ")
    sprintf("Effect-size benchmarks for %s: %s.", name, summary)
}

.jr_apply_inclusions <- function(text, analysis, include) {
    if (!"descriptives" %in% include && analysis == "ttest") {
        text <- gsub(
            ", with [^(]+ \\(n = [0-9]+, M = [^,]+, SD = [^)]+\\) scoring (?:higher|lower|the same) on .*? than [^(]+ \\(n = [0-9]+, M = [^,]+, SD = [^)]+\\)",
            "", text, perl = TRUE
        )
        text <- gsub(" between [^(]+ \\(M = [^)]*\\) and [^(]+ \\(M = [^)]*\\)",
                     " between the two groups", text, perl = TRUE)
        text <- gsub(" from [^(]+ \\(M = [^)]*\\) to [^(]+ \\(M = [^)]*\\)",
                     " between the two measurements", text, perl = TRUE)
    }
    if (!"descriptives" %in% include && analysis %in% c("mann_whitney", "bayes_ttest")) {
        text <- gsub(" between [^(]+ \\((?:M|Mdn) = [^)]*\\) and [^(]+ \\((?:M|Mdn) = [^)]*\\)",
                     " between the two groups", text, perl = TRUE)
    }
    if (!"descriptives" %in% include && analysis %in% c("wilcoxon_signed_rank", "bayes_ttest")) {
        text <- gsub(" from [^(]+ \\((?:M|Mdn) = [^)]*\\) to [^(]+ \\((?:M|Mdn) = [^)]*\\)",
                     " between the two measurements", text, perl = TRUE)
    }
    if (!"posthoc" %in% include && analysis == "anova_oneway")
        text <- gsub(" (Tukey|Holm)-adjusted[^.]*\\.", "", text, perl = TRUE)
    if (!"effect_size" %in% include) {
        if (analysis == "ttest") {
            text <- gsub(" The effect size was [^.]+, Cohen's d = -?[0-9.]+(?:, [0-9]+% CI \\[[^]]+\\])?\\.", "", text, perl = TRUE)
            text <- gsub(", Cohen's d = -?[0-9.]+(?:, [0-9]+% CI \\[[^]]+\\])?", "", text, perl = TRUE)
        }
        if (analysis %in% c("mann_whitney", "wilcoxon_signed_rank"))
            text <- gsub(", rank-biserial r = -?[0-9.]+(?:, [0-9]+% CI \\[[^]]+\\])?", "", text, perl = TRUE)
        if (analysis == "anova_oneway")
            text <- gsub(", eta-squared = -?[0-9.]+(?:, [0-9]+% CI \\[[^]]+\\])?", "", text, perl = TRUE)
        if (analysis %in% c("anova_between", "ancova", "anova_rm", "anova_mixed"))
            text <- gsub(", partial eta-squared = -?[0-9.]+(?:, [0-9]+% CI \\[[^]]+\\])?", "", text, perl = TRUE)
        if (analysis == "manova")
            text <- gsub(", Pillai's trace = -?[0-9.]+", "", text, perl = TRUE)
        if (analysis == "reliability_omega") {
            text <- gsub(", omega = -?[0-9.]+(?:, [0-9]+% bootstrap CI \\[[^]]+\\])?", "", text, perl = TRUE)
            text <- gsub(",? and Cronbach's alpha, alpha = -?[0-9.]+", "", text, perl = TRUE)
            text <- gsub("; Cronbach's alpha = -?[0-9.]+", "", text, perl = TRUE)
        }
        if (analysis == "regression") {
            text <- gsub(", R-squared = -?[0-9.]+, adjusted R-squared = -?[0-9.]+", "", text, perl = TRUE)
            text <- gsub(", beta = -?[0-9.]+", "", text, perl = TRUE)
        }
        if (analysis == "logistic_regression") {
            text <- gsub(", McFadden's R-squared = -?[0-9.]+", "", text, perl = TRUE)
            text <- gsub(", OR = [0-9.]+", "", text, perl = TRUE)
        }
        if (analysis == "chisq_independence")
            text <- gsub(", Cramer's V = -?[0-9.]+", "", text, perl = TRUE)
        if (analysis == "chisq_gof")
            text <- gsub(", Cohen's w = -?[0-9.]+", "", text, perl = TRUE)
    }
    if (!"ci" %in% include)
        text <- gsub(", [0-9]+% CI \\[[^]]+\\]", "", text, perl = TRUE)
    if (!"assumptions" %in% include && analysis == "ttest")
        text <- gsub(" Levene's test.*?(?:Student's|Welch's) t-test\\.", "", text, perl = TRUE)
    if (!"p" %in% include)
        text <- gsub(", p (?:= \\.[0-9]+|< \\.001)", "", text, perl = TRUE)
    if (!"df" %in% include)
        text <- gsub("([tF])\\([^)]*\\)", "\\1", text, perl = TRUE)
    if (!"df" %in% include && analysis == "logistic_regression")
        text <- gsub("chi-square\\([^)]*\\)", "chi-square", text, perl = TRUE)
    if (!"df" %in% include && analysis %in% c("chisq_independence", "chisq_gof"))
        text <- gsub("chi-square\\([^)]*\\)", "chi-square", text, perl = TRUE)
    if (!"test_statistic" %in% include) {
        text <- gsub(", t(?:\\([^)]*\\))? = -?[0-9.]+", "", text, perl = TRUE)
        text <- gsub(", F(?:\\([^)]*\\))? = -?[0-9.]+", "", text, perl = TRUE)
        if (analysis == "mann_whitney")
            text <- gsub(", U = -?[0-9.]+", "", text, perl = TRUE)
        if (analysis == "wilcoxon_signed_rank")
            text <- gsub(", W = -?[0-9.]+", "", text, perl = TRUE)
        if (analysis == "bayes_ttest")
            text <- gsub(" produced BF10 = [0-9.]+", "", text, perl = TRUE)
        if (analysis %in% c("logistic_regression", "chisq_independence", "chisq_gof")) {
            text <- gsub(", chi-square(?:\\([^)]*\\))? = -?[0-9.]+", "", text, perl = TRUE)
        }
        if (analysis == "logistic_regression") {
            text <- gsub(", z = -?[0-9.]+", "", text, perl = TRUE)
        }
        if (analysis == "correlation")
            text <- gsub(", (r|rho|tau) = -?[0-9.]+", "", text, perl = TRUE)
    }
    text
}

#' @export
print.edu_analysis <- function(x, ...) {
    cat(x$label, "\n", sep = "")
    cat(strrep("-", nchar(x$label)), "\n", sep = "")
    cat(edu_report(x, style = "apa7", format = "short"), "\n")
    invisible(x)
}
