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
    result <- structure(
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
    .jr_finalize_edu_analysis(result)
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
    model <- .jr_report_model(x)
    units <- model$narrativeUnits
    claim <- .jr_apply_inclusions(units$inferential, model$analysisType, options$include)

    if (options$format == "table_paragraph") {
        p_values <- model$p[is.finite(model$p)]
        evidence <- if (length(p_values) && any(p_values < .05)) {
            "The analysis provided evidence for at least one tested effect."
        } else if (length(p_values)) {
            "The analysis did not provide clear evidence for the tested effects in this sample."
        } else {
            "The numerical results are summarized in the accompanying table."
        }
        selected <- c(
            evidence,
            sprintf("The APA table reports the exact %s estimates without repeating every value here.", model$label)
        )
    } else {
        selected <- switch(
            options$style,
            plain = c(sprintf("What this analysis asks: %s", units$question), claim),
            journal = c(claim),
            dissertation = c(units$rationale, claim),
            c(claim)
        )

        if (options$tone %in% c("student_friendly", "detailed", "critical") &&
                !identical(options$style, "journal") &&
                !identical(options$format, "copy_ready") &&
                "interpretation" %in% options$include) {
            selected <- c(selected, units$explanation)
        }
        if (options$tone %in% c("detailed", "critical") &&
                "descriptives" %in% options$include) {
            selected <- c(units$descriptives, selected)
        }
        if (options$tone %in% c("detailed", "critical") &&
                "assumptions" %in% options$include) {
            selected <- c(selected, units$assumptions)
        }
    }

    warnings <- model$warnings
    if (nrow(warnings)) {
        show <- warnings$severity == "severe" |
            (options$tone %in% c("detailed", "critical") && "cautions" %in% options$include)
        selected <- c(selected, unique(warnings$message[show]))
    }
    if (options$tone == "critical" && "cautions" %in% options$include)
        selected <- c(selected, units$caution)
    if (options$tone %in% c("detailed", "critical") && options$format != "copy_ready" && nzchar(units$note))
        selected <- c(selected, paste0("*", units$note, "*"))

    selected <- .jr_nonempty_text(selected)
    if (options$format == "short") {
        severe <- if (nrow(warnings)) unique(warnings$message[warnings$severity == "severe"]) else character()
        selected <- unique(c(
            utils::head(selected, if (options$style == "apa7") 1L else 2L),
            severe
        ))
    }
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
        "The practical importance of an effect should be considered in the context of the research area, measurement scale, and existing literature (Cohen, 1988; Cumming, 2014)."
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
        if (analysis == "ttest")
            text <- gsub(", Cohen's d = -?[0-9.]+(?:, [0-9]+% CI \\[[^]]+\\])?", "", text, perl = TRUE)
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
