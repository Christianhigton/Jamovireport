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

.new_edu_analysis <- function(analysis, label, question, requirements, main,
                              descriptives, effects, diagnostics, interpretation,
                              caution, plot_data, report_blocks, statistics,
                              call = NULL) {
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

    if (options$style == "plain") {
        opening <- sprintf("What this analysis asks: %s", x$question)
        selected <- c(opening, blocks$plain)
        if ("assumptions" %in% options$include)
            selected <- c(selected, blocks$assumptions)
        if ("cautions" %in% options$include && nzchar(x$caution))
            selected <- c(selected, x$caution)
    } else if (options$format == "short") {
        selected <- blocks$apa
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

    selected <- selected[nzchar(selected)]
    if (options$format == "bullets")
        return(paste0("- ", selected, collapse = "\n"))
    paste(selected, collapse = "\n\n")
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
        if (analysis == "reliability_omega")
            text <- gsub(", omega = -?[0-9.]+(?:, [0-9]+% bootstrap CI \\[[^]]+\\])?", "", text, perl = TRUE)
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
