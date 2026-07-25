.jr_has_follow_up_rows <- function(result) {
    any(vapply(c("posthoc", "posthoc_report"), function(name) {
        value <- result[[name]]
        !is.null(value) && (
            (is.data.frame(value) && nrow(value) > 0L) ||
            (!is.data.frame(value) && length(value) > 0L)
        )
    }, logical(1)))
}

.jr_significant_terms <- function(result, alpha = .05) {
    statistics <- result$statistics
    if (!is.data.frame(statistics) || nrow(statistics) == 0L ||
            !"p" %in% names(statistics))
        return(character())
    keep <- is.finite(statistics$p) & statistics$p < alpha
    if (!any(keep))
        return(character())
    labels <- if ("term" %in% names(statistics)) {
        as.character(statistics$term)
    } else if ("test" %in% names(statistics)) {
        as.character(statistics$test)
    } else {
        rep(result$label %||% "omnibus effect", nrow(statistics))
    }
    unique(labels[keep & nzchar(labels)])
}

.jr_factor_level_count <- function(result) {
    counts <- integer()
    model <- result$model
    if (!is.null(model)) {
        xlevels <- tryCatch(model$xlevels, error = function(e) NULL)
        if (is.null(xlevels))
            xlevels <- tryCatch(model$lm$xlevels, error = function(e) NULL)
        if (length(xlevels))
            counts <- c(counts, lengths(xlevels))
    }
    plot_data <- result$plot_data
    if (is.data.frame(plot_data)) {
        candidates <- intersect(
            c("group", "occasion", "row", "column", "category"),
            names(plot_data)
        )
        counts <- c(counts, vapply(candidates, function(name) {
            length(unique(plot_data[[name]][!is.na(plot_data[[name]])]))
        }, integer(1)))
    }
    counts[counts > 0L]
}

.jr_follow_up_reference_text <- function(analysis) {
    general <- c(
        "Maxwell, S. E., Delaney, H. D., & Kelley, K. (2018). Designing Experiments and Analyzing Data: A Model Comparison Perspective (3rd ed.). Routledge.",
        "Lenth, R. V. (2016). Least-Squares Means: The R Package lsmeans. Journal of Statistical Software, 69(1), 1–33. https://doi.org/10.18637/jss.v069.i01",
        "Holm, S. (1979). A simple sequentially rejective multiple test procedure. Scandinavian Journal of Statistics, 6(2), 65–70."
    )
    if (analysis %in% c("chisq_independence", "chisq_gof")) {
        return(c(
            "Agresti, A. (2019). An Introduction to Categorical Data Analysis (3rd ed.). Wiley.",
            general[3]
        ))
    }
    if (identical(analysis, "manova")) {
        return(c(
            "Huberty, C. J., & Olejnik, S. (2006). Applied MANOVA and Discriminant Analysis (2nd ed.). Wiley.",
            general
        ))
    }
    general
}

.jr_follow_up_analysis_guidance <- function(result, alpha = .05) {
    if (!inherits(result, "edu_analysis"))
        return("")
    supported <- c(
        "anova_oneway", "anova_between", "ancova", "anova_rm",
        "anova_mixed", "manova", "chisq_independence", "chisq_gof"
    )
    analysis <- result$analysis %||% ""
    if (!analysis %in% supported)
        return("")
    terms <- .jr_significant_terms(result, alpha)
    if (!length(terms))
        return("")

    levels <- .jr_factor_level_count(result)
    has_interaction <- any(grepl(":", terms, fixed = TRUE))
    more_than_two <- any(levels > 2L)
    categorical_follow_up <- if (identical(analysis, "chisq_independence")) {
        observed <- result$observed
        !is.null(observed) && length(dim(observed)) == 2L &&
            (nrow(observed) > 2L || ncol(observed) > 2L)
    } else if (identical(analysis, "chisq_gof")) {
        is.data.frame(result$cells) && nrow(result$cells) > 2L
    } else {
        FALSE
    }
    multivariate_follow_up <- identical(analysis, "manova")
    needs_follow_up <- if (analysis %in% c(
            "chisq_independence", "chisq_gof"
        )) {
        categorical_follow_up
    } else {
        has_interaction || more_than_two || multivariate_follow_up
    }

    if (!needs_follow_up)
        return("")
    if (!multivariate_follow_up && !categorical_follow_up &&
            .jr_has_follow_up_rows(result))
        return("")

    term_text <- paste(terms, collapse = ", ")
    opening <- sprintf(
        paste(
            "Why this appears: The omnibus %s result was statistically significant for %s.",
            "An omnibus result shows that a difference or association is present, but it does not necessarily identify the comparisons or pattern that answer the research question."
        ),
        result$label %||% analysis, term_text
    )
    decision <- switch(
        analysis,
        anova_oneway = paste(
            "Human decision required: Select planned contrasts or pairwise group comparisons that follow from the research question.",
            "If every pair is genuinely of interest, an all-pairs method such as Tukey may be suitable; otherwise specify the smaller, theoretically justified family of comparisons."
        ),
        anova_between = paste(
            "Human decision required: For a significant interaction, choose simple-effects or interaction contrasts before interpreting main effects.",
            "For a significant factor with more than two levels, choose planned contrasts or justified pairwise comparisons."
        ),
        ancova = paste(
            "Human decision required: Choose comparisons of adjusted means or planned contrasts that match the research question and use meaningful covariate values.",
            "If homogeneity of regression slopes is doubtful, model and interpret the factor-by-covariate interaction before comparing adjusted means."
        ),
        anova_rm = paste(
            "Human decision required: Choose planned within-participant contrasts or paired comparisons between occasions.",
            "The selected comparisons should reflect the expected pattern of change and account for their dependence and multiplicity."
        ),
        anova_mixed = paste(
            "Human decision required: If the interaction is significant, choose simple effects such as group comparisons at particular occasions or occasion comparisons within particular groups.",
            "The choice should be driven by the research question rather than by whichever comparison produces the smallest p-value."
        ),
        manova = paste(
            "Human decision required: Decide which outcomes and group contrasts should be examined after the multivariate result.",
            "Any displayed univariate follow-ups are screening information; they do not automatically determine the scientifically relevant outcome comparisons or replace multiplicity control."
        ),
        chisq_independence = paste(
            "Human decision required: Inspect standardised residuals and select theoretically meaningful cell or proportion comparisons to locate the association.",
            "Do not treat every cell as a separate unadjusted significance test."
        ),
        chisq_gof = paste(
            "Human decision required: Inspect residuals and choose justified category comparisons to identify how the observed distribution differs from expectation.",
            "The expected proportions and the family of follow-up comparisons should be specified substantively."
        )
    )
    multiplicity <- paste(
        "Control multiplicity across the coherent family of follow-up tests.",
        "Holm adjustment is a broadly applicable familywise-error procedure, but the most appropriate method depends on whether the comparisons were planned, whether all pairs are of interest, and the assumptions of the model.",
        "Report the selected comparisons, adjustment method, effect estimates, confidence intervals, and the rationale for those choices."
    )
    boundary <- paste(
        "jReport has not selected these follow-ups automatically because the software cannot infer the study's hypotheses, the scientifically meaningful contrasts, or which tests form one comparison family.",
        "Use the relevant jamovi post-hoc, estimated-marginal-means, contrast, or residual options after making those decisions."
    )
    references <- paste(
        "References:",
        paste(.jr_follow_up_reference_text(analysis), collapse = "\n"),
        sep = "\n"
    )
    paste(opening, decision, multiplicity, boundary, references, sep = "\n\n")
}
