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
    replace_literal <- function(value, pattern, replacement) {
        if (is.na(value))
            return(value)
        matches <- gregexpr(pattern, value, perl = TRUE)[[1]]
        if (length(matches) == 1L && matches[1] < 0L)
            return(value)
        lengths <- attr(matches, "match.length")
        pieces <- character()
        cursor <- 1L
        for (i in seq_along(matches)) {
            pieces <- c(
                pieces,
                substr(value, cursor, matches[i] - 1L),
                replacement
            )
            cursor <- matches[i] + lengths[i]
        }
        paste0(c(pieces, substr(value, cursor, nchar(value))), collapse = "")
    }
    variables <- names(labels)[labels != names(labels) & nzchar(labels)]
    variables <- variables[order(nchar(variables), decreasing = TRUE)]
    for (variable in variables) {
        pattern <- paste0(
            "(?<![[:alnum:]_.])", .jr_regex_escape(variable),
            "(?![[:alnum:]_.])"
        )
        text <- vapply(
            text,
            replace_literal,
            pattern = pattern,
            replacement = labels[[variable]],
            FUN.VALUE = character(1),
            USE.NAMES = FALSE
        )
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


.jr_guided_report_sections_html <- function(result, options) {
    args <- .jr_jamovi_report_args(options)
    .jr_build_report_sections_html(
        apa_wording             = do.call(edu_report, c(list(x = result), args)),
        report_style            = args$style
    )
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
    args <- if (is.null(options)) {
        .jr_jamovi_report_args(.jr_addon_reporting_options())
    } else {
        .jr_jamovi_report_args(options)
    }
    include <- args$include
    apa <- .jr_anova_between_apa_text(result, include, posthoc_text)
    if (!identical(args$style, "apa7") || !identical(args$format, "paragraph") ||
            !identical(args$tone, "student_friendly")) {
        apa <- do.call(edu_report, c(list(x = result), args))
    }
    .jr_build_report_sections_html(
        apa_wording = apa,
        report_style = args$style
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
    report_style <- "apa7"
    if (!is.null(options)) {
        args <- .jr_jamovi_report_args(options)
        include <- args$include
        report_style <- args$style
        copy_ready_text <- .jr_apply_inclusions(copy_ready_text, result$analysis, include)
    }
    .jr_build_report_sections_html(
        apa_wording = copy_ready_text,
        report_style = report_style
    )
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
            "Effect size (\u03b7\u00b2) matches jamovi output.",
            "Post hoc comparisons (if run) match jamovi output.",
            "Assumption checks (normality, homogeneity of variance) have been reviewed.",
            "Interpretation matches the research question."
        ),
        ancova = c(
            "Outcome, grouping factor, and covariate(s) are correctly specified.",
            "F statistics, df, and p values match jamovi output.",
            "Effect sizes (\u03b7p\u00b2) match jamovi output.",
            "Covariate(s) are measured before the intervention or are not affected by group.",
            "Assumption checks have been reviewed.",
            "Interpretation matches the research question."
        ),
        anova_rm = c(
            "Within-subjects factor levels and outcome columns are correctly mapped.",
            "F statistic, df (with Greenhouse-Geisser correction if applied), and p value match jamovi output.",
            "Effect size (\u03b7p\u00b2) matches jamovi output.",
            "Sphericity assumption and correction have been noted if applicable.",
            "Post hoc comparisons (if run) match jamovi output.",
            "Interpretation matches the research question."
        ),
        anova_mixed = c(
            "Between-subjects and within-subjects factors are correctly specified.",
            "F statistics, df, and p values for all effects match jamovi output.",
            "Effect sizes (\u03b7p\u00b2) match jamovi output.",
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
            "McFadden's R\u00b2 matches jamovi output.",
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
        multinomial_logistic = c(
            "Outcome variable has 3 or more nominal categories.",
            "Reference category is correctly identified.",
            "Predictors are the intended covariates and factors.",
            "Chi-square model fit statistic, df, and p value match jamovi output.",
            "McFadden's R\u00b2 matches jamovi output.",
            "Relative risk ratios and confidence intervals match the coefficient table.",
            "Assumption checks (convergence, sample size per category) have been reviewed.",
            "Interpretation matches the research question."
        ),
        regression = c(
            "Outcome variable is the intended dependent variable.",
            "Predictors are the intended covariates and factors.",
            "Categorical predictors are coded with the correct groups and reference levels.",
            "Model fit statistics are copied from the final jamovi output.",
            "R-squared and adjusted R-squared match the final model.",
            "F statistic and degrees of freedom match the final model.",
            "b, SE, beta, t, p, and confidence intervals match the coefficient table.",
            "Assumption checks have been reviewed.",
            "Interpretation matches the research question."
        ),
        c(
            "All reported statistics match jamovi output.",
            "Assumption checks have been reviewed.",
            "Interpretation matches the research question."
        )
    )
}
