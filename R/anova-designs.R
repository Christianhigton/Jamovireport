.jr_term_effects <- function(table, df_error, ci = .95) {
    table <- as.data.frame(table)
    terms <- rownames(table)
    keep <- terms != "Residuals" & is.finite(table[["F value"]])
    table <- table[keep, , drop = FALSE]
    terms <- terms[keep]
    effects <- effectsize::F_to_eta2(table[["F value"]], table[["Df"]], df_error, ci = ci)
    data.frame(
        term = terms,
        statistic = table[["F value"]],
        df1 = table[["Df"]],
        df2 = df_error,
        p = table[["Pr(>F)"]],
        effect = effects$Eta2_partial,
        ci_low = effects$CI_low,
        ci_high = effects$CI_high,
        stringsAsFactors = FALSE
    )
}

.jr_effect_sentences <- function(statistics, ci = .95) {
    has_ges <- "ges" %in% names(statistics) && any(is.finite(statistics$ges))
    order <- order(grepl(":", statistics$term), decreasing = TRUE)
    paste(vapply(order, function(i) {
        ges_i <- if (has_ges) statistics$ges[i] else NA_real_
        effect_phrase <- if (is.finite(ges_i)) {
            sprintf(
                "\u03b7G\u00b2 = %s, \u03b7p\u00b2 = %s, %s%% CI %s",
                .jr_num(ges_i, 2L, TRUE),
                .jr_num(statistics$effect[i], 2L, TRUE),
                .jr_num(ci * 100, 0L),
                .jr_ci(statistics$ci_low[i], statistics$ci_high[i], 2L, TRUE)
            )
        } else {
            sprintf(
                "\u03b7p\u00b2 = %s, %s%% CI %s",
                .jr_num(statistics$effect[i], 2L, TRUE),
                .jr_num(ci * 100, 0L),
                .jr_ci(statistics$ci_low[i], statistics$ci_high[i], 2L, TRUE)
            )
        }
        sprintf(
            "%s %s, F(%s, %s) = %s, p %s, %s.",
            statistics$term[i],
            if (statistics$p[i] < .05) "was statistically significant" else "was not statistically significant",
            .jr_num(statistics$df1[i], 2L), .jr_num(statistics$df2[i], 2L),
            .jr_num(statistics$statistic[i]), .jr_p(statistics$p[i]),
            effect_phrase
        )
    }, character(1)), collapse = " ")
}

.jr_factor_descriptives <- function(data, outcome, factors) {
    cell <- interaction(data[, factors, drop = FALSE], sep = " x ", drop = TRUE)
    split_y <- split(data[[outcome]], cell)
    data.frame(
        group = names(split_y),
        n = vapply(split_y, length, integer(1)),
        mean = vapply(split_y, mean, numeric(1)),
        sd = vapply(split_y, stats::sd, numeric(1)),
        stringsAsFactors = FALSE
    )
}

.jr_factor_formula <- function(outcome, factors, covariates = character()) {
    .jr_formula(outcome, rhs = covariates, factorial = factors)
}

#' Guided between-subjects factorial ANOVA
#'
#' @param data A data frame.
#' @param outcome Numeric dependent variable name.
#' @param factors Character vector of categorical between-subjects factor names.
#' @param ci Confidence level for partial eta-squared intervals.
#' @return An `edu_analysis` object.
#' @export
edu_anova_between <- function(data, outcome, factors, ci = .95) {
    if (length(factors) == 0L)
        .jr_stop("`factors` must contain at least one between-subjects factor.")
    .jr_assert_columns(data, c(outcome, factors))
    d <- .jr_complete(data, c(outcome, factors))
    .jr_numeric(d[[outcome]], outcome)
    for (factor_name in factors)
        d[[factor_name]] <- droplevels(factor(d[[factor_name]]))
    model <- stats::lm(.jr_factor_formula(outcome, factors), data = d)
    statistics <- .jr_term_effects(car::Anova(model, type = 2), stats::df.residual(model), ci)
    d$.cell <- interaction(d[, factors, drop = FALSE], sep = " x ", drop = TRUE)
    d$.outcome <- d[[outcome]]
    diagnostics <- rbind(
        .jr_shapiro(stats::residuals(model), "Residual normality (Shapiro-Wilk)"),
        .jr_levene(".outcome", ".cell", d)
    )
    descriptives <- .jr_factor_descriptives(d, outcome, factors)
    significant_interaction <- any(statistics$p[grepl(":", statistics$term)] < .05)
    plain <- if (significant_interaction) {
        "A statistically significant interaction was identified. Interpret how the effect of one factor changes across levels of the other factor before focusing on main effects."
    } else if (any(statistics$p < .05)) {
        "At least one between-subjects factor was associated with differences in average outcome scores; no significant interaction requires priority in interpretation."
    } else {
        "The model did not provide clear evidence of between-group mean differences or interactions in this sample."
    }
    apa <- paste(
        sprintf("A between-subjects ANOVA examined %s as a function of %s.",
                outcome, paste(factors, collapse = " and ")),
        .jr_effect_sentences(statistics, ci)
    )
    assumption_text <- .jr_diagnostic_text(diagnostics)
    caution <- if (any(diagnostics$status == "Caution")) paste("Caution:", assumption_text) else ""
    result <- .new_edu_analysis(
        analysis = "anova_between", label = "Between-Subjects ANOVA",
        question = sprintf("Do average %s scores differ across %s or their interaction?", outcome, paste(factors, collapse = " and ")),
        requirements = "A numeric outcome and independent groups defined by one or more categorical between-subjects factors.",
        main = statistics, descriptives = descriptives, effects = statistics,
        diagnostics = diagnostics, interpretation = plain, caution = caution,
        plot_data = data.frame(outcome = d[[outcome]], group = d$.cell),
        report_blocks = list(
            rationale = sprintf("A between-subjects ANOVA compares mean %s values across factorial groups.", outcome),
            descriptives = plain, apa = apa, assumptions = assumption_text, plain = plain
        ),
        statistics = statistics, call = match.call()
    )
    result$model <- model
    result
}

#' Guided analysis of covariance
#'
#' @param data A data frame.
#' @param outcome Numeric dependent variable name.
#' @param factors Character vector of categorical group variables.
#' @param covariates Character vector of numeric covariates.
#' @param ci Confidence level.
#' @return An `edu_analysis` object.
#' @export
edu_ancova <- function(data, outcome, factors, covariates, ci = .95) {
    if (length(factors) == 0L || length(covariates) == 0L)
        .jr_stop("ANCOVA requires at least one factor and one covariate.")
    .jr_assert_columns(data, c(outcome, factors, covariates))
    d <- .jr_complete(data, c(outcome, factors, covariates))
    .jr_numeric(d[[outcome]], outcome)
    for (name in covariates)
        .jr_numeric(d[[name]], name)
    for (name in factors)
        d[[name]] <- droplevels(factor(d[[name]]))
    for (name in factors) {
        lvls <- levels(d[[name]])
        if (length(lvls) < 2L)
            .jr_stop(sprintf("Factor '%s' has only one level in the data after removing missing values. ANCOVA requires at least two groups.", name))
    }
    n_params <- length(factors) + length(covariates) +
        sum(vapply(factors, function(f) length(levels(d[[f]])) - 1L, integer(1)))
    if (nrow(d) <= n_params + 1L)
        .jr_stop(sprintf(
            "Not enough complete observations (%d) to fit this model. Need at least %d rows. Try reducing the number of factors or covariates, or collect more data.",
            nrow(d), n_params + 2L
        ))
    formula <- .jr_factor_formula(outcome, factors, covariates)
    model <- stats::lm(formula, data = d)
    if (stats::df.residual(model) == 0L)
        .jr_stop("The model uses all available degrees of freedom (residual df = 0). Reduce the number of predictors or collect more data.")
    anova_table <- tryCatch(
        car::Anova(model, type = 2),
        error = function(e) {
            msg <- conditionMessage(e)
            if (grepl("residual sum of squares is 0", msg, fixed = TRUE))
                .jr_stop("The model fits the data perfectly, leaving no residual variance. Check whether a covariate is perfectly correlated with the outcome, or whether the outcome has no real variability within groups.")
            stop(e)
        }
    )
    statistics <- .jr_term_effects(anova_table, stats::df.residual(model), ci)
    factorial_expression <- .jr_formula_term(factors, operator = "*")
    slope_expressions <- lapply(covariates, function(covariate) {
        call(":", factorial_expression, .jr_formula_symbol(covariate))
    })
    base_expressions <- c(
        list(factorial_expression),
        lapply(covariates, .jr_formula_symbol)
    )
    slope_formula <- .jr_formula_from_expressions(
        outcome,
        c(base_expressions, slope_expressions)
    )
    slope_model <- tryCatch(
        stats::lm(slope_formula, data = d),
        error = function(e) NULL
    )
    slopes <- if (!is.null(slope_model)) stats::anova(model, slope_model) else NULL
    slope_p <- if (!is.null(slopes)) slopes[["Pr(>F)"]][2] else NA_real_
    d$.cell <- interaction(d[, factors, drop = FALSE], sep = " x ", drop = TRUE)
    d$.residual <- stats::residuals(model)
    diagnostics <- rbind(
        .jr_shapiro(stats::residuals(model), "Residual normality (Shapiro-Wilk)"),
        .jr_levene(".residual", ".cell", d),
        data.frame(
            check = "Homogeneity of regression slopes",
            statistic = slopes[["F"]][2], p = slope_p,
            status = if (!is.na(slope_p) && slope_p < .05) "Serious" else "Acceptable",
            interpretation = if (!is.na(slope_p) && slope_p < .05)
                "The relationship between the covariate and outcome differs across groups."
            else "No clear evidence that covariate slopes differ across groups was detected.",
            action = if (!is.na(slope_p) && slope_p < .05)
                "Do not interpret a single adjusted group effect without modelling the interaction."
            else "Adjusted group comparisons are interpretable alongside other diagnostics.",
            stringsAsFactors = FALSE
        )
    )
    descriptives <- .jr_factor_descriptives(d, outcome, factors)
    factor_rows <- statistics$term %in% factors | grepl(":", statistics$term)
    factor_stats <- statistics[factor_rows, , drop = FALSE]
    apa <- paste(
        sprintf("After adjusting for %s, an ANCOVA examined %s across %s.",
                paste(covariates, collapse = " and "), outcome, paste(factors, collapse = " and ")),
        .jr_effect_sentences(factor_stats, ci)
    )
    if (!is.na(slope_p) && slope_p < .05) {
        apa <- paste(
            apa,
            "The homogeneity of regression slopes assumption was not met, so this adjusted group effect should not be interpreted without modelling the group-by-covariate interaction."
        )
    }
    plain <- if (!is.na(slope_p) && slope_p < .05) {
        "The adjusted comparison requires caution because the covariate-outcome relationship is not comparable across groups."
    } else if (any(factor_stats$p < .05)) {
        "After accounting for the covariate, at least one group effect remained statistically significant."
    } else {
        "After accounting for the covariate, no clear adjusted group difference was detected."
    }
    assumption_text <- .jr_diagnostic_text(diagnostics)
    caution <- if (any(diagnostics$status %in% c("Caution", "Serious"))) paste("Caution:", assumption_text) else ""
    result <- .new_edu_analysis(
        analysis = "ancova", label = "ANCOVA",
        question = sprintf("Do average %s scores differ across %s after accounting for %s?", outcome, paste(factors, collapse = " and "), paste(covariates, collapse = " and ")),
        requirements = "A numeric outcome, categorical independent groups, and numeric covariates measured before or independently of group effects.",
        main = statistics, descriptives = descriptives, effects = statistics,
        diagnostics = diagnostics, interpretation = plain, caution = caution,
        plot_data = data.frame(outcome = d[[outcome]], group = d$.cell),
        report_blocks = list(
            rationale = sprintf("ANCOVA compares adjusted mean %s values while accounting for %s.", outcome, paste(covariates, collapse = " and ")),
            descriptives = plain, apa = apa, assumptions = assumption_text, plain = plain
        ),
        statistics = statistics, call = match.call()
    )
    result$model <- model
    result$slope_model <- slope_model
    result
}

.jr_within_long <- function(data, measures, levels, group = NULL) {
    d <- .jr_complete(data, c(measures, group))
    for (measure in measures)
        .jr_numeric(d[[measure]], measure)
    if (!is.null(group))
        d[[group]] <- droplevels(factor(d[[group]]))
    n <- nrow(d)
    long <- data.frame(
        id = factor(rep(seq_len(n), each = length(measures))),
        occasion = factor(rep(levels, times = n), levels = levels),
        outcome = as.vector(t(as.matrix(d[, measures, drop = FALSE]))),
        stringsAsFactors = FALSE
    )
    if (!is.null(group))
        long$group <- factor(rep(d[[group]], each = length(measures)))
    long
}

.jr_sphericity <- function(afex_result, term = "occasion", n_levels) {
    if (n_levels <= 2L) {
        return(data.frame(
            check = "Sphericity", statistic = NA_real_, p = NA_real_,
            status = "Not required",
            interpretation = "Sphericity is not required when the within-subject factor has two levels.",
            action = "Interpret the within-subject test directly.",
            stringsAsFactors = FALSE
        ))
    }
    summary_result <- suppressWarnings(summary(afex_result$Anova, multivariate = FALSE))
    tests <- summary_result$sphericity.tests
    row <- which(rownames(tests) == term)[1]
    if (is.na(row)) {
        return(data.frame(
            check = "Sphericity", statistic = NA_real_, p = NA_real_,
            status = "Not assessed", interpretation = "A sphericity test could not be extracted.",
            action = "Retain the Greenhouse-Geisser-corrected result reported in the main table.",
            stringsAsFactors = FALSE
        ))
    }
    p <- tests[row, "p-value"]
    data.frame(
        check = "Sphericity (Mauchly)",
        statistic = tests[row, "Test statistic"], p = p,
        status = if (p < .05) "Caution" else "Acceptable",
        interpretation = if (p < .05) "Sphericity appears to be violated." else "No clear sphericity violation was detected.",
        action = "Greenhouse-Geisser-corrected degrees of freedom are reported for within-subject effects.",
        stringsAsFactors = FALSE
    )
}

.jr_afex_statistics <- function(fit, ci = .95) {
    table <- as.data.frame(fit$anova_table)
    effects <- effectsize::F_to_eta2(table$F, table[["num Df"]], table[["den Df"]], ci = ci)
    ges <- tryCatch({
        g <- suppressMessages(
            as.data.frame(effectsize::eta_squared(fit, generalized = TRUE, ci = ci))
        )
        col <- if ("Eta2_generalized" %in% names(g)) "Eta2_generalized"
               else if ("Eta2" %in% names(g)) "Eta2"
               else NA_character_
        if (is.na(col)) {
            rep(NA_real_, nrow(table))
        } else {
            terms <- rownames(table)
            param_col <- if ("Parameter" %in% names(g)) g$Parameter
                         else if ("Group" %in% names(g)) g$Group
                         else rownames(g)
            vals <- g[[col]][match(terms, param_col)]
            if (all(is.na(vals))) rep(NA_real_, nrow(table)) else vals
        }
    }, error = function(e) rep(NA_real_, nrow(table)))
    data.frame(
        term = rownames(table), statistic = table$F,
        df1 = table[["num Df"]], df2 = table[["den Df"]],
        p = table[["Pr(>F)"]], effect = table$pes,
        ges = ges,
        ci_low = effects$CI_low, ci_high = effects$CI_high, stringsAsFactors = FALSE
    )
}

.jr_rm_descriptives <- function(long, mixed = FALSE) {
    keys <- if (mixed) interaction(long$group, long$occasion, sep = " x ", drop = TRUE) else long$occasion
    split_y <- split(long$outcome, keys)
    data.frame(
        group = names(split_y), n = vapply(split_y, length, integer(1)),
        mean = vapply(split_y, mean, numeric(1)), sd = vapply(split_y, stats::sd, numeric(1)),
        stringsAsFactors = FALSE
    )
}

#' Guided repeated-measures ANOVA
#'
#' @param data A wide-format data frame with one row per participant.
#' @param measures Character vector of repeated numeric measurement columns.
#' @param levels Optional meaningful labels for measurement occasions.
#' @param ci Confidence level retained for report interface consistency.
#' @return An `edu_analysis` object.
#' @export
edu_anova_rm <- function(data, measures, levels = measures, ci = .95) {
    if (length(measures) < 2L || length(levels) != length(measures))
        .jr_stop("Repeated-measures ANOVA requires two or more measurement columns and matching labels.")
    .jr_assert_columns(data, measures)
    long <- .jr_within_long(data, measures, levels)
    fit <- suppressMessages(afex::aov_ez(
        id = "id", dv = "outcome", data = long, within = "occasion",
        anova_table = list(correction = "GG", es = "pes")
    ))
    statistics <- .jr_afex_statistics(fit, ci)
    diagnostics <- rbind(
        .jr_shapiro(as.vector(stats::residuals(fit$lm)), "Residual normality (Shapiro-Wilk)"),
        .jr_sphericity(fit, "occasion", length(measures))
    )
    descriptives <- .jr_rm_descriptives(long)
    apa <- paste(
        "A repeated-measures ANOVA with Greenhouse-Geisser-corrected within-subject inference examined change across occasions.",
        .jr_effect_sentences(statistics, ci)
    )
    plain <- if (statistics$p[1] < .05)
        "Average scores changed across the repeated measurement occasions."
    else "No clear change in average scores was detected across measurement occasions."
    assumption_text <- .jr_diagnostic_text(diagnostics)
    caution <- if (any(diagnostics$status == "Caution")) paste("Caution:", assumption_text) else ""
    result <- .new_edu_analysis(
        analysis = "anova_rm", label = "Repeated-Measures ANOVA",
        question = "Do average scores change across repeated measurement occasions?",
        requirements = "Two or more numeric measurements from the same participants, stored in wide format with one row per participant.",
        main = statistics, descriptives = descriptives, effects = statistics,
        diagnostics = diagnostics, interpretation = plain, caution = caution,
        plot_data = long[, c("occasion", "outcome")],
        report_blocks = list(
            rationale = "A repeated-measures ANOVA compares related means measured on the same participants.",
            descriptives = plain, apa = apa, assumptions = assumption_text, plain = plain
        ),
        statistics = statistics, call = match.call()
    )
    result$model <- fit
    result
}

#' Guided mixed ANOVA
#'
#' @param data A wide-format data frame with one row per participant.
#' @param measures Character vector of repeated numeric measurement columns.
#' @param group Categorical between-subjects factor name.
#' @param levels Optional meaningful labels for measurement occasions.
#' @param ci Confidence level.
#' @return An `edu_analysis` object.
#' @export
edu_anova_mixed <- function(data, measures, group, levels = measures, ci = .95) {
    if (length(measures) < 2L || length(levels) != length(measures))
        .jr_stop("Mixed ANOVA requires two or more measurement columns and matching labels.")
    .jr_assert_columns(data, c(measures, group))
    long <- .jr_within_long(data, measures, levels, group)
    fit <- suppressMessages(afex::aov_ez(
        id = "id", dv = "outcome", data = long, within = "occasion", between = "group",
        anova_table = list(correction = "GG", es = "pes")
    ))
    statistics <- .jr_afex_statistics(fit, ci)
    diagnostics <- rbind(
        .jr_shapiro(as.vector(stats::residuals(fit$lm)), "Residual normality (Shapiro-Wilk)"),
        .jr_sphericity(fit, "occasion", length(measures))
    )
    descriptives <- .jr_rm_descriptives(long, mixed = TRUE)
    interaction <- statistics[statistics$term == "group:occasion", , drop = FALSE]
    apa <- paste(
        "A mixed ANOVA with Greenhouse-Geisser-corrected within-subject inference examined group differences over occasions.",
        .jr_effect_sentences(statistics, ci)
    )
    plain <- if (nrow(interaction) == 1L && interaction$p < .05)
        "The pattern of change over occasions differed between groups; interpret the interaction before the separate main effects."
    else "The analysis estimates overall group differences, change over occasions, and whether change differs between groups."
    assumption_text <- .jr_diagnostic_text(diagnostics)
    caution <- if (any(diagnostics$status == "Caution")) paste("Caution:", assumption_text) else ""
    result <- .new_edu_analysis(
        analysis = "anova_mixed", label = "Mixed ANOVA",
        question = "Do scores change over occasions, differ between groups, or show different patterns of change by group?",
        requirements = "Repeated numeric measurements for each participant and an independent categorical between-subjects grouping variable.",
        main = statistics, descriptives = descriptives, effects = statistics,
        diagnostics = diagnostics, interpretation = plain, caution = caution,
        plot_data = long[, c("occasion", "outcome", "group")],
        report_blocks = list(
            rationale = "A mixed ANOVA combines within-participant change with between-group comparisons.",
            descriptives = plain, apa = apa, assumptions = assumption_text, plain = plain
        ),
        statistics = statistics, call = match.call()
    )
    result$model <- fit
    result
}
