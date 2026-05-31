.jr_multivariate_formula <- function(outcomes, factors, covariates) {
    response <- sprintf("cbind(%s)", paste(outcomes, collapse = ", "))
    stats::reformulate(c(factors, covariates), response = response)
}

.jr_pillai_statistics <- function(model) {
    table <- as.data.frame(summary(model, test = "Pillai")$stats)
    table <- table[rownames(table) != "Residuals", , drop = FALSE]
    data.frame(
        term = rownames(table),
        statistic = table[["approx F"]],
        df1 = table[["num Df"]],
        df2 = table[["den Df"]],
        p = table[["Pr(>F)"]],
        effect = table[["Pillai"]],
        ci_low = NA_real_,
        ci_high = NA_real_,
        stringsAsFactors = FALSE
    )
}

.jr_manova_univariate_formula <- function(outcome, factors, covariates) {
    terms <- c(
        if (length(factors) > 0L) paste(factors, collapse = " * ") else character(),
        covariates
    )
    stats::reformulate(terms, response = outcome)
}

.jr_manova_followups <- function(data, outcomes, factors, covariates, statistics, ci = .95) {
    significant <- statistics[
        is.finite(statistics$p) & statistics$p < .05 &
            is.finite(statistics$effect) & is.finite(statistics$statistic),
        ,
        drop = FALSE
    ]
    if (nrow(significant) == 0L)
        return(data.frame())

    rows <- list()
    for (outcome in outcomes) {
        model <- stats::lm(.jr_manova_univariate_formula(outcome, factors, covariates), data = data)
        table <- tryCatch(car::Anova(model, type = 2), error = function(e) NULL)
        if (is.null(table))
            next
        univariate <- .jr_term_effects(table, stats::df.residual(model), ci = ci)
        for (term in significant$term) {
            row <- univariate[univariate$term == term, , drop = FALSE]
            if (nrow(row) != 1L)
                next
            rows[[length(rows) + 1L]] <- data.frame(
                term = term,
                outcome = outcome,
                statistic = row$statistic,
                df1 = row$df1,
                df2 = row$df2,
                p = row$p,
                p_holm = NA_real_,
                effect = row$effect,
                ci_low = row$ci_low,
                ci_high = row$ci_high,
                stringsAsFactors = FALSE
            )
        }
    }
    if (length(rows) == 0L)
        return(data.frame())
    followups <- do.call(rbind, rows)
    followups$p_holm <- stats::p.adjust(followups$p, method = "holm")
    followups[order(followups$term, -followups$effect, followups$p_holm), , drop = FALSE]
}

.jr_manova_followup_note <- function() {
    paste(
        "These follow-up analyses were conducted automatically following a significant omnibus MANOVA/MANCOVA and should be considered exploratory.",
        "Researchers who specified hypotheses before data collection may prefer planned contrasts (a priori comparisons), which directly test theoretical predictions and are typically more powerful than post hoc procedures.",
        "Because the software cannot determine whether hypotheses were specified in advance, Holm-adjusted follow-up analyses are reported by default (Field, 2024)."
    )
}

.jr_manova_followup_text <- function(followups) {
    if (!is.data.frame(followups) || nrow(followups) == 0L)
        return("")
    by_term <- split(followups, followups$term)
    summaries <- vapply(by_term, function(rows) {
        rows <- rows[order(-rows$effect, rows$p_holm), , drop = FALSE]
        strongest_n <- if (nrow(rows) > 2L) 2L else 1L
        strongest <- utils::head(rows$outcome, strongest_n)
        weakest <- rows$outcome[nrow(rows)]
        evidence <- if (nrow(rows) == 1L)
            sprintf("The strongest evidence was observed for %s.", strongest)
        else
            sprintf(
                "The strongest evidence was observed for %s. Little evidence was found for %s.",
                paste(strongest, collapse = ", followed by "),
                weakest
            )
        sprintf(
            "For %s, follow-up univariate analyses were conducted to identify which outcomes contributed to the significant multivariate effect. P-values were adjusted using the Holm procedure to control the family-wise error rate across follow-up tests. %s",
            rows$term[1],
            evidence
        )
    }, character(1))
    paste("Follow-up analyses:", paste(summaries, collapse = " "))
}

#' Guided multivariate analysis of variance or covariance
#'
#' @param data A data frame.
#' @param outcomes Character vector containing two or more numeric dependent variables.
#' @param factors Character vector of categorical explanatory variables.
#' @param covariates Character vector of numeric covariates.
#' @return An `edu_analysis` object reporting Pillai's trace and, when an
#' omnibus effect is significant, Holm-adjusted univariate follow-up analyses.
#' @export
edu_manova <- function(data, outcomes, factors = character(), covariates = character()) {
    if (length(outcomes) < 2L)
        .jr_stop("MANOVA/MANCOVA requires at least two dependent variables.")
    if (length(c(factors, covariates)) == 0L)
        .jr_stop("MANOVA/MANCOVA requires at least one factor or covariate.")
    .jr_assert_columns(data, c(outcomes, factors, covariates))
    d <- .jr_complete(data, c(outcomes, factors, covariates))
    for (name in c(outcomes, covariates))
        .jr_numeric(d[[name]], name)
    for (name in factors)
        d[[name]] <- droplevels(factor(d[[name]]))
    formula <- .jr_multivariate_formula(outcomes, factors, covariates)
    model <- stats::manova(formula, data = d)
    statistics <- .jr_pillai_statistics(model)
    followups <- .jr_manova_followups(d, outcomes, factors, covariates, statistics)
    followup_text <- .jr_manova_followup_text(followups)
    label <- if (length(covariates) == 0L) "MANOVA" else "MANCOVA"
    residuals <- stats::residuals(model)
    diagnostics <- do.call(rbind, lapply(seq_along(outcomes), function(i) {
        .jr_shapiro(
            residuals[, i],
            sprintf("Residual normality for %s (Shapiro-Wilk)", outcomes[i])
        )
    }))
    diagnostics <- rbind(
        diagnostics,
        data.frame(
            check = "Homogeneity of covariance matrices",
            statistic = NA_real_,
            p = NA_real_,
            status = "Not assessed",
            interpretation = "This generated report does not recalculate Box's M from the parent analysis.",
            action = "Request and inspect Box's M and the multivariate diagnostic plots in the built-in MANCOVA analysis.",
            stringsAsFactors = FALSE
        )
    )
    effects <- paste(vapply(seq_len(nrow(statistics)), function(i) {
        sprintf(
            "%s %s, Pillai's trace = %s, F(%s, %s) = %s, p %s.",
            statistics$term[i],
            if (statistics$p[i] < .05) "was significant" else "was not significant",
            .jr_num(statistics$effect[i], 2L, TRUE),
            .jr_num(statistics$df1[i], 2L), .jr_num(statistics$df2[i], 2L),
            .jr_num(statistics$statistic[i]), .jr_p(statistics$p[i])
        )
    }, character(1)), collapse = " ")
    explanatory_text <- if (length(factors) > 0L && length(covariates) > 0L)
        sprintf("%s after adjusting for %s", paste(factors, collapse = " and "), paste(covariates, collapse = " and "))
    else if (length(factors) > 0L)
        paste(factors, collapse = " and ")
    else
        paste(covariates, collapse = " and ")
    apa <- sprintf(
        "A %s examined the combined dependent variables (%s) in relation to %s using Pillai's trace. %s",
        label, paste(outcomes, collapse = " and "),
        explanatory_text, effects
    )
    if (nzchar(followup_text))
        apa <- paste(apa, followup_text, .jr_manova_followup_note())
    plain <- if (any(statistics$p < .05)) {
        "At least one explanatory variable was associated with the combined dependent-variable outcome. Examine planned follow-up analyses before interpreting separate outcomes."
    } else {
        "The multivariate test did not identify clear evidence of differences in the combined dependent-variable outcome."
    }
    assumption_text <- .jr_diagnostic_text(diagnostics)
    if (any(diagnostics$status %in% c("Caution", "Serious"))) {
        diagnostic_note <- "These diagnostics should inform interpretation and sensitivity analyses but should not automatically determine the choice of statistical procedure."
        assumption_text <- paste(assumption_text, diagnostic_note)
    }
    caution <- if (any(diagnostics$status %in% c("Caution", "Serious")))
        paste("Caution:", assumption_text)
    else ""
    if (nzchar(followup_text))
        plain <- paste(plain, followup_text, .jr_manova_followup_note())
    descriptives <- data.frame(
        group = outcomes,
        n = vapply(d[outcomes], length, integer(1)),
        mean = vapply(d[outcomes], mean, numeric(1)),
        sd = vapply(d[outcomes], stats::sd, numeric(1)),
        stringsAsFactors = FALSE
    )
    result <- .new_edu_analysis(
        analysis = "manova", label = label,
        question = sprintf("Do %s jointly vary with %s?", paste(outcomes, collapse = " and "), paste(c(factors, covariates), collapse = " and ")),
        requirements = "Two or more numeric dependent variables and categorical factors and/or numeric covariates.",
        main = statistics, descriptives = descriptives, effects = statistics,
        diagnostics = diagnostics, interpretation = plain, caution = caution,
        plot_data = data.frame(),
        report_blocks = list(
            rationale = sprintf("%s tests effects on multiple dependent variables simultaneously.", label),
            descriptives = plain, apa = apa, assumptions = assumption_text, plain = plain
        ),
        statistics = statistics, call = match.call()
    )
    result$model <- model
    result$followups <- followups
    result
}
