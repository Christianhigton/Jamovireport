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

#' Guided multivariate analysis of variance or covariance
#'
#' @param data A data frame.
#' @param outcomes Character vector containing two or more numeric dependent variables.
#' @param factors Character vector of categorical explanatory variables.
#' @param covariates Character vector of numeric covariates.
#' @return An `edu_analysis` object reporting Pillai's trace.
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
    plain <- if (any(statistics$p < .05)) {
        "At least one explanatory variable was associated with the combined dependent-variable outcome. Examine planned follow-up analyses before interpreting separate outcomes."
    } else {
        "The multivariate test did not identify clear evidence of differences in the combined dependent-variable outcome."
    }
    assumption_text <- .jr_diagnostic_text(diagnostics)
    caution <- if (any(diagnostics$status %in% c("Caution", "Serious")))
        paste("Caution:", assumption_text)
    else ""
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
    result
}
