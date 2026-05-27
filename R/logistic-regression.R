#' Guided binomial logistic regression
#'
#' @param data A data frame.
#' @param formula A model formula with a binary outcome.
#' @param ci Confidence level.
#' @return An `edu_analysis` object.
#' @export
edu_logistic_regression <- function(data, formula, ci = 0.95) {
    if (!inherits(formula, "formula"))
        .jr_stop("`formula` must be a model formula.")
    variables <- all.vars(formula)
    .jr_assert_columns(data, variables)
    d <- .jr_complete(data, variables)
    outcome <- as.character(formula[[2]])
    d[[outcome]] <- droplevels(as.factor(d[[outcome]]))
    if (nlevels(d[[outcome]]) != 2L)
        .jr_stop("Binomial logistic regression requires an outcome with exactly two levels.")
    model <- stats::glm(formula, data = d, family = stats::binomial())
    if (!isTRUE(model$converged))
        warning("The binomial logistic regression did not converge.", call. = FALSE)

    outcome_levels <- levels(d[[outcome]])
    reference <- outcome_levels[1]
    event <- outcome_levels[2]
    terms <- attr(stats::terms(model), "term.labels")
    null_model <- stats::update(model, . ~ 1)
    likelihood_ratio <- stats::deviance(null_model) - stats::deviance(model)
    df <- stats::df.residual(null_model) - stats::df.residual(model)
    p_value <- stats::pchisq(likelihood_ratio, df = df, lower.tail = FALSE)
    mcfadden <- 1 - as.numeric(stats::logLik(model) / stats::logLik(null_model))

    coefficient_table <- summary(model)$coefficients
    critical <- stats::qnorm(1 - (1 - ci) / 2)
    coefficients <- data.frame(
        Parameter = rownames(coefficient_table),
        Coefficient = coefficient_table[, "Estimate"],
        SE = coefficient_table[, "Std. Error"],
        z = coefficient_table[, "z value"],
        p = coefficient_table[, "Pr(>|z|)"],
        OR = exp(coefficient_table[, "Estimate"]),
        CI_low = exp(coefficient_table[, "Estimate"] - critical * coefficient_table[, "Std. Error"]),
        CI_high = exp(coefficient_table[, "Estimate"] + critical * coefficient_table[, "Std. Error"]),
        stringsAsFactors = FALSE
    )
    diagnostics <- .jr_logistic_diagnostics(model, d, outcome)
    significance <- if (p_value < .05) "statistically significant" else "not statistically significant"
    apa <- sprintf(
        "A binomial logistic regression predicting %s (relative to %s) from %s was %s, chi-square(%s) = %s, p %s, McFadden's R-squared = %s.",
        event, reference, paste(terms, collapse = ", "), significance,
        .jr_num(df, 0L), .jr_num(likelihood_ratio), .jr_p(p_value),
        .jr_num(mcfadden, 2L, TRUE)
    )
    predictor_rows <- coefficients$Parameter != "(Intercept)"
    coefficient_sentences <- vapply(which(predictor_rows), function(i) {
        direction <- if (coefficients$OR[i] > 1) "higher" else "lower"
        sprintf(
            "%s was associated with %s odds of %s, B = %s, SE = %s, OR = %s, %s%% CI %s, z = %s, p %s.",
            coefficients$Parameter[i], direction, event,
            .jr_num(coefficients$Coefficient[i]), .jr_num(coefficients$SE[i]),
            .jr_or(coefficients$OR[i]), .jr_num(ci * 100, 0L),
            .jr_or_ci(coefficients$CI_low[i], coefficients$CI_high[i]),
            .jr_num(coefficients$z[i]), .jr_p(coefficients$p[i])
        )
    }, character(1))
    if (length(coefficient_sentences) > 0L)
        apa <- paste(apa, paste(coefficient_sentences, collapse = " "))
    plain <- sprintf(
        "This model estimates the odds of %s rather than %s. An odds ratio above 1 indicates higher odds of %s as that predictor increases or relative to its reference level; an odds ratio below 1 indicates lower odds.",
        event, reference, event
    )
    assumption_text <- .jr_diagnostic_text(diagnostics)
    caution <- if (any(diagnostics$status %in% c("Caution", "Serious")))
        paste("Caution:", assumption_text)
    else ""
    statistics <- data.frame(
        test = "Overall model likelihood-ratio test",
        statistic = likelihood_ratio,
        df = df,
        p = p_value,
        r2 = mcfadden,
        stringsAsFactors = FALSE
    )
    result <- .new_edu_analysis(
        analysis = "logistic_regression", label = "Binomial Logistic Regression",
        question = sprintf("Which predictors are associated with the odds of %s rather than %s?", event, reference),
        requirements = "A binary outcome and predictors represented appropriately in a logistic regression model.",
        main = statistics, descriptives = coefficients, effects = coefficients,
        diagnostics = diagnostics, interpretation = plain, caution = caution,
        plot_data = data.frame(fitted = stats::fitted(model), observed = as.numeric(d[[outcome]]) - 1),
        report_blocks = list(
            rationale = sprintf("Binomial logistic regression models the odds that %s is observed rather than %s.", event, reference),
            descriptives = plain, apa = apa, assumptions = assumption_text, plain = plain
        ),
        statistics = statistics, call = match.call()
    )
    result$model <- model
    result$parameters <- coefficients
    result$event <- event
    result$reference <- reference
    result
}

.jr_logistic_diagnostics <- function(model, data, outcome) {
    converged <- isTRUE(model$converged)
    diagnostics <- data.frame(
        check = "Model convergence",
        statistic = NA_real_,
        p = NA_real_,
        status = if (converged) "Acceptable" else "Serious",
        interpretation = if (converged)
            "The estimation algorithm converged on a fitted model."
        else
            "The estimation algorithm did not converge, so estimates may be unreliable.",
        action = if (converged)
            "Continue with predictor and model-fit interpretation."
        else
            "Do not report the model until convergence problems have been investigated.",
        stringsAsFactors = FALSE
    )
    parameters <- max(length(stats::coef(model)) - 1L, 1L)
    smallest_outcome_group <- min(table(data[[outcome]]))
    events_per_parameter <- as.numeric(smallest_outcome_group) / parameters
    limited <- events_per_parameter < 10
    diagnostics <- rbind(
        diagnostics,
        data.frame(
            check = "Outcome information per predictor parameter",
            statistic = events_per_parameter,
            p = NA_real_,
            status = if (limited) "Caution" else "Acceptable",
            interpretation = if (limited)
                "The smaller outcome category provides limited information relative to model complexity."
            else
                "The smaller outcome category provides at least 10 observations per estimated predictor parameter.",
            action = if (limited)
                "Interpret odds ratios cautiously and consider a simpler model or penalised sensitivity analysis."
            else
                "Retain the planned model while considering design-based limitations.",
            stringsAsFactors = FALSE
        )
    )
    numeric_predictors <- names(Filter(is.numeric, data[setdiff(names(data), outcome)]))
    if (length(numeric_predictors) > 0L) {
        diagnostics <- rbind(
            diagnostics,
            data.frame(
                check = "Linearity of the logit for continuous predictors",
                statistic = NA_real_,
                p = NA_real_,
                status = "Not assessed",
                interpretation = "Logistic regression assumes each continuous predictor relates linearly to the log odds unless non-linear terms are included.",
                action = "Inspect non-linear terms, plots, or a planned Box-Tidwell-type assessment for continuous predictors.",
                stringsAsFactors = FALSE
            )
        )
    }
    collinearity <- try(performance::check_collinearity(model), silent = TRUE)
    if (!inherits(collinearity, "try-error") && nrow(collinearity) > 0L) {
        worst <- max(collinearity$VIF, na.rm = TRUE)
        flagged <- is.finite(worst) && worst >= 5
        diagnostics <- rbind(
            diagnostics,
            data.frame(
                check = "Multicollinearity (maximum VIF)",
                statistic = worst,
                p = NA_real_,
                status = if (flagged) "Caution" else "Acceptable",
                interpretation = if (flagged)
                    "Some odds-ratio estimates may be unstable because predictors overlap strongly."
                else
                    "No substantial multicollinearity was indicated by VIF.",
                action = if (flagged)
                    "Review overlapping predictors and interpret individual odds ratios cautiously."
                else
                    "Interpret odds ratios alongside model fit and study design.",
                stringsAsFactors = FALSE
            )
        )
    }
    diagnostics
}
