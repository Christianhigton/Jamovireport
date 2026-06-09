#' Guided multinomial logistic regression
#'
#' @param data A data frame.
#' @param formula A model formula with a nominal outcome (3+ levels).
#' @param ci Confidence level.
#' @return An `edu_analysis` object.
#' @export
edu_multinomial_logistic <- function(data, formula, ci = 0.95) {
    if (!inherits(formula, "formula"))
        .jr_stop("`formula` must be a model formula.")
    variables <- all.vars(formula)
    .jr_assert_columns(data, variables)
    d <- .jr_complete(data, variables)
    outcome_var <- as.character(formula[[2]])
    d[[outcome_var]] <- droplevels(as.factor(d[[outcome_var]]))
    n_levels <- nlevels(d[[outcome_var]])
    if (n_levels < 3L)
        .jr_stop("Multinomial logistic regression requires an outcome with 3 or more levels.")
    all_levels <- levels(d[[outcome_var]])
    reference <- all_levels[1]
    outcome_levels <- all_levels[-1]
    terms_labels <- attr(stats::terms(formula), "term.labels")
    environment(formula) <- environment()

    model <- nnet::multinom(formula, data = d, trace = FALSE)
    converged <- model$convergence == 0L
    if (!converged)
        warning("The multinomial logistic regression did not converge.", call. = FALSE)

    sm <- summary(model)
    coefs <- sm$coefficients
    ses <- sm$standard.errors
    if (is.null(dim(coefs))) {
        coefs <- matrix(coefs, nrow = 1, dimnames = list(outcome_levels, names(coefs)))
        ses   <- matrix(ses,   nrow = 1, dimnames = list(outcome_levels, names(ses)))
    }
    z_stats <- coefs / ses
    p_vals  <- 2 * (1 - stats::pnorm(abs(z_stats)))
    crit    <- stats::qnorm(1 - (1 - ci) / 2)

    params_list <- lapply(seq_along(outcome_levels), function(i) {
        level <- outcome_levels[i]
        data.frame(
            category  = paste(level, "vs.", reference),
            term      = colnames(coefs),
            B         = coefs[i, ],
            SE        = ses[i, ],
            z         = z_stats[i, ],
            p         = p_vals[i, ],
            RRR       = exp(coefs[i, ]),
            CI_low    = exp(coefs[i, ] - crit * ses[i, ]),
            CI_high   = exp(coefs[i, ] + crit * ses[i, ]),
            stringsAsFactors = FALSE,
            row.names = NULL
        )
    })
    parameters <- do.call(rbind, params_list)

    null_formula <- stats::as.formula(paste(outcome_var, "~ 1"))
    environment(null_formula) <- environment()
    null_model <- nnet::multinom(null_formula, data = d, trace = FALSE)
    lr_stat  <- as.numeric(2 * (stats::logLik(model) - stats::logLik(null_model)))
    df_model <- attr(stats::logLik(model), "df") - attr(stats::logLik(null_model), "df")
    p_value  <- stats::pchisq(lr_stat, df = df_model, lower.tail = FALSE)
    mcfadden <- as.numeric(1 - stats::logLik(model) / stats::logLik(null_model))

    apa <- sprintf(
        "A multinomial logistic regression predicting %s (%s) from %s was %s, χ²(%s) = %s, p %s, McFadden’s R² = %s.",
        outcome_var, paste(all_levels, collapse = ", "),
        paste(terms_labels, collapse = ", "),
        if (p_value < .05) "statistically significant" else "not statistically significant",
        .jr_num(df_model, 0L), .jr_num(lr_stat), .jr_p(p_value),
        .jr_num(mcfadden, 2L, TRUE)
    )
    predictor_rows <- parameters[parameters$term != "(Intercept)", ]
    if (nrow(predictor_rows) > 0L) {
        coef_sentences <- vapply(seq_len(nrow(predictor_rows)), function(i) {
            r <- predictor_rows[i, ]
            direction <- if (r$RRR > 1) "higher" else "lower"
            sprintf(
                "For the comparison %s, %s was associated with %s relative risk (RRR = %s, %s%% CI [%s, %s], z = %s, p %s).",
                r$category, r$term, direction,
                .jr_num(r$RRR, 2L, TRUE), .jr_num(ci * 100, 0L),
                .jr_num(r$CI_low, 2L, TRUE), .jr_num(r$CI_high, 2L, TRUE),
                .jr_num(r$z), .jr_p(r$p)
            )
        }, character(1))
        apa <- paste(c(apa, coef_sentences), collapse = " ")
    }
    plain <- sprintf(
        paste(
            "This model compares each outcome category against the reference category (%s).",
            "A relative risk ratio (RRR) above 1 indicates increased likelihood of the comparison category;",
            "a value below 1 indicates decreased likelihood, relative to %s."
        ),
        reference, reference
    )
    diagnostics <- .jr_multinomial_diagnostics(model, d, outcome_var, converged)
    assumption_text <- .jr_diagnostic_text(diagnostics)
    caution <- if (any(diagnostics$status %in% c("Caution", "Serious")))
        paste("Caution:", assumption_text)
    else ""
    statistics <- data.frame(
        test      = "Overall model likelihood-ratio test",
        statistic = lr_stat,
        df        = df_model,
        p         = p_value,
        r2        = mcfadden,
        stringsAsFactors = FALSE
    )
    result <- .new_edu_analysis(
        analysis    = "multinomial_logistic",
        label       = "Multinomial Logistic Regression",
        question    = sprintf("Which predictors are associated with category membership in %s?", outcome_var),
        requirements = paste(
            "A nominal outcome with 3 or more categories and predictors appropriate",
            "for inclusion in a logistic regression model."
        ),
        main         = statistics, descriptives = parameters, effects = parameters,
        diagnostics  = diagnostics, interpretation = plain, caution = caution,
        plot_data    = data.frame(
            observed = as.character(d[[outcome_var]]),
            fitted   = as.character(all_levels[max.col(stats::fitted(model))])
        ),
        report_blocks = list(
            rationale    = sprintf(
                "Multinomial logistic regression models the log odds of each category of %s relative to the reference category (%s).",
                outcome_var, reference
            ),
            descriptives = plain, apa = apa, assumptions = assumption_text, plain = plain
        ),
        statistics = statistics, call = match.call()
    )
    result$model      <- model
    result$parameters <- parameters
    result$reference  <- reference
    result$outcome_levels <- outcome_levels
    result
}

.jr_multinomial_diagnostics <- function(model, data, outcome, converged) {
    d1 <- data.frame(
        check          = "Model convergence",
        statistic      = NA_real_,
        p              = NA_real_,
        status         = if (converged) "Acceptable" else "Serious",
        interpretation = if (converged)
            "The estimation algorithm converged successfully."
        else
            "The algorithm did not converge; estimates may be unreliable.",
        action         = if (converged)
            "Proceed with interpreting the model output."
        else
            "Do not report until convergence is resolved (try fewer predictors or more data).",
        stringsAsFactors = FALSE
    )
    n_cats <- nlevels(data[[outcome]])
    n_obs  <- nrow(data)
    n_params <- length(stats::coef(model))
    min_n_per_cat <- min(table(data[[outcome]]))
    ratio <- as.numeric(min_n_per_cat) / (n_params / (n_cats - 1))
    limited <- ratio < 10
    d2 <- data.frame(
        check          = "Observations per estimated parameter (smallest category)",
        statistic      = ratio,
        p              = NA_real_,
        status         = if (limited) "Caution" else "Acceptable",
        interpretation = if (limited)
            "The smallest outcome category provides limited information relative to model complexity."
        else
            "The smallest category provides at least 10 observations per estimated parameter.",
        action         = if (limited)
            "Interpret relative risk ratios cautiously; consider a simpler model."
        else
            "Retain the planned model while considering design limitations.",
        stringsAsFactors = FALSE
    )
    numeric_preds <- names(Filter(is.numeric, data[setdiff(names(data), outcome)]))
    if (length(numeric_preds) > 0L) {
        d3 <- data.frame(
            check          = "Linearity of the logit for continuous predictors",
            statistic      = NA_real_,
            p              = NA_real_,
            status         = "Not assessed",
            interpretation = "Multinomial logistic regression assumes a linear relationship between continuous predictors and the log odds of each category.",
            action         = "Inspect scatter plots or polynomial terms for continuous predictors.",
            stringsAsFactors = FALSE
        )
        rbind(d1, d2, d3)
    } else {
        rbind(d1, d2)
    }
}
