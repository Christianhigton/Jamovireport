#' Guided linear regression
#'
#' @param data A data frame.
#' @param formula A model formula with a numeric outcome.
#' @param ci Confidence level.
#' @return An `edu_analysis` object.
#' @export
edu_lm <- function(data, formula, ci = 0.95) {
    if (!inherits(formula, "formula"))
        .jr_stop("`formula` must be a model formula.")
    model <- stats::lm(formula, data = data)
    if (length(stats::residuals(model)) == 0L)
        .jr_stop("The regression model could not be estimated from complete observations.")
    model_summary <- summary(model)
    f <- model_summary$fstatistic
    df1 <- unname(f[["numdf"]])
    df2 <- unname(f[["dendf"]])
    f_value <- unname(f[["value"]])
    p_value <- stats::pf(f_value, df1, df2, lower.tail = FALSE)
    params <- parameters::model_parameters(model, ci = ci)
    standardized <- parameters::standardize_parameters(model, method = "basic", ci = ci)
    params$beta <- standardized$Std_Coefficient[match(params$Parameter, standardized$Parameter)]
    fit <- performance::model_performance(model)
    diagnostics <- .jr_model_diagnostics(model)
    outcome <- as.character(formula[[2]])
    terms <- attr(stats::terms(model), "term.labels")
    apa <- sprintf(
        "The linear regression model predicting %s from %s was %s, F(%s, %s) = %s, p %s, R-squared = %s, adjusted R-squared = %s.",
        outcome, paste(terms, collapse = ", "),
        if (p_value < .05) "statistically significant" else "not statistically significant",
        .jr_num(df1, 0L), .jr_num(df2, 0L), .jr_num(f_value),
        .jr_p(p_value), .jr_num(fit$R2[1], 2L, TRUE), .jr_num(fit$R2_adjusted[1], 2L, TRUE)
    )
    predictor_rows <- params$Parameter != "(Intercept)"
    coefficient_sentences <- vapply(which(predictor_rows), function(i) {
        sprintf(
            "%s: B = %s, SE = %s, %s%% CI %s, beta = %s, t(%s) = %s, p %s.",
            params$Parameter[i], .jr_num(params$Coefficient[i]), .jr_num(params$SE[i]),
            .jr_num(ci * 100, 0L), .jr_ci(params$CI_low[i], params$CI_high[i]),
            .jr_num(params$beta[i], 2L, TRUE), .jr_num(params$df_error[i], 0L),
            .jr_num(params$t[i]), .jr_p(params$p[i])
        )
    }, character(1))
    if (length(coefficient_sentences) > 0L)
        apa <- paste(apa, paste(coefficient_sentences, collapse = " "))
    plain <- sprintf(
        "Together, the predictors accounted for about %s%% of the variation in %s. Coefficients describe each predictor's association while holding the others constant.",
        .jr_num(100 * fit$R2[1], 1L), outcome
    )
    assumption_text <- .jr_diagnostic_text(diagnostics)
    caution <- if (any(diagnostics$status == "Caution"))
        paste("Caution:", assumption_text)
    else ""
    stats <- data.frame(test = "Overall model F", statistic = f_value, df1 = df1, df2 = df2,
                        p = p_value, r2 = fit$R2[1], adjusted_r2 = fit$R2_adjusted[1])
    result <- .new_edu_analysis(
        analysis = "regression", label = "Linear Regression",
        question = sprintf("How well do %s explain or predict %s?", paste(terms, collapse = ", "), outcome),
        requirements = "A numeric outcome and predictors whose relationships with the outcome are represented appropriately in the model.",
        main = stats, descriptives = params, effects = params,
        diagnostics = diagnostics, interpretation = plain, caution = caution,
        plot_data = data.frame(fitted = stats::fitted(model), residual = stats::residuals(model)),
        report_blocks = list(
            rationale = sprintf("Linear regression estimates the association between %s and the specified predictors.", outcome),
            descriptives = plain, apa = apa, assumptions = assumption_text, plain = plain
        ),
        statistics = stats, call = match.call()
    )
    result$model <- model
    result$parameters <- params
    result
}
