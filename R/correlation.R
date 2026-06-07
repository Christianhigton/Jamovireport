#' Guided bivariate correlation
#'
#' @param data A data frame.
#' @param x,y Numeric variable names.
#' @param method Correlation method.
#' @param ci Confidence level; base R confidence intervals are reported for Pearson correlation.
#' @return An `edu_analysis` object.
#' @export
edu_correlation <- function(data, x, y, method = c("pearson", "spearman", "kendall"), ci = 0.95) {
    method <- match.arg(method)
    .jr_assert_columns(data, c(x, y))
    d <- .jr_complete(data, c(x, y))
    .jr_numeric(d[[x]], x)
    .jr_numeric(d[[y]], y)
    test <- suppressWarnings(stats::cor.test(d[[x]], d[[y]], method = method, conf.level = ci, exact = FALSE))
    coefficient <- unname(test$estimate)
    coefficient_name <- switch(method, pearson = "r", spearman = "ρ", kendall = "τ")
    n <- nrow(d)
    sig_label <- if (test$p.value < .05) "statistically significant" else "not statistically significant"
    direction <- if (coefficient >= 0) "positive" else "negative"
    ci_text <- if (!is.null(test$conf.int))
        sprintf(", %s%% CI %s", .jr_num(ci * 100, 0L), .jr_ci(test$conf.int[1], test$conf.int[2], 2L, TRUE))
    else ""
    df_text <- if (!is.null(test$parameter) && is.finite(test$parameter))
        sprintf(", df = %s", .jr_num(test$parameter, 0L))
    else ""
    apa <- sprintf(
        "A %s %s correlation between %s and %s was found, %s(%s) = %s%s, p %s, n = %s.",
        sig_label, tools::toTitleCase(method), x, y,
        coefficient_name, .jr_num(if (!is.null(test$parameter) && is.finite(test$parameter)) test$parameter else n - 2L, 0L),
        .jr_num(coefficient, 2L, TRUE),
        ci_text, .jr_p(test$p.value), n
    )
    plain <- sprintf(
        "Higher %s values tended to be associated with %s %s values. %s",
        x, if (coefficient >= 0) "higher" else "lower", y,
        if (test$p.value < .05)
            "The association is unlikely to be zero in the sampled population under the test model."
        else
            "The sample does not provide clear evidence that the association differs from zero."
    )
    diagnostics <- data.frame(
        check = if (method == "pearson") "Linearity and influential observations" else "Monotonic relationship and influential observations",
        statistic = NA_real_, p = NA_real_, status = "Not assessed",
        interpretation = "The form of the association should be examined visually.",
        action = "Inspect the scatterplot before interpreting the coefficient.",
        stringsAsFactors = FALSE
    )
    stats <- data.frame(test = method, statistic = coefficient, df = unname(test$parameter %||% NA_real_),
                        p = test$p.value,
                        ci_low = if (is.null(test$conf.int)) NA_real_ else test$conf.int[1],
                        ci_high = if (is.null(test$conf.int)) NA_real_ else test$conf.int[2])
    .new_edu_analysis(
        analysis = "correlation", label = "Correlation",
        question = sprintf("Are %s and %s associated?", x, y),
        requirements = if (method == "pearson")
            "Two numeric variables with an approximately linear association."
        else
            "Two ordinal or numeric variables with an approximately monotonic association.",
        main = stats, descriptives = data.frame(variable = c(x, y), n = nrow(d),
                                                mean = c(mean(d[[x]]), mean(d[[y]])),
                                                sd = c(stats::sd(d[[x]]), stats::sd(d[[y]]))),
        effects = stats, diagnostics = diagnostics, interpretation = plain, caution = "",
        plot_data = data.frame(x = d[[x]], y = d[[y]], x_name = x, y_name = y),
        report_blocks = list(
            rationale = sprintf("%s correlation estimates the direction and strength of association between %s and %s.", tools::toTitleCase(method), x, y),
            descriptives = plain, apa = apa,
            assumptions = .jr_diagnostic_text(diagnostics), plain = plain
        ),
        statistics = stats, call = match.call()
    )
}
