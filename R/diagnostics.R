.jr_levene <- function(outcome, group, data) {
    fit <- suppressWarnings(car::leveneTest(data[[outcome]], data[[group]], center = stats::median))
    p <- fit[["Pr(>F)"]][1]
    statistic <- fit[["F value"]][1]
    flagged <- is.finite(p) && p < .05
    data.frame(
        check = "Homogeneity of variance (Levene)",
        statistic = statistic,
        p = p,
        status = if (flagged) "Caution" else "Acceptable",
        interpretation = if (flagged)
            "Variability differs across groups according to Levene's test."
        else
            "No clear evidence of unequal group variances was detected.",
        action = if (flagged)
            "Report Welch inference or include it as a sensitivity analysis."
        else
            "The equal-variance version remains reasonable if it was planned.",
        stringsAsFactors = FALSE
    )
}

.jr_model_diagnostics <- function(model) {
    diagnostics <- .jr_shapiro(stats::residuals(model), "Residual normality (Shapiro-Wilk)")
    hetero <- try(performance::check_heteroscedasticity(model), silent = TRUE)
    if (!inherits(hetero, "try-error")) {
        p <- suppressWarnings(as.numeric(hetero)[1])
        diagnostics <- rbind(
            diagnostics,
            data.frame(
                check = "Homoscedasticity",
                statistic = NA_real_,
                p = p,
                status = if (!is.na(p) && p < .05) "Caution" else "Acceptable",
                interpretation = if (!is.na(p) && p < .05)
                    "Residual variance may change across fitted values."
                else
                    "No clear heteroscedasticity concern was identified.",
                action = if (!is.na(p) && p < .05)
                    "Consider heteroscedasticity-consistent standard errors."
                else
                    "Continue to inspect residual plots.",
                stringsAsFactors = FALSE
            )
        )
    }
    vif <- try(performance::check_collinearity(model), silent = TRUE)
    if (!inherits(vif, "try-error") && nrow(vif) > 0L) {
        worst <- max(vif$VIF, na.rm = TRUE)
        flagged <- is.finite(worst) && worst >= 5
        diagnostics <- rbind(
            diagnostics,
            data.frame(
                check = "Multicollinearity (maximum VIF)",
                statistic = worst,
                p = NA_real_,
                status = if (flagged) "Caution" else "Acceptable",
                interpretation = if (flagged)
                    "Some predictor coefficients may be unstable because predictors overlap strongly."
                else
                    "No substantial multicollinearity was indicated by VIF.",
                action = if (flagged)
                    "Interpret individual coefficients cautiously and examine overlapping predictors."
                else
                    "Interpret coefficients with the other model diagnostics.",
                stringsAsFactors = FALSE
            )
        )
    }
    diagnostics
}

`%||%` <- function(x, y) {
    if (is.null(x) || length(x) == 0L) y else x
}
