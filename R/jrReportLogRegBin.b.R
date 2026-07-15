#' @importFrom jmvcore .
jrReportLogRegBinClass <- if (requireNamespace("jmvcore", quietly = TRUE)) R6Class(
    "jrReportLogRegBinClass",
    inherit = jrReportLogRegBinBase,
    private = list(
        .init = function() {
            .jr_addon_insert_card(
                self, coefficients = TRUE,
                refs = c("RCore", "jReport", "jmvcore", "parameters", "performance", "effectsize",
                         "Cohen1988", "Cumming2014")
            )
        },
        .run = function() {
            outcome <- self$parent$options$dep
            predictors <- unique(c(self$parent$options$covs, self$parent$options$factors))
            if (is.null(outcome) || length(predictors) == 0L)
                return()
            adjusted_data <- .jr_apply_reference_levels(self$data, self$parent$options$refLevels)
            model_formula <- .jr_parent_model_formula(
                outcome, predictors, self$parent$options$blocks
            )
            result <- try(
                edu_logistic_regression(adjusted_data, model_formula, ci = .jr_parent_ci(self$parent)),
                silent = TRUE
            )
            if (inherits(result, "try-error")) {
                .jr_addon_message(self, .jr_guided_error_message(attr(result, "condition")))
                return()
            }
            .jr_addon_set_card(
                self, list(result),
                .jr_accuracy_note("This generated paragraph and odds-ratio table summarise the selected binomial logistic regression model.")
            )
        }
    )
)
