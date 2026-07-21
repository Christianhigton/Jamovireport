#' @importFrom jmvcore .
jrReportLinRegClass <- if (requireNamespace("jmvcore", quietly = TRUE)) R6Class(
    "jrReportLinRegClass",
    inherit = jrReportLinRegBase,
    private = list(
        .init = function() {
            .jr_addon_insert_card(
                self,
                refs = c("RCore", "jReport", "jmvcore", "parameters", "performance", "effectsize",
                         "Cohen1988", "Cumming2014")
            )
        },
        .run = function() {
            outcome <- self$parent$options$dep
            predictors <- unique(c(self$parent$options$covs, self$parent$options$factors))
            if (is.null(outcome) || length(predictors) == 0L)
                return()
            model_formula <- .jr_formula(outcome, predictors)
            result <- try(edu_lm(self$data, model_formula, ci = .jr_parent_ci(self$parent)), silent = TRUE)
            if (inherits(result, "try-error")) {
                .jr_addon_message(self, .jr_guided_error_message(attr(result, "condition")))
                return()
            }
            .jr_addon_set_card(
                self, list(result),
                .jr_accuracy_note("This generated paragraph reports the selected predictors using the current automatic model summary.")
            )
        }
    )
)
