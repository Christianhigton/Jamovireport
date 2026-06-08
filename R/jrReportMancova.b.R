#' @importFrom jmvcore .
jrReportMancovaClass <- if (requireNamespace("jmvcore", quietly = TRUE)) R6Class(
    "jrReportMancovaClass",
    inherit = jrReportMancovaBase,
    private = list(
        .init = function() {
            .jr_addon_insert_card(
                self, followups = TRUE,
                refs = c("RCore", "jReport", "jmvcore", "car", "effectsize")
            )
        },
        .run = function() {
            outcomes <- self$parent$options$deps
            factors <- self$parent$options$factors %||% character()
            covariates <- self$parent$options$covs %||% character()
            if (length(outcomes) < 2L || length(c(factors, covariates)) == 0L)
                return()
            result <- try(
                edu_manova(self$data, outcomes, factors, covariates),
                silent = TRUE
            )
            if (inherits(result, "try-error")) {
                .jr_addon_message(self, .jr_guided_error_message(attr(result, "condition")))
                return()
            }
            .jr_addon_set_card(
                self, list(result),
                .jr_accuracy_note(
                    "This generated paragraph reports Pillai's trace. Compare it with the selected multivariate statistics and diagnostic options in the built-in MANCOVA output."
                )
            )
        }
    )
)
