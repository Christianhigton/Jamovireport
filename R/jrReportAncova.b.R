#' @importFrom jmvcore .
jrReportAncovaClass <- if (requireNamespace("jmvcore", quietly = TRUE)) R6Class(
    "jrReportAncovaClass",
    inherit = jrReportAncovaBase,
    private = list(
        .init = function() {
            .jr_addon_insert_card(
                self, posthoc = TRUE,
                refs = c("RCore", "jReport", "jmvcore", "car", "effectsize", "emmeans")
            )
        },
        .run = function() {
            outcome <- self$parent$options$dep
            factors <- self$parent$options$factors
            covariates <- self$parent$options$covs
            if (is.null(outcome) || length(factors) == 0L || length(covariates) == 0L)
                return()
            result <- try(
                edu_ancova(
                    self$data, outcome, factors, covariates,
                    ci = .jr_parent_ci(self$parent)
                ),
                silent = TRUE
            )
            if (inherits(result, "try-error")) {
                .jr_addon_message(self, .jr_guided_error_message(attr(result, "condition")))
                return()
            }
            if (inherits(result, "edu_analysis")) {
                result$posthoc_report <- .jr_model_posthoc(
                    result,
                    self$parent$options$postHoc,
                    self$parent$options$postHocCorr
                )
            }
            .jr_addon_set_card(
                self, list(result),
                .jr_accuracy_note("This generated paragraph summarises the selected factorial effects and covariates.")
            )
        }
    )
)
