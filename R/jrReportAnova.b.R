#' @importFrom jmvcore .
jrReportAnovaClass <- if (requireNamespace("jmvcore", quietly = TRUE)) R6Class(
    "jrReportAnovaClass",
    inherit = jrReportAnovaBase,
    private = list(
        .init = function() {
            .jr_addon_insert_card(self, posthoc = TRUE)
        },
        .run = function() {
            outcome <- self$parent$options$dep
            factors <- self$parent$options$factors
            if (is.null(outcome) || length(factors) == 0L)
                return()
            result <- try(
                edu_anova_between(self$data, outcome, factors, ci = .jr_parent_ci(self$parent)),
                silent = TRUE
            )
            if (inherits(result, "edu_analysis")) {
                result$posthoc_report <- .jr_model_posthoc(
                    result,
                    self$parent$options$postHoc,
                    self$parent$options$postHocCorr
                )
            }
            .jr_addon_set_card(
                self, list(result),
                .jr_accuracy_note("This generated paragraph describes the selected factorial design.")
            )
        }
    )
)
