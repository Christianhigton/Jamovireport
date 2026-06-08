#' @importFrom jmvcore .
jrReportReliabilityClass <- if (requireNamespace("jmvcore", quietly = TRUE)) R6Class(
    "jrReportReliabilityClass",
    inherit = jrReportReliabilityBase,
    private = list(
        .init = function() {
            .jr_addon_insert_card(
                self,
                refs = c("RCore", "jReport", "jmvcore", "psych", "McDonald1999", "RevelleCondon2019")
            )
        },
        .run = function() {
            items <- self$parent$options$vars
            reverse_items <- self$parent$options$revItems
            if (length(items) < 3L)
                return()
            result <- try(
                edu_reliability_omega(
                    self$data, items, reverse_items = reverse_items,
                    correlation = "pearson",
                    ci = .95,
                    bootstrap = FALSE
                ),
                silent = TRUE
            )
            if (inherits(result, "try-error")) {
                .jr_addon_message(self, .jr_guided_error_message(attr(result, "condition")))
                return()
            }
            .jr_addon_set_card(
                self, list(result),
                "This report card reports McDonald's omega total and Cronbach's alpha. We recommend reporting omega alongside alpha and checking item diagnostics before using the values in assessed, clinical, or published work."
            )
        }
    )
)
