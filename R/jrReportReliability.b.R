#' @importFrom jmvcore .
jrReportReliabilityClass <- if (requireNamespace("jmvcore", quietly = TRUE)) R6Class(
    "jrReportReliabilityClass",
    inherit = jrReportReliabilityBase,
    private = list(
        .init = function() {
            .jr_addon_insert_card(self)
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
            .jr_addon_set_card(
                self, list(result),
                "This report card uses McDonald's omega total. Select McDonald's omega in the standard jamovi output if you also want its native reliability table; alpha is not used in this generated narrative."
            )
        }
    )
)
