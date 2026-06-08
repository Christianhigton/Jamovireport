#' @importFrom jmvcore .
jrReportContTablesClass <- if (requireNamespace("jmvcore", quietly = TRUE)) R6Class(
    "jrReportContTablesClass",
    inherit = jrReportContTablesBase,
    private = list(
        .init = function() {
            .jr_addon_insert_card(
                self, cells = TRUE,
                refs = c("RCore", "jReport", "jmvcore", "effectsize")
            )
        },
        .run = function() {
            row <- self$parent$options$rows
            column <- self$parent$options$cols
            layers <- self$parent$options$layers
            if (is.null(row) || is.null(column))
                return()
            if (length(layers) > 0L) {
                .jr_addon_message(self, "Automatic reporting currently supports an unlayered contingency table. Remove layers or use a guided analysis to report separate strata carefully.")
                return()
            }
            result <- try(
                edu_chisq_independence(self$data, row, column, counts = self$parent$options$counts),
                silent = TRUE
            )
            if (inherits(result, "try-error")) {
                .jr_addon_message(self, .jr_guided_error_message(attr(result, "condition")))
                return()
            }
            .jr_addon_set_card(
                self, list(result),
                .jr_accuracy_note("This generated paragraph summarises Pearson's chi-square association test and Cramer's V for the selected contingency table.")
            )
        }
    )
)
