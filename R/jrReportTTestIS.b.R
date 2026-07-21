#' @importFrom jmvcore .
.jr_ttest_is_output_names <- c(
    "jReportHeading", "jReportApaTable", "jReportAssumptions",
    "jReportCard", "jReportInterpretation", "methodsReferences"
)

.jr_ttest_is_hide_outputs <- function(self) {
    for (name in .jr_ttest_is_output_names) {
        item <- .jr_addon_get(self$parent$results, name)
        if (!is.null(item))
            item$setVisible(FALSE)
    }
    invisible(NULL)
}

jrReportTTestISClass <- if (requireNamespace("jmvcore", quietly = TRUE)) R6Class(
    "jrReportTTestISClass",
    inherit = jrReportTTestISBase,
    private = list(
        .init = function() {
            .jr_addon_insert_card(
                self,
                refs = c("RCore", "jReport", "jmvcore", "effectsize", "BayesFactor",
                         "Cohen1988", "Cumming2014")
            )
            .jr_ttest_is_hide_outputs(self)
        },
        .run = function() {
            .jr_ttest_is_hide_outputs(self)
            if (!isTRUE(self$options$jreportEnabled))
                return()
            card <- .jr_addon_get(self$parent$results, "jReportCard")
            if (!is.null(card)) {
                card$setVisible(TRUE)
                card$setContent(.jr_html_card(
                    "jReport experiment",
                    "jReport: Reporting and explanation",
                    "jReport add-on proof-of-concept is active."
                ))
            }
        }
    )
)
