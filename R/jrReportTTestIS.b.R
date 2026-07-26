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
            adapted <- tryCatch(
                .jr_core_ttest_adapter(
                    self$parent$options, self$parent$results, self$data,
                    allow_fallback = TRUE
                ),
                error = function(error) structure(list(
                    analysis = "independentSamplesTTest", analyses = list(),
                    warnings = paste("The core t-test output could not be read:", conditionMessage(error)),
                    hostSchema = "jmv::ttestIS"
                ), class = c("jr_core_ttest_adapter", "list"))
            )
            rendered <- tryCatch(
                .jr_core_ttest_render(adapted, self$options),
                error = function(error) list(
                    report = .jr_html_card(
                        "jReport add-on", "Reporting output could not be generated",
                        paste(
                            "The core analysis is unchanged.",
                            "jReport encountered a reporting error:", conditionMessage(error)
                        ),
                        accent = "#b46c21"
                    ),
                    guidance = .jr_html_card(
                        "Interpretation guidance", "Reporting output could not be generated",
                        paste(
                            "The core analysis is unchanged.",
                            "jReport encountered a reporting error:", conditionMessage(error)
                        ),
                        accent = "#b46c21"
                    ),
                    references = ""
                )
            )
            card <- .jr_addon_get(self$parent$results, "jReportCard")
            if (!is.null(card)) {
                card$setVisible(TRUE)
                card$setContent(rendered$report)
            }
            interpretation <- .jr_addon_get(self$parent$results, "jReportInterpretation")
            if (!is.null(interpretation) && nzchar(rendered$guidance %||% "")) {
                interpretation$setVisible(TRUE)
                interpretation$setContent(rendered$guidance)
            }
            references <- .jr_addon_get(self$parent$results, "methodsReferences")
            if (!is.null(references) && nzchar(rendered$references)) {
                references$setVisible(TRUE)
                references$setContent(rendered$references)
            }
        }
    )
)
