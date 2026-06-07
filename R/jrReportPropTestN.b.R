#' @importFrom jmvcore .
jrReportPropTestNClass <- if (requireNamespace("jmvcore", quietly = TRUE)) R6Class(
    "jrReportPropTestNClass",
    inherit = jrReportPropTestNBase,
    private = list(
        .init = function() {
            .jr_addon_insert_card(
                self, cells = TRUE,
                refs = c("RCore", "jReport", "jmvcore", "effectsize")
            )
        },
        .run = function() {
            variable <- self$parent$options$var
            if (is.null(variable))
                return()
            ratios <- self$parent$options$ratio
            expected <- NULL
            if (!is.null(ratios) && length(ratios) > 0L)
                expected <- .jr_expected_ratio_values(ratios)
            result <- try(
                edu_chisq_gof(self$data, variable, counts = self$parent$options$counts, expected = expected),
                silent = TRUE
            )
            .jr_addon_set_card(
                self, list(result),
                .jr_accuracy_note("This generated paragraph summarises the selected chi-square goodness-of-fit test and expected proportions.")
            )
        }
    )
)
