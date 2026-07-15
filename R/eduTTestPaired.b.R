#' @importFrom jmvcore .
eduTTestPairedClass <- if (requireNamespace("jmvcore", quietly = TRUE)) R6Class(
    "eduTTestPairedClass",
    inherit = eduTTestPairedBase,
    private = list(
        .run = function() {
            if (is.null(self$options$outcome) || is.null(self$options$pairedOutcome))
                return()

            result <- .jr_guided_computation(edu_t_test(
                data = self$data,
                outcome = self$options$outcome,
                paired_outcome = self$options$pairedOutcome,
                type = "paired",
                ci = self$options$ciWidth / 100
            ))
            .jr_populate_guided_ttest(self, result)
        },
        .plot = function(image, ggtheme, theme, ...) {
            if (is.null(image$state))
                return(FALSE)
            edu_plot(image$state) + ggtheme
        }
    )
)
