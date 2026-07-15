#' @importFrom jmvcore .
eduTTestIndependentClass <- if (requireNamespace("jmvcore", quietly = TRUE)) R6Class(
    "eduTTestIndependentClass",
    inherit = eduTTestIndependentBase,
    private = list(
        .run = function() {
            if (is.null(self$options$outcome) || is.null(self$options$group))
                return()

            result <- .jr_guided_computation(edu_t_test(
                data = self$data,
                outcome = self$options$outcome,
                group = self$options$group,
                type = "independent",
                var_equal = self$options$varEqual,
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
