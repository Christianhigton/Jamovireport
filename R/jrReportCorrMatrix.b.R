#' @importFrom jmvcore .
jrReportCorrMatrixClass <- if (requireNamespace("jmvcore", quietly = TRUE)) R6Class(
    "jrReportCorrMatrixClass",
    inherit = jrReportCorrMatrixBase,
    private = list(
        .init = function() {
            .jr_addon_insert_card(
                self,
                refs = c("RCore", "jReport", "jmvcore", "effectsize")
            )
        },
        .run = function() {
            variables <- self$parent$options$vars
            if (length(variables) < 2L)
                return()
            selected <- c(
                pearson = isTRUE(self$parent$options$pearson),
                spearman = isTRUE(self$parent$options$spearman),
                kendall = isTRUE(self$parent$options$kendall)
            )
            methods <- names(selected)[selected]
            if (length(methods) == 0L)
                return()
            pairs <- utils::combn(variables, 2, simplify = FALSE)
            results <- unlist(lapply(methods, function(method) {
                lapply(pairs, function(pair) {
                    try(
                        edu_correlation(
                            self$data, pair[1], pair[2], method = method,
                            ci = .jr_parent_ci(self$parent)
                        ),
                        silent = TRUE
                    )
                })
            }), recursive = FALSE)
            results <- Filter(function(r) !inherits(r, "try-error"), results)
            .jr_addon_set_card(
                self, results,
                "A report card is generated for each selected correlation method and variable pair."
            )
        }
    )
)
