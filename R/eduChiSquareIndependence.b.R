#' @importFrom jmvcore .
eduChiSquareIndependenceClass <- if (requireNamespace("jmvcore", quietly = TRUE)) R6Class(
    "eduChiSquareIndependenceClass",
    inherit = eduChiSquareIndependenceBase,
    private = list(
        .run = function() {
            if (is.null(self$options$rowVariable) || is.null(self$options$columnVariable))
                return()
            result <- .jr_guided_computation(edu_chisq_independence(
                self$data, self$options$rowVariable, self$options$columnVariable,
                counts = self$options$counts
            ))
            result <- .jr_apply_variable_descriptions(result, self$data)
            self$results$overview$setContent(.jr_jamovi_overview_html(result))
            self$results$fit$setRow(rowKey = "1", values = as.list(result$statistics[1, ]))
            for (i in seq_len(nrow(result$cells)))
                self$results$cells$addRow(rowKey = i, values = as.list(result$cells[i, ]))
            .jr_populate_diagnostics(self$results$diagnostics, result$diagnostics, fixed = TRUE)
            self$results$report$setContent(.jr_jamovi_report_html(result, self$options))
            self$results$interpretation$setContent(.jr_jamovi_interpretation_html(result))
        }
    )
)
