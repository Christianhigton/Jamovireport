#' @importFrom jmvcore .
eduChiSquareGoodnessClass <- if (requireNamespace("jmvcore", quietly = TRUE)) R6Class(
    "eduChiSquareGoodnessClass",
    inherit = eduChiSquareGoodnessBase,
    private = list(
        .run = function() {
            if (is.null(self$options$variable))
                return()
            ratios <- trimws(self$options$expectedRatios)
            expected <- NULL
            if (nzchar(ratios)) {
                expected <- suppressWarnings(as.numeric(strsplit(ratios, ",", fixed = TRUE)[[1]]))
                if (any(is.na(expected))) {
                    self$results$interpretation$setContent(
                        .jr_html_card("Check expected ratios", "Input could not be read",
                            "Enter expected ratios as comma-separated numbers, for example: 1, 1, 2.",
                            accent = "#b46c21")
                    )
                    return()
                }
            }
            result <- .jr_guided_computation(edu_chisq_gof(
                self$data, self$options$variable, counts = self$options$counts,
                expected = expected
            ))
            result <- .jr_apply_variable_descriptions(result, self$data)
            self$results$overview$setContent(.jr_jamovi_overview_html(result))
            self$results$fit$addRow(rowKey = 1, values = as.list(result$statistics[1, ]))
            for (i in seq_len(nrow(result$cells)))
                self$results$cells$addRow(rowKey = i, values = as.list(result$cells[i, ]))
            .jr_populate_diagnostics(self$results$diagnostics, result$diagnostics)
            self$results$report$setContent(.jr_jamovi_report_html(result, self$options))
            self$results$interpretation$setContent(.jr_jamovi_interpretation_html(result))
        }
    )
)
