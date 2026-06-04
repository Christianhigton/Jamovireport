#' @importFrom jmvcore .
eduMancovaClass <- if (requireNamespace("jmvcore", quietly = TRUE)) R6Class(
    "eduMancovaClass",
    inherit = eduMancovaBase,
    private = list(
        .run = function() {
            outcomes <- self$options$outcomes
            factors <- self$options$factors
            covariates <- self$options$covariates
            if (is.null(factors))
                factors <- character()
            if (is.null(covariates))
                covariates <- character()
            if (length(outcomes) < 2L || length(c(factors, covariates)) == 0L)
                return()
            result <- .jr_guided_computation(edu_manova(self$data, outcomes, factors, covariates))
            result <- .jr_apply_variable_descriptions(result, self$data)
            self$results$overview$setContent(.jr_jamovi_overview_html(result))
            for (i in seq_len(nrow(result$statistics)))
                self$results$main$addRow(rowKey = i, values = as.list(result$statistics[i, ]))
            for (i in seq_len(nrow(result$descriptives))) {
                d <- result$descriptives[i, ]
                self$results$descriptives$addRow(rowKey = i, values = list(
                    label = d$group, n = d$n, mean = d$mean, sd = d$sd
                ))
            }
            self$results$followups$deleteRows()
            self$results$followups$setVisible(nrow(result$followups) > 0L)
            if (nrow(result$followups) > 0L) {
                for (i in seq_len(nrow(result$followups)))
                    self$results$followups$addRow(rowKey = i, values = as.list(result$followups[i, ]))
            }
            .jr_populate_diagnostics(self$results$diagnostics, result$diagnostics)
            self$results$report$setContent(.jr_jamovi_report_html(result, self$options))
            self$results$interpretation$setContent(.jr_jamovi_interpretation_html(result))
        }
    )
)
