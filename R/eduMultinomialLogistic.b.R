#' @importFrom jmvcore .
eduMultinomialLogisticClass <- if (requireNamespace("jmvcore", quietly = TRUE)) R6Class(
    "eduMultinomialLogisticClass",
    inherit = eduMultinomialLogisticBase,
    private = list(
        .run = function() {
            predictors <- unique(c(self$options$covariates, self$options$factors))
            if (is.null(self$options$outcome) || length(predictors) == 0L)
                return()
            formula <- .jr_formula(self$options$outcome, predictors)
            result <- .jr_guided_computation(
                edu_multinomial_logistic(self$data, formula, ci = self$options$ciWidth / 100)
            )
            result <- .jr_apply_variable_descriptions(result, self$data)
            self$results$overview$setContent(.jr_jamovi_overview_html(result))
            self$results$fit$setRow(rowKey = "1", values = as.list(result$statistics[1, ]))
            coefficients <- result$parameters
            for (i in seq_len(nrow(coefficients))) {
                self$results$coefficients$addRow(rowKey = i, values = list(
                    category  = coefficients$category[i],
                    term      = coefficients$term[i],
                    estimate  = coefficients$B[i],
                    se        = coefficients$SE[i],
                    statistic = coefficients$z[i],
                    p         = coefficients$p[i],
                    rrr       = coefficients$RRR[i],
                    lower     = coefficients$CI_low[i],
                    upper     = coefficients$CI_high[i]
                ))
            }
            .jr_populate_diagnostics(self$results$diagnostics, result$diagnostics)
            self$results$report$setContent(.jr_guided_report_sections_html(result, self$options))
            self$results$interpretation$setContent(.jr_jamovi_interpretation_html(result))
            self$results$methodsReferences$setContent(.jr_methods_references_html(result))
        }
    )
)
