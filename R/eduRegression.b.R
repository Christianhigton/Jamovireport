#' @importFrom jmvcore .
eduRegressionClass <- if (requireNamespace("jmvcore", quietly = TRUE)) R6Class(
    "eduRegressionClass",
    inherit = eduRegressionBase,
    private = list(
        .run = function() {
            if (is.null(self$options$outcome) || length(self$options$predictors) == 0L)
                return()
            formula <- stats::reformulate(self$options$predictors, response = self$options$outcome)
            result <- .jr_guided_computation(edu_lm(self$data, formula, ci = self$options$ciWidth / 100))
            result <- .jr_apply_variable_descriptions(result, self$data)
            self$results$overview$setContent(.jr_jamovi_overview_html(result))
            self$results$fit$setRow(rowKey = "1", values = as.list(result$statistics[1, ]))
            coefficients <- result$parameters
            for (i in seq_len(nrow(coefficients))) {
                self$results$coefficients$addRow(rowKey = i, values = list(
                    term = coefficients$Parameter[i],
                    estimate = coefficients$Coefficient[i],
                    se = coefficients$SE[i],
                    lower = coefficients$CI_low[i],
                    upper = coefficients$CI_high[i],
                    beta = coefficients$beta[i],
                    statistic = coefficients$t[i],
                    p = coefficients$p[i]
                ))
            }
            .jr_populate_diagnostics(self$results$diagnostics, result$diagnostics)
            self$results$report$setContent(.jr_guided_report_sections_html(result, self$options))
            self$results$interpretation$setContent(.jr_jamovi_interpretation_html(result))
            self$results$methodsReferences$setContent(.jr_methods_references_html(result))
            self$results$plot$setState(result)
        },
        .plot = function(image, ggtheme, theme, ...) {
            if (is.null(image$state))
                return(FALSE)
            edu_plot(image$state) + ggtheme
        }
    )
)

