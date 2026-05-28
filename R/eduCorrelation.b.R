#' @importFrom jmvcore .
eduCorrelationClass <- if (requireNamespace("jmvcore", quietly = TRUE)) R6Class(
    "eduCorrelationClass",
    inherit = eduCorrelationBase,
    private = list(
        .run = function() {
            if (is.null(self$options$x) || is.null(self$options$y))
                return()
            result <- edu_correlation(
                self$data, self$options$x, self$options$y,
                method = self$options$method, ci = self$options$ciWidth / 100
            )
            result <- .jr_apply_variable_descriptions(result, self$data)
            self$results$overview$setContent(.jr_jamovi_overview_html(result))
            self$results$main$addRow(rowKey = 1, values = as.list(result$statistics[1, ]))
            for (i in seq_len(nrow(result$descriptives))) {
                d <- result$descriptives[i, ]
                self$results$descriptives$addRow(rowKey = i, values = list(
                    label = d$variable, n = d$n, mean = d$mean, sd = d$sd
                ))
            }
            .jr_populate_diagnostics(self$results$diagnostics, result$diagnostics)
            self$results$report$setContent(.jr_jamovi_report_html(result, self$options))
            self$results$interpretation$setContent(.jr_jamovi_interpretation_html(result))
            if (self$options$showPlot)
                self$results$plot$setState(result)
        },
        .plot = function(image, ggtheme, theme, ...) {
            if (is.null(image$state))
                return(FALSE)
            edu_plot(image$state) + ggtheme
        }
    )
)
