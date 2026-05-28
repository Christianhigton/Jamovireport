#' @importFrom jmvcore .
eduAncovaClass <- if (requireNamespace("jmvcore", quietly = TRUE)) R6Class(
    "eduAncovaClass",
    inherit = eduAncovaBase,
    private = list(
        .run = function() {
            if (is.null(self$options$outcome) || length(self$options$factors) == 0L ||
                length(self$options$covariates) == 0L)
                return()
            result <- edu_ancova(
                self$data, self$options$outcome, self$options$factors,
                self$options$covariates, ci = self$options$ciWidth / 100
            )
            result <- .jr_apply_variable_descriptions(result, self$data)
            self$results$overview$setContent(.jr_jamovi_overview_html(result))
            for (i in seq_len(nrow(result$statistics)))
                self$results$main$addRow(rowKey = i, values = as.list(result$statistics[i, ]))
            for (i in seq_len(nrow(result$descriptives))) {
                d <- result$descriptives[i, ]
                self$results$descriptives$addRow(rowKey = i, values = list(label = d$group, n = d$n, mean = d$mean, sd = d$sd))
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
