#' @importFrom jmvcore .
eduBetweenAnovaClass <- if (requireNamespace("jmvcore", quietly = TRUE)) R6Class(
    "eduBetweenAnovaClass",
    inherit = eduBetweenAnovaBase,
    private = list(
        .run = function() {
            if (is.null(self$options$outcome) || length(self$options$factors) == 0L)
                return()
            result <- .jr_guided_computation(edu_anova_between(
                self$data, self$options$outcome, self$options$factors,
                ci = self$options$ciWidth / 100
            ))
            result <- .jr_apply_variable_descriptions(result, self$data)
            self$results$overview$setContent(.jr_jamovi_overview_html(result))
            for (i in seq_len(nrow(result$statistics)))
                self$results$main$addRow(rowKey = i, values = as.list(result$statistics[i, ]))
            for (i in seq_len(nrow(result$descriptives))) {
                d <- result$descriptives[i, ]
                self$results$descriptives$addRow(rowKey = i, values = list(label = d$group, n = d$n, mean = d$mean, sd = d$sd))
            }
            .jr_populate_diagnostics(self$results$diagnostics, result$diagnostics, fixed = TRUE)
            self$results$report$setContent(.jr_anova_between_report_sections_html(result, self$options))
            self$results$interpretation$setContent("")
            self$results$plot$setState(result)
        },
        .plot = function(image, ggtheme, theme, ...) {
            if (is.null(image$state))
                return(FALSE)
            edu_plot(image$state) + ggtheme
        }
    )
)
