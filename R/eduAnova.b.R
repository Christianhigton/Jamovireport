#' @importFrom jmvcore .
eduAnovaClass <- if (requireNamespace("jmvcore", quietly = TRUE)) R6Class(
    "eduAnovaClass",
    inherit = eduAnovaBase,
    private = list(
        .run = function() {
            if (is.null(self$options$outcome) || is.null(self$options$group))
                return()
            result <- .jr_guided_computation(edu_anova_oneway(
                self$data, self$options$outcome, self$options$group,
                method = self$options$method, ci = self$options$ciWidth / 100,
                posthoc = self$options$posthoc
            ))
            result <- .jr_apply_variable_descriptions(result, self$data)
            self$results$overview$setContent(.jr_jamovi_overview_html(result))
            self$results$main$setRow(rowKey = "1", values = as.list(result$statistics[1, ]))
            for (i in seq_len(nrow(result$descriptives))) {
                d <- result$descriptives[i, ]
                self$results$descriptives$addRow(rowKey = i, values = list(
                    label = d$group, n = d$n, mean = d$mean, sd = d$sd
                ))
            }
            .jr_populate_diagnostics(self$results$diagnostics, result$diagnostics, fixed = TRUE)
            self$results$report$setContent(.jr_jamovi_report_html(result, self$options))
            self$results$interpretation$setContent(.jr_jamovi_interpretation_html(result))
            self$results$plot$setState(result)
        },
        .plot = function(image, ggtheme, theme, ...) {
            if (is.null(image$state))
                return(FALSE)
            edu_plot(image$state) + ggtheme
        }
    )
)
