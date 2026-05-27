#' @importFrom jmvcore .
eduTTestClass <- if (requireNamespace("jmvcore", quietly = TRUE)) R6Class(
    "eduTTestClass",
    inherit = eduTTestBase,
    private = list(
        .run = function() {
            if (is.null(self$options$outcome))
                return()
            if (self$options$testType == "independent" && is.null(self$options$group))
                return()
            if (self$options$testType == "paired" && is.null(self$options$pairedOutcome))
                return()

            result <- edu_t_test(
                data = self$data,
                outcome = self$options$outcome,
                group = self$options$group,
                paired_outcome = self$options$pairedOutcome,
                type = self$options$testType,
                var_equal = self$options$varEqual,
                ci = self$options$ciWidth / 100
            )
            self$results$overview$setContent(.jr_jamovi_overview_html(result))
            self$results$main$addRow(rowKey = 1, values = as.list(result$statistics[1, ]))

            desc <- result$descriptives
            label <- if ("group" %in% names(desc)) desc$group else desc$condition
            for (i in seq_len(nrow(desc))) {
                self$results$descriptives$addRow(rowKey = i, values = list(
                    label = label[i], n = desc$n[i], mean = desc$mean[i], sd = desc$sd[i]
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
