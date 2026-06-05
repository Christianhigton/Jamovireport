#' @importFrom jmvcore .
eduRMAnovaClass <- if (requireNamespace("jmvcore", quietly = TRUE)) R6Class(
    "eduRMAnovaClass",
    inherit = eduRMAnovaBase,
    private = list(
        .run = function() {
            if (length(self$options$measures) < 2L)
                return()
            labels <- self$options$occasionLabels
            if (is.null(labels) || !nzchar(trimws(labels))) {
                labels <- self$options$measures
            } else {
                labels <- trimws(strsplit(labels, ",", fixed = TRUE)[[1]])
                if (length(labels) != length(self$options$measures))
                    jmvcore::reject("Number of occasion labels must match the number of repeated measurements.")
            }
            result <- .jr_guided_computation(edu_anova_rm(
                self$data, self$options$measures, labels,
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
