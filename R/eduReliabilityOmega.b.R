#' @importFrom jmvcore .
eduReliabilityOmegaClass <- if (requireNamespace("jmvcore", quietly = TRUE)) R6Class(
    "eduReliabilityOmegaClass",
    inherit = eduReliabilityOmegaBase,
    private = list(
        .run = function() {
            if (length(self$options$items) < 3L)
                return()
            if (!all(self$options$reverseItems %in% self$options$items))
                jmvcore::reject("Reverse-keyed items must also be selected as scale items.")
            result <- .jr_guided_computation(edu_reliability_omega(
                self$data, self$options$items, self$options$reverseItems,
                correlation = self$options$correlationType,
                ci = self$options$ciWidth / 100,
                bootstrap = self$options$bootstrapCI,
                boot_iterations = self$options$bootstrapSamples
            ))
            result <- .jr_apply_variable_descriptions(result, self$data)
            self$results$overview$setContent(.jr_jamovi_overview_html(result))
            self$results$main$deleteRows()
            for (i in seq_len(nrow(result$statistics)))
                self$results$main$addRow(rowKey = i, values = as.list(result$statistics[i, ]))
            for (i in seq_len(nrow(result$descriptives)))
                self$results$items$addRow(rowKey = i, values = as.list(result$descriptives[i, ]))
            .jr_populate_diagnostics(self$results$diagnostics, result$diagnostics, fixed = TRUE)
            .jr_guided_reporting_output(self, result)
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
