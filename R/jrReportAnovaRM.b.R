#' @importFrom jmvcore .
jrReportAnovaRMClass <- if (requireNamespace("jmvcore", quietly = TRUE)) R6Class(
    "jrReportAnovaRMClass",
    inherit = jrReportAnovaRMBase,
    private = list(
        .init = function() {
            .jr_addon_insert_card(
                self, posthoc = TRUE,
                refs = c("jReport", "jmvcore", "afex", "effectsize")
            )
        },
        .run = function() {
            cells <- self$parent$options$rmCells
            within <- self$parent$options$rm
            between <- self$parent$options$bs
            covariates <- self$parent$options$cov
            if (is.null(cells) || length(cells) < 2L)
                return()
            if (length(within) != 1L) {
                .jr_addon_message(self, "Automatic reporting currently supports one repeated-measures factor; multi-factor within-subject reporting is not yet automated.")
                return()
            }
            if (length(covariates) > 0L) {
                .jr_addon_message(self, "Repeated-measures ANCOVA reporting is not yet automated in this add-on because covariate interpretation requires additional model checks.")
                return()
            }
            measures <- vapply(cells, function(cell) cell$measure, character(1))
            labels <- vapply(cells, function(cell) paste(cell$cell, collapse = " x "), character(1))
            if (any(!nzchar(measures)))
                return()
            if (length(between) == 0L) {
                result <- try(edu_anova_rm(self$data, measures, labels, ci = .jr_parent_ci(self$parent)), silent = TRUE)
            } else if (length(between) == 1L) {
                result <- try(edu_anova_mixed(self$data, measures, between, labels, ci = .jr_parent_ci(self$parent)), silent = TRUE)
            } else {
                .jr_addon_message(self, "Automatic mixed-ANOVA reporting currently supports one between-subjects factor; more complex factorial mixed reporting is not yet automated.")
                return()
            }
            if (inherits(result, "edu_analysis")) {
                within_label <- within[[1]]$label
                term_map <- list()
                term_map[[within_label]] <- "occasion"
                if (length(between) == 1L)
                    term_map[[between]] <- "group"
                result$posthoc_report <- .jr_model_posthoc(
                    result,
                    self$parent$options$postHoc,
                    self$parent$options$postHocCorr,
                    term_map = term_map
                )
            }
            .jr_addon_set_card(
                self, list(result),
                .jr_accuracy_note("This generated paragraph summarises the selected repeated-measures or mixed design.")
            )
        }
    )
)
