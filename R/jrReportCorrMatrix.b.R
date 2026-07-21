#' @importFrom jmvcore .
jrReportCorrMatrixClass <- if (requireNamespace("jmvcore", quietly = TRUE)) R6Class(
    "jrReportCorrMatrixClass",
    inherit = jrReportCorrMatrixBase,
    private = list(
        .init = function() {
            .jr_addon_insert_card(
                self,
                refs = c("RCore", "jReport", "jmvcore", "effectsize",
                         "Cohen1988", "Cumming2014")
            )
        },
        .run = function() {
            variables <- unique(as.character(self$parent$options$vars))
            if (length(variables) < 2L)
                return()
            selected <- c(
                pearson = isTRUE(self$parent$options$pearson),
                spearman = isTRUE(self$parent$options$spearman),
                kendall = isTRUE(self$parent$options$kendall)
            )
            methods <- names(selected)[selected]
            if (length(methods) == 0L)
                return()
            analysis_data <- self$data
            missing_method <- tryCatch(
                tolower(as.character(self$parent$options$missing)[1]),
                error = function(e) "pairwise"
            )
            if (missing_method %in% c("listwise", "complete")) {
                complete <- stats::complete.cases(analysis_data[, variables, drop = FALSE])
                analysis_data <- analysis_data[complete, , drop = FALSE]
            }
            pairs <- utils::combn(variables, 2, simplify = FALSE)
            results <- unlist(lapply(methods, function(method) {
                lapply(pairs, function(pair) {
                    .jr_tag_correlation_attempt(try(
                        edu_correlation(
                            analysis_data, pair[1], pair[2], method = method,
                            ci = .jr_parent_ci(self$parent)
                        ),
                        silent = TRUE
                    ), method, pair)
                })
            }), recursive = FALSE)
            adjustment <- tryCatch(self$options$pAdjustment, error = function(e) "holm")
            .jr_addon_set_card(
                self, results,
                paste(
                    "A report card is generated for each unique selected correlation method and variable pair.",
                    "Apply one correction only when the correlations form a planned family addressing the same overall research question."
                ),
                adjustment = adjustment,
                alpha = 1 - .jr_parent_ci(self$parent)
            )
        }
    )
)
