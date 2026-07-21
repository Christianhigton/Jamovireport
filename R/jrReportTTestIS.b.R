#' @importFrom jmvcore .
jrReportTTestISClass <- if (requireNamespace("jmvcore", quietly = TRUE)) R6Class(
    "jrReportTTestISClass",
    inherit = jrReportTTestISBase,
    private = list(
        .init = function() {
            .jr_addon_insert_card(
                self,
                refs = c("RCore", "jReport", "jmvcore", "effectsize", "BayesFactor",
                         "Cohen1988", "Cumming2014")
            )
        },
        .run = function() {
            group <- self$parent$options$group
            outcomes <- self$parent$options$vars
            if (is.null(group) || length(outcomes) == 0L)
                return()
            results <- list()
            if (isTRUE(self$parent$options$students)) {
                results <- c(results, lapply(outcomes, function(outcome) {
                    .jr_tag_ttest_attempt(try(
                        edu_t_test(
                            self$data, outcome, group = group,
                            var_equal = TRUE, ci = .jr_parent_ci(self$parent)
                        ),
                        silent = TRUE
                    ), paste(outcome, "Student t-test"))
                }))
            }
            if (isTRUE(self$parent$options$welchs)) {
                results <- c(results, lapply(outcomes, function(outcome) {
                    .jr_tag_ttest_attempt(try(
                        edu_t_test(
                            self$data, outcome, group = group,
                            var_equal = FALSE, ci = .jr_parent_ci(self$parent)
                        ),
                        silent = TRUE
                    ), paste(outcome, "Welch t-test"))
                }))
            }
            if (isTRUE(self$parent$options$mann)) {
                results <- c(results, lapply(outcomes, function(outcome) {
                    try(
                        edu_mann_whitney(
                            self$data, outcome, group = group,
                            ci = .jr_parent_ci(self$parent)
                        ),
                        silent = TRUE
                    )
                }))
            }
            if (isTRUE(self$parent$options$bf)) {
                results <- c(results, lapply(outcomes, function(outcome) {
                    try(
                        edu_bayes_t_test(
                            self$data, outcome, group = group,
                            prior_width = self$parent$options$bfPrior
                        ),
                        silent = TRUE
                    )
                }))
            }
            if (!any(vapply(results, inherits, logical(1), what = "edu_analysis"))) {
                .jr_addon_message(self, "Select Student's, Welch's, Mann-Whitney U, or Bayes factor to generate report text.")
                return()
            }
            adjustment <- tryCatch(self$options$pAdjustment, error = function(e) "holm")
            .jr_addon_set_card(
                self, results,
                adjustment = adjustment,
                alpha = 1 - .jr_parent_ci(self$parent)
            )
        }
    )
)
