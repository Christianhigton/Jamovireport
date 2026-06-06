#' @importFrom jmvcore .
jrReportTTestPSClass <- if (requireNamespace("jmvcore", quietly = TRUE)) R6Class(
    "jrReportTTestPSClass",
    inherit = jrReportTTestPSBase,
    private = list(
        .init = function() {
            .jr_addon_insert_card(
                self,
                refs = c("jReport", "jmvcore", "effectsize", "BayesFactor")
            )
        },
        .run = function() {
            pairs <- self$parent$options$pairs
            if (length(pairs) == 0L)
                return()
            results <- list()
            if (isTRUE(self$parent$options$students)) {
                results <- c(results, lapply(pairs, function(pair) {
                    try(
                        edu_t_test(
                            self$data, pair$i1, paired_outcome = pair$i2,
                            type = "paired", ci = .jr_parent_ci(self$parent)
                        ),
                        silent = TRUE
                    )
                }))
            }
            if (isTRUE(self$parent$options$wilcoxon)) {
                results <- c(results, lapply(pairs, function(pair) {
                    try(
                        edu_wilcoxon_signed_rank(
                            self$data, pair$i1, paired_outcome = pair$i2,
                            ci = .jr_parent_ci(self$parent)
                        ),
                        silent = TRUE
                    )
                }))
            }
            if (isTRUE(self$parent$options$bf)) {
                results <- c(results, lapply(pairs, function(pair) {
                    try(
                        edu_bayes_t_test(
                            self$data, pair$i1, paired_outcome = pair$i2,
                            type = "paired", prior_width = self$parent$options$bfPrior
                        ),
                        silent = TRUE
                    )
                }))
            }
            if (length(results) == 0L) {
                .jr_addon_message(self, "Select the paired-samples t-test, Wilcoxon signed-rank test, or Bayes factor to generate report text.")
                return()
            }
            .jr_addon_set_card(self, results)
        }
    )
)
