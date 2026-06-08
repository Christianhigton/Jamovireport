#' @importFrom jmvcore .
jrReportAnovaOneWClass <- if (requireNamespace("jmvcore", quietly = TRUE)) R6Class(
    "jrReportAnovaOneWClass",
    inherit = jrReportAnovaOneWBase,
    private = list(
        .init = function() {
            .jr_addon_insert_card(
                self, posthoc = TRUE,
                refs = c("RCore", "jReport", "jmvcore", "effectsize", "emmeans")
            )
        },
        .run = function() {
            group <- self$parent$options$group
            outcomes <- self$parent$options$deps
            if (is.null(group) || length(outcomes) == 0L)
                return()
            methods <- character()
            if (isTRUE(self$parent$options$fishers))
                methods <- c(methods, "standard")
            if (isTRUE(self$parent$options$welchs))
                methods <- c(methods, "welch")
            if (length(methods) == 0L) {
                .jr_addon_message(self, "Select Fisher's or Welch's one-way ANOVA to generate automatic report text.")
                return()
            }
            results <- unlist(lapply(methods, function(method) {
                lapply(outcomes, function(outcome) {
                    try(
                        edu_anova_oneway(
                            self$data, outcome, group, method = method,
                            ci = .jr_parent_ci(self$parent), posthoc = FALSE
                        ),
                        silent = TRUE
                    )
                })
            }), recursive = FALSE)
            posthoc_method <- self$parent$options$phMethod
            if (!is.null(posthoc_method) && !identical(posthoc_method, "none")) {
                for (i in seq_along(outcomes)) {
                    target <- i
                    if (inherits(results[[target]], "try-error") && length(methods) > 1L)
                        target <- i + length(outcomes)
                    if (.jr_has_significant_omnibus(results[[target]])) {
                        results[[target]]$posthoc_report <- .jr_oneway_posthoc(
                            self$data, outcomes[i], group, posthoc_method
                        )
                    }
                }
            }
            results <- Filter(function(r) !inherits(r, "try-error"), results)
            .jr_addon_set_card(self, results)
        }
    )
)
