eduDemographicsClass <- if (requireNamespace("jmvcore", quietly = TRUE)) R6::R6Class(
    "eduDemographicsClass",
    inherit = eduDemographicsBase,
    private = list(
        .run = function() {
            variables <- self$options$variables
            if (length(variables) == 0L) return(invisible(NULL))

            title <- trimws(self$options$tableTitle)
            if (!nzchar(title))
                title <- "Table 1. Demographic Characteristics of the Sample"
            self$results$demographics$setTitle(title)

            custom_rows <- .dm_custom_rows_from_options(self$options)

            dm <- .jr_guided_computation(
                edu_demographics(
                    self$data,
                    variables                   = variables,
                    custom_rows                 = custom_rows,
                    include_custom_in_paragraph = self$options$includeCustomInParagraph
                )
            )

            for (i in seq_along(dm$rows)) {
                self$results$demographics$addRow(rowKey = i, values = dm$rows[[i]])
            }

            if (self$options$includeParagraph) {
                self$results$paragraph$setContent(
                    .dm_paragraph_html(dm$paragraph)
                )
            }
        }
    )
)

.dm_custom_rows_from_options <- function(options) {
    lapply(1:5, function(i) {
        list(
            variable = tryCatch(options[[paste0("customRow", i, "Var")]],  error = function(e) ""),
            category = tryCatch(options[[paste0("customRow", i, "Cat")]],  error = function(e) ""),
            n        = tryCatch(options[[paste0("customRow", i, "N")]],    error = function(e) ""),
            percent  = tryCatch(options[[paste0("customRow", i, "Pct")]], error = function(e) ""),
            note     = tryCatch(options[[paste0("customRow", i, "Note")]], error = function(e) "")
        )
    })
}

.dm_paragraph_html <- function(text) {
    escaped <- gsub("&", "&amp;", text, fixed = TRUE)
    escaped <- gsub("<", "&lt;",  escaped, fixed = TRUE)
    escaped <- gsub(">", "&gt;",  escaped, fixed = TRUE)
    sprintf('<div style="font-family:sans-serif;font-size:0.9em;line-height:1.5;padding:8px 0"><p>%s</p></div>', escaped)
}
