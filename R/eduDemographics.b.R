
eduDemographicsClass <- if (requireNamespace("jmvcore", quietly = TRUE)) R6::R6Class(
    "eduDemographicsClass",
    inherit = eduDemographicsBase,
    private = list(
        .run = function() {
            tv <- self$options$tableVariables
            pv <- self$options$paragraphVariables
            if (length(tv) == 0L && length(pv) == 0L) return(invisible(NULL))

            title <- trimws(self$options$tableTitle)
            if (!nzchar(title))
                title <- "Demographic Characteristics of the Sample"
            self$results$demographics$setTitle(title)

            dm <- .jr_guided_computation(
                edu_demographics(
                    self$data,
                    table_variables     = tv,
                    paragraph_variables = pv,
                    stat_mean           = self$options$statMean,
                    stat_sd             = self$options$statSD,
                    stat_median         = self$options$statMedian,
                    stat_iqr            = self$options$statIQR,
                    stat_min            = self$options$statMin,
                    stat_max            = self$options$statMax,
                    stat_range          = self$options$statRange,
                    stat_cont_missing   = self$options$statContMissing,
                    stat_n              = self$options$statN,
                    stat_pct            = self$options$statPct,
                    stat_cat_missing    = self$options$statCatMissing,
                    custom_rows         = .dm_custom_rows_from_options(self$options)
                )
            )

            for (i in seq_along(dm$table_rows))
                self$results$demographics$addRow(rowKey = i, values = dm$table_rows[[i]])

            self$results$omitNote$setContent(.dm_omit_note_html(dm$table_omit, dm$para_omit))
            self$results$paragraph$setContent(.dm_paragraph_html(dm$paragraph))
            self$results$bestPractices$setContent(.dm_best_practices_html())
            self$results$methodsReferences$setContent(.jr_methods_references_html(keys = c("jReport", "RCore")))
        }
    )
)

.dm_best_practices_html <- function() {
    sections <- list(
        "What this output describes" = paste(
            "The demographic table and paragraph describe the analysed sample.",
            "They do not test group differences or establish that the sample represents a wider population."
        ),
        "Check the variables and data" = .jr_guidance_block(bullets = c(
            "Confirm that each selected variable is demographic or otherwise appropriate for sample description.",
            "Check category labels, valid sample sizes and the treatment of missing values.",
            "Check that percentages use the intended denominator and that continuous summaries suit each distribution."
        )),
        "Descriptive information" = paste(
            "Use M (SD) for a roughly symmetric continuous distribution and median [IQR] when a resistant summary is more informative.",
            "Report n and percentage for categorical levels. Percentages may not total exactly 100% because of rounding."
        ),
        "Missing data and uncertainty" = paste(
            "Enable the missing-count option when omissions matter and describe the study's missing-data strategy in the Method section.",
            "Descriptive summaries are sample estimates; avoid implying population precision unless suitable confidence intervals or design-based estimates are available."
        ),
        "Check before using this result" = .jr_guidance_block(bullets = c(
            "Verify the total N and all displayed values against the source data.",
            "Keep the paragraph descriptive and adapt its wording to the study.",
            "Apply the target document's APA table styling after export and cite the table by its final number.",
            "Do not add significance tests merely to describe baseline or demographic characteristics without a substantive question."
        )),
        "Literature and guidance" = paste(
            "Use the current APA style guidance and the study area's reporting standards for demographic tables.",
            "The software and module citations are provided in the References output."
        )
    )
    paste0(
        "<div style='width:100%;box-sizing:border-box;display:block;'>",
        .jr_interpretation_guidance_panel(sections),
        "</div>"
    )
}
