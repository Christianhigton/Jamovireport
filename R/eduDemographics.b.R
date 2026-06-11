
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

            omit_html <- .dm_omit_note_html(dm$table_omit, dm$para_omit)
            self$results$omitNote$setContent(omit_html)

            self$results$paragraph$setContent(.dm_paragraph_html(dm$paragraph))
        }
    )
)
