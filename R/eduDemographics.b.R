
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
        }
    )
)

.dm_best_practices_html <- function() {
    '<div style="font-family:sans-serif;font-size:0.88em;line-height:1.6;
        border-top:2px solid #ccc;padding:10px 0 4px">
<p style="margin:0 0 6px"><strong>APA 7 Demographic Table — Reporting Best Practices</strong></p>

<p style="margin:0 0 5px"><strong>Table format.</strong>
This table follows APA 7 conventions. After pasting into Word: (1) italicise the table title,
(2) apply thin horizontal borders above and below the column headers and at the table foot only
(no vertical lines or shading), and (3) bold the word <em>Note.</em> in any table note.</p>

<p style="margin:0 0 5px"><strong>Continuous variables.</strong>
Report <em>M</em>&nbsp;(<em>SD</em>) for approximately normally distributed variables (e.g., age,
years of education). For markedly skewed distributions use Median&nbsp;[IQR] instead. Always report
the full range for age (e.g., <em>ranged from 18 to 65 years, M</em>&nbsp;=&nbsp;32.4,
<em>SD</em>&nbsp;=&nbsp;11.2).</p>

<p style="margin:0 0 5px"><strong>Categorical variables.</strong>
Report <em>n</em> and % for each category. Percentages are calculated from the valid (non-missing)
<em>N</em>. If the displayed percentages do not sum to 100 due to rounding, add a table note:
"<em>Note.</em> Percentages may not sum to 100 due to rounding."</p>

<p style="margin:0 0 5px"><strong>Missing data.</strong>
Enable "Missing n" to show the count of missing values per variable. Describe your missing-data
strategy (e.g., listwise deletion, multiple imputation) in the Method section.</p>

<p style="margin:0 0 5px"><strong>Paragraph.</strong>
Begin the paragraph with the total sample size, then describe each variable. Keep the tone
descriptive — save significance testing for the Results section. Edit the auto-generated
paragraph above to match your specific phrasing before submitting.</p>

<p style="margin:0"><strong>Table citation.</strong>
Reference the table in-text as "Table&nbsp;1" (or whichever number applies) the first time you
mention it: e.g., "Demographic characteristics of the sample are presented in Table&nbsp;1."</p>
</div>'
}
