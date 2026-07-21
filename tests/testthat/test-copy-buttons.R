count_fixed <- function(text, pattern) {
    lengths(regmatches(text, gregexpr(pattern, text, fixed = TRUE)))
}

test_that("every report section has its own body-only copy button", {
    html <- .jr_build_report_sections_html(
        apa_wording = "Report wording.",
        diagnostic_note = "Diagnostic note.",
        interpretation_guidance = "Interpretation guidance.",
        checklist_items = c("Check one", "Check two")
    )

    expect_equal(count_fixed(html, "data-jr-copy-section='true'"), 4L)
    expect_equal(count_fixed(html, "aria-label='Copy text'"), 4L)
    expect_equal(count_fixed(html, "data-jr-copy-content='true'"), 4L)
    expect_match(html, "navigator.clipboard")
    expect_match(html, "document.execCommand\\('copy'\\)")

    first_body <- sub(
        ".*?<div data-jr-copy-content='true'>(.*?)</div><div style='clear:both;'>.*",
        "\\1", html
    )
    expect_match(first_body, "Report wording", fixed = TRUE)
    expect_false(grepl("Suggested APA-style report wording", first_body, fixed = TRUE))
    expect_false(grepl("Copy text", first_body, fixed = TRUE))
})

test_that("main and add-on analysis renderers expose copy controls", {
    result <- edu_t_test(ToothGrowth, "len", "supp")
    options <- list(
        reportStyle = "apa7", reportFormat = "paragraph", reportTone = "student_friendly",
        reportDescriptives = TRUE, reportAssumptions = TRUE, reportStatistic = TRUE,
        reportDf = TRUE, reportP = TRUE, reportEffect = TRUE, reportCI = TRUE,
        reportInterpretation = TRUE, reportCautions = TRUE
    )

    main_html <- .jr_guided_report_sections_html(result, options)
    addon_html <- .jr_addon_report_html(list(result), options = .jr_addon_reporting_options())
    interpretation_html <- .jr_addon_interpretation_html(list(result))

    expect_gt(count_fixed(main_html, "aria-label='Copy text'"), 0L)
    expect_gt(count_fixed(addon_html, "aria-label='Copy text'"), 0L)
    expect_gt(count_fixed(interpretation_html, "aria-label='Copy text'"), 0L)
    expect_false(grepl("Interpretation guidance", main_html, fixed = TRUE))
    expect_false(grepl("Interpretation guidance", addon_html, fixed = TRUE))
    expect_match(interpretation_html, "Interpretation guidance", fixed = TRUE)
})

test_that("multiple add-on results identify the analysis in tables and card titles", {
    results <- list(
        edu_correlation(mtcars, "mpg", "wt", method = "pearson"),
        edu_correlation(mtcars, "mpg", "hp", method = "spearman")
    )

    assumptions <- .jr_addon_assumption_rows(results)
    report <- .jr_addon_report_html(
        results, options = .jr_addon_reporting_options()
    )
    interpretation <- .jr_addon_interpretation_html(results)

    expect_setequal(
        unique(assumptions$analysis),
        c("Pearson: mpg with wt", "Spearman: mpg with hp")
    )
    for (label in c("Pearson: mpg with wt", "Spearman: mpg with hp")) {
        expect_match(
            report,
            paste("Optional assumptions / diagnostic note \u2014", label),
            fixed = TRUE
        )
        expect_match(
            report, paste("Check before reporting \u2014", label), fixed = TRUE
        )
        expect_match(
            interpretation,
            paste("Interpretation guidance \u2014", label),
            fixed = TRUE
        )
    }
})

test_that("single add-on results retain compact generic section titles", {
    result <- edu_correlation(mtcars, "mpg", "wt", method = "pearson")
    report <- .jr_addon_report_html(
        list(result), options = .jr_addon_reporting_options()
    )
    assumptions <- .jr_addon_assumption_rows(list(result))

    expect_equal(unique(assumptions$analysis), "Correlation")
    expect_match(report, "Optional assumptions / diagnostic note", fixed = TRUE)
    expect_false(grepl(
        "Optional assumptions / diagnostic note \u2014 Pearson: mpg with wt",
        report, fixed = TRUE
    ))
})

test_that("multi-result labels distinguish t-test outcomes and ANOVA methods", {
    t_data <- ToothGrowth
    t_data$double_len <- 2 * t_data$len
    t_results <- list(
        edu_t_test(t_data, "len", "supp"),
        edu_t_test(t_data, "double_len", "supp")
    )
    a_data <- ToothGrowth
    a_data$dose <- factor(a_data$dose)
    a_results <- list(
        edu_anova_oneway(a_data, "len", "dose", method = "standard"),
        edu_anova_oneway(a_data, "len", "dose", method = "welch")
    )

    expect_equal(
        vapply(t_results, .jr_addon_result_title, character(1)),
        c("Welch's t: len by supp", "Welch's t: double_len by supp")
    )
    expect_equal(
        vapply(a_results, .jr_addon_result_title, character(1)),
        c("One-Way ANOVA: len by dose", "Welch One-Way ANOVA: len by dose")
    )
})

test_that("report cards use compact vertical spacing", {
    html <- .jr_build_report_sections_html(
        apa_wording = "Report wording.",
        diagnostic_note = "Diagnostic note.",
        checklist_items = c("Check one", "Check two")
    )

    expect_match(html, "margin:2px 0 6px 0", fixed = TRUE)
    expect_false(grepl("margin:4px 0 12px 0", html, fixed = TRUE))
})

test_that("demographics text panels are copyable and remain escaped", {
    payload <- "<img src=x onerror=alert(1)>"
    paragraph <- .dm_paragraph_html(payload)
    omission <- .dm_omit_note_html(payload, character())
    practices <- .dm_best_practices_html()

    for (html in list(paragraph, omission, practices)) {
        expect_match(html, "aria-label='Copy text'", fixed = TRUE)
        expect_match(html, "data-jr-copy-content='true'", fixed = TRUE)
    }
    expect_false(grepl("<img", paragraph, fixed = TRUE))
    expect_false(grepl("<img", omission, fixed = TRUE))
    expect_match(paragraph, "&lt;img", fixed = TRUE)
    expect_match(omission, "&lt;img", fixed = TRUE)
})
