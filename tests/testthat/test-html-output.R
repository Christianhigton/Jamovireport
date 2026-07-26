# Tests that the reporting and verification sections produced by jReport
# contain substantive content for every guided analysis. Interpretation is
# tested separately because it has its own jamovi result panel.
#
# "Appropriate size" means:
#   - The report wording and verification headings appear
#   - The APA wording section contains at least MIN_APA_CHARS of plain text
#   - The checklist section contains at least MIN_CHECKLIST_ITEMS bullet items
#   - No section collapses to an empty box (just a heading with nothing inside)

MIN_APA_CHARS       <- 80L   # shortest plausible APA sentence
MIN_CHECKLIST_ITEMS <- 5L    # every analysis has at least 5 checklist items

# ---------------------------------------------------------------------------
# Helpers
# ---------------------------------------------------------------------------

strip_tags  <- function(html) gsub("<[^>]+>", " ", html)
strip_ents  <- function(text) gsub("&[a-zA-Z0-9#]+;", " ", text)
plain_text  <- function(html) trimws(gsub("\\s+", " ", strip_ents(strip_tags(html))))
plain_nchar <- function(html) nchar(plain_text(html))

section_present <- function(html, heading) grepl(heading, html, fixed = TRUE)
count_bullets   <- function(html) lengths(regmatches(html, gregexpr("<li ", html, fixed = TRUE)))

# Calls the private HTML builder via triple-colon for white-box testing
report_html <- function(result) {
    jReport:::.jr_guided_report_sections_html(result, list(
        reportStyle          = "apa7",
        reportFormat         = "paragraph",
        reportTone           = "student_friendly",
        reportDescriptives   = TRUE,
        reportAssumptions    = TRUE,
        reportStatistic      = TRUE,
        reportDf             = TRUE,
        reportP              = TRUE,
        reportEffect         = TRUE,
        reportCI             = TRUE,
        reportInterpretation = TRUE,
        reportCautions       = TRUE
    ))
}

# ---------------------------------------------------------------------------
# Data fixtures
# ---------------------------------------------------------------------------

data(iris)

two_sp <- iris[iris$Species != "virginica", ]
two_sp$Species <- droplevels(two_sp$Species)

logistic_binary <- mtcars
logistic_binary$am <- factor(logistic_binary$am, labels = c("automatic", "manual"))

multinomial_data <- warpbreaks

# ---------------------------------------------------------------------------
# Per-analysis report/guidance separation
# ---------------------------------------------------------------------------

test_that("guided analyses use the standard three-part reporting structure", {
    d <- iris
    d$sample_block <- factor(rep(c("A", "B"), length.out = nrow(d)))
    chi_data <- data.frame(
        petal_size = factor(ifelse(iris$Petal.Length > 3.5, "large", "small")),
        sepal_size = factor(ifelse(iris$Sepal.Length > 5.8, "large", "small"))
    )
    results <- list(
        edu_t_test(two_sp, "Sepal.Length", "Species"),
        edu_anova_oneway(iris, "Petal.Length", "Species"),
        edu_correlation(iris, "Sepal.Length", "Petal.Length"),
        edu_lm(iris, Sepal.Length ~ Petal.Length + Petal.Width),
        edu_logistic_regression(logistic_binary, am ~ wt + hp),
        edu_multinomial_logistic(multinomial_data, tension ~ breaks + wool),
        edu_anova_between(d, "Sepal.Length", c("Species", "sample_block")),
        edu_ancova(iris, "Sepal.Length", "Species", "Petal.Length"),
        edu_manova(iris, c("Sepal.Length", "Sepal.Width"), "Species"),
        edu_reliability_omega(
            iris[, 1:4],
            items = c("Sepal.Length", "Sepal.Width", "Petal.Length", "Petal.Width"),
            bootstrap = FALSE
        ),
        edu_chisq_independence(chi_data, "petal_size", "sepal_size")
    )

    for (result in results) {
        report <- report_html(result)
        guidance <- jReport:::.jr_jamovi_interpretation_html(result)
        references <- jReport:::.jr_methods_references_html(result)

        expect_true(section_present(report, "Suggested APA report"))
        expect_false(section_present(report, "Interpretation guidance"))
        expect_false(section_present(report, "Check before using this result"))
        expect_gte(plain_nchar(report), MIN_APA_CHARS)

        expect_true(section_present(guidance, "Interpretation guidance"))
        expect_true(section_present(guidance, "What this analysis examines"))
        expect_true(section_present(guidance, "Assumptions and diagnostics"))
        expect_true(section_present(guidance, "Check before using this result"))
        expect_true(section_present(guidance, "Literature and guidance"))
        expect_true(grepl("<details", guidance, fixed = TRUE))
        expect_false(grepl("<details open", guidance, fixed = TRUE))
        expect_gte(count_bullets(guidance), 4L)

        expect_true(section_present(references, "R packages and software"))
        expect_true(section_present(references, "Literature and reporting guidance"))
    }
})

# ---------------------------------------------------------------------------
# Box structure / HTML integrity tests
# ---------------------------------------------------------------------------

test_that("HTML cards have balanced opening and closing div tags", {
    result <- edu_t_test(two_sp, "Sepal.Length", "Species")
    html   <- report_html(result)

    opens  <- lengths(regmatches(html, gregexpr("<div", html, fixed = TRUE)))
    closes <- lengths(regmatches(html, gregexpr("</div>", html, fixed = TRUE)))
    expect_equal(opens, closes)
})

test_that("section cards have the correct accent colours", {
    result <- edu_t_test(two_sp, "Sepal.Length", "Species")
    html   <- report_html(result)
    interpretation <- jReport:::.jr_jamovi_interpretation_html(result)

    expect_true(grepl("#278058", html, fixed = TRUE))   # APA wording — green
    expect_false(grepl("#b46c21", html, fixed = TRUE))  # no duplicate interpretation card
    expect_true(grepl("#b46c21", interpretation, fixed = TRUE))
    expect_true(grepl("#fff9ef", interpretation, fixed = TRUE))
})

test_that("HTML card content is HTML-escaped — no raw < or > in text content", {
    # Manufacture a result whose text contains characters that need escaping
    result <- edu_correlation(iris, "Sepal.Length", "Petal.Length")
    # Inject a text snippet with angle brackets via interpretation field
    result$interpretation <- "Use < and > carefully when reporting."
    html <- jReport:::.jr_jamovi_interpretation_html(result)

    # After escaping, raw < should not appear in text nodes
    # (only inside tag delimiters themselves)
    text_only <- plain_text(html)
    expect_false(grepl("<", text_only, fixed = TRUE))
    expect_false(grepl(">", text_only, fixed = TRUE))
    # But the escaped forms should be present somewhere in the raw HTML
    expect_true(grepl("&lt;", html, fixed = TRUE))
    expect_true(grepl("&gt;", html, fixed = TRUE))
})

test_that("empty optional sections are silently omitted, not rendered as blank boxes", {
    # Build HTML with no diagnostic note and no interpretation
    html <- jReport:::.jr_build_report_sections_html(
        apa_wording             = "A test was run, F(1, 98) = 4.5, p = .036.",
        diagnostic_note         = "",
        interpretation_guidance = "",
        checklist_items         = c("Check A", "Check B", "Check C",
                                    "Check D", "Check E")
    )

    # APA and checklist appear; optional sections are absent
    expect_true(section_present(html, "Suggested APA report"))
    expect_true(section_present(html, "Check before reporting"))
    expect_false(section_present(html, "Optional assumptions / diagnostic note"))
    expect_false(section_present(html, "Interpretation guidance"))
    # No empty divs with just the heading and nothing else
    expect_false(grepl("<p style[^>]*></p>", html))
})

test_that("checklist items are individually wrapped in <li> tags", {
    html <- jReport:::.jr_build_report_sections_html(
        apa_wording     = "Sentence.",
        checklist_items = c("Item one", "Item two", "Item three")
    )

    expect_equal(count_bullets(html), 3L)
    expect_true(grepl("Item one", html, fixed = TRUE))
    expect_true(grepl("Item three", html, fixed = TRUE))
})

test_that("legacy section builder remains expanded while structured guidance collapses", {
    html <- jReport:::.jr_build_report_sections_html(
        apa_wording = "A test was run, F(1, 98) = 4.5, p = .036.",
        diagnostic_note = "Assumptions should be checked.",
        interpretation_guidance = "Interpret this in context.",
        checklist_items = c("Check A", "Check B")
    )

    expect_false(grepl("<details", html, fixed = TRUE))
    expect_match(html, "Optional assumptions / diagnostic note", fixed = TRUE)
    expect_match(html, "Interpretation guidance", fixed = TRUE)
    expect_match(html, "Check before reporting", fixed = TRUE)
    expect_match(html, "Suggested APA report", fixed = TRUE)

    result <- edu_t_test(two_sp, "Sepal.Length", "Species")
    guidance <- jReport:::.jr_jamovi_interpretation_html(result)
    expect_match(guidance, "<details", fixed = TRUE)
    expect_false(grepl("<details open", guidance, fixed = TRUE))
})

test_that("guided report heading follows selected report style", {
    result <- edu_t_test(ToothGrowth, "len", "supp")
    base_options <- list(
        reportFormat         = "paragraph",
        reportTone           = "student_friendly",
        reportDescriptives   = TRUE,
        reportAssumptions    = TRUE,
        reportStatistic      = TRUE,
        reportDf             = TRUE,
        reportP              = TRUE,
        reportEffect         = TRUE,
        reportCI             = TRUE,
        reportInterpretation = TRUE,
        reportCautions       = TRUE
    )

    plain <- jReport:::.jr_guided_report_sections_html(
        result,
        c(base_options, list(reportStyle = "plain"))
    )
    journal <- jReport:::.jr_guided_report_sections_html(
        result,
        c(base_options, list(reportStyle = "journal"))
    )
    dissertation <- jReport:::.jr_guided_report_sections_html(
        result,
        c(base_options, list(reportStyle = "dissertation"))
    )

    expect_true(section_present(plain, "Suggested plain-language report wording"))
    expect_true(section_present(journal, "Suggested journal-style report wording"))
    expect_true(section_present(dissertation, "Suggested dissertation-style report wording"))
    expect_false(section_present(plain, "Suggested APA report"))
})

test_that("html_card escapes special characters in eyebrow and title", {
    html <- jReport:::.jr_html_card(
        eyebrow = "Section & notes",
        title   = "Result <p> here",
        content = "Normal content."
    )

    expect_true(grepl("Section &amp; notes", html, fixed = TRUE))
    expect_true(grepl("Result &lt;p&gt; here", html, fixed = TRUE))
    expect_false(grepl("<p> here", html, fixed = TRUE))
})

test_that("html_bullets returns empty string for zero-length input", {
    expect_equal(jReport:::.jr_html_bullets(character(0)), "")
    expect_equal(jReport:::.jr_html_bullets(c("", "")), "")
})

test_that("html_paragraphs wraps each double-newline block in its own <p>", {
    html <- jReport:::.jr_html_paragraphs("First paragraph.\n\nSecond paragraph.")

    expect_equal(
        lengths(regmatches(html, gregexpr("<p ", html, fixed = TRUE))),
        2L
    )
})
