# Unit tests for the redesigned eduDemographics module.
# All tests use built-in datasets (iris, mtcars) so no external files are needed.

data(iris)
data(mtcars)

# --------------------------------------------------------------------------
# Helpers
# --------------------------------------------------------------------------

iris_mixed <- function() {
    d <- iris
    d$large_petal <- factor(ifelse(d$Petal.Length > median(d$Petal.Length),
                                   "large", "small"))
    d
}

# --------------------------------------------------------------------------
# edu_demographics — return structure
# --------------------------------------------------------------------------

test_that("edu_demographics returns the expected list structure", {
    dm <- edu_demographics(iris,
                           table_variables = c("Sepal.Length", "Species"),
                           paragraph_variables = c("Sepal.Length", "Species"))
    expect_type(dm, "list")
    expect_named(dm, c("table_rows", "paragraph", "total_n", "table_omit", "para_omit"))
    expect_equal(dm$total_n, 150L)
    expect_gt(length(dm$table_rows), 0L)
    expect_type(dm$paragraph, "character")
    expect_gt(nchar(dm$paragraph), 10L)
})

test_that("table_rows rows each have characteristic and value fields", {
    dm <- edu_demographics(iris,
                           table_variables = c("Sepal.Length", "Species"))
    for (r in dm$table_rows)
        expect_named(r, c("characteristic", "value"))
})

test_that("edu_demographics total_n equals nrow(data)", {
    dm <- edu_demographics(iris, table_variables = "Sepal.Length")
    expect_equal(dm$total_n, nrow(iris))
})

test_that("paragraph mentions total N", {
    dm <- edu_demographics(iris,
                           table_variables = "Sepal.Length",
                           paragraph_variables = "Sepal.Length")
    expect_true(grepl("150", dm$paragraph))
})

# --------------------------------------------------------------------------
# Continuous variable rows
# --------------------------------------------------------------------------

test_that("continuous variable produces one row with M (SD) in value", {
    dm   <- edu_demographics(iris, table_variables = "Sepal.Length")
    rows <- dm$table_rows
    expect_equal(length(rows), 1L)
    expect_equal(rows[[1]]$characteristic, "Sepal.Length")
    expect_match(rows[[1]]$value, "[0-9]+\\.[0-9]+")
})

test_that("mean+SD default produces value like '5.84 (0.83)'", {
    dm  <- edu_demographics(iris, table_variables = "Sepal.Length",
                            stat_mean = TRUE, stat_sd = TRUE)
    val <- dm$table_rows[[1]]$value
    expect_match(val, "^[0-9]+\\.[0-9]+ \\([0-9]+\\.[0-9]+\\)$")
})

test_that("stat_mean=FALSE stat_sd=FALSE produces empty value cell", {
    dm  <- edu_demographics(iris, table_variables = "Sepal.Length",
                            stat_mean = FALSE, stat_sd = FALSE)
    expect_equal(dm$table_rows[[1]]$value, "")
})

test_that("stat_median produces 'Mdn = X.XX' in value", {
    dm  <- edu_demographics(iris, table_variables = "Sepal.Length",
                            stat_mean = FALSE, stat_sd = FALSE, stat_median = TRUE)
    expect_match(dm$table_rows[[1]]$value, "Mdn =")
})

test_that("stat_range adds range to value string", {
    dm  <- edu_demographics(iris, table_variables = "Sepal.Length",
                            stat_range = TRUE)
    expect_match(dm$table_rows[[1]]$value, "range ")
})

test_that("stat_min stat_max adds min and max to value string", {
    dm  <- edu_demographics(iris, table_variables = "Sepal.Length",
                            stat_mean = FALSE, stat_sd = FALSE,
                            stat_min = TRUE, stat_max = TRUE)
    val <- dm$table_rows[[1]]$value
    expect_match(val, "min =")
    expect_match(val, "max =")
})

test_that("stat_cont_missing adds a missing row when NAs present", {
    d <- iris
    d$Sepal.Length[1:10] <- NA
    dm <- edu_demographics(d, table_variables = "Sepal.Length",
                            stat_cont_missing = TRUE)
    chars <- vapply(dm$table_rows, `[[`, character(1), "characteristic")
    expect_true(any(grepl("missing", chars, ignore.case = TRUE)))
})

test_that("stat_cont_missing does not add row when no NAs", {
    dm <- edu_demographics(iris, table_variables = "Sepal.Length",
                            stat_cont_missing = TRUE)
    expect_equal(length(dm$table_rows), 1L)
})

# --------------------------------------------------------------------------
# Categorical variable rows
# --------------------------------------------------------------------------

test_that("categorical variable produces header + one row per level", {
    dm     <- edu_demographics(iris, table_variables = "Species")
    rows   <- dm$table_rows
    n_levs <- nlevels(iris$Species)
    expect_equal(length(rows), n_levs + 1L)
    expect_equal(rows[[1]]$characteristic, "Species")
    expect_equal(rows[[1]]$value, "")
})

test_that("level rows have indented characteristic names", {
    dm   <- edu_demographics(iris, table_variables = "Species")
    levs <- levels(iris$Species)
    for (i in seq_along(levs))
        expect_equal(dm$table_rows[[i + 1L]]$characteristic, paste0("  ", levs[i]))
})

test_that("level row values contain n and pct when both enabled", {
    dm  <- edu_demographics(iris, table_variables = "Species",
                            stat_n = TRUE, stat_pct = TRUE)
    val <- dm$table_rows[[2]]$value
    expect_match(val, "^[0-9]+ \\([0-9]+\\.[0-9]+%\\)$")
})

test_that("stat_n=TRUE stat_pct=FALSE gives integer-only level values", {
    dm  <- edu_demographics(iris, table_variables = "Species",
                            stat_n = TRUE, stat_pct = FALSE)
    val <- dm$table_rows[[2]]$value
    expect_match(val, "^[0-9]+$")
})

test_that("stat_n=FALSE stat_pct=TRUE gives pct-only level values", {
    dm  <- edu_demographics(iris, table_variables = "Species",
                            stat_n = FALSE, stat_pct = TRUE)
    val <- dm$table_rows[[2]]$value
    expect_match(val, "%$")
    expect_false(grepl("^[0-9]+ \\(", val))
})

test_that("pct values across all levels of a factor sum to approximately 100", {
    dm     <- edu_demographics(iris, table_variables = "Species",
                               stat_n = FALSE, stat_pct = TRUE)
    level_rows <- dm$table_rows[-1L]
    pcts <- vapply(level_rows, function(r) {
        as.numeric(sub("%", "", r$value))
    }, numeric(1))
    # Displayed 1-decimal values may differ from 100 by up to 0.5 due to rounding
    expect_true(abs(sum(pcts) - 100) < 0.5)
})

test_that("stat_cat_missing adds missing row when NAs present", {
    d <- iris
    d$Species[1:5] <- NA
    dm <- edu_demographics(d, table_variables = "Species",
                            stat_cat_missing = TRUE)
    chars <- vapply(dm$table_rows, `[[`, character(1), "characteristic")
    expect_true(any(grepl("Missing", chars)))
})

test_that("character variable is coerced to factor", {
    d       <- iris
    d$label <- as.character(d$Species)
    dm      <- edu_demographics(d, table_variables = "label")
    chars   <- vapply(dm$table_rows, `[[`, character(1), "characteristic")
    expect_true("label" %in% chars)
})

# --------------------------------------------------------------------------
# Table vs paragraph variable routing
# --------------------------------------------------------------------------

test_that("variable in table_variables appears in table_rows", {
    dm <- edu_demographics(iris,
                           table_variables     = "Sepal.Length",
                           paragraph_variables = character(0))
    expect_gt(length(dm$table_rows), 0L)
})

test_that("variable in paragraph_variables but not table produces no table rows", {
    dm <- edu_demographics(iris,
                           table_variables     = character(0),
                           paragraph_variables = "Sepal.Length")
    expect_equal(length(dm$table_rows), 0L)
    expect_match(dm$paragraph, "Sepal.Length")
})

test_that("variable in both lists appears in both table and paragraph", {
    dm <- edu_demographics(iris,
                           table_variables     = "Species",
                           paragraph_variables = "Species")
    expect_gt(length(dm$table_rows), 0L)
    expect_match(dm$paragraph, "Species")
})

test_that("paragraph does not mention table-only variable", {
    dm <- edu_demographics(iris,
                           table_variables     = "Sepal.Length",
                           paragraph_variables = character(0))
    expect_false(grepl("Sepal.Length", dm$paragraph))
})

# --------------------------------------------------------------------------
# All-NA variable → omit tracking
# --------------------------------------------------------------------------

test_that("all-NA table variable is added to table_omit and excluded from table", {
    d       <- iris
    d$bad   <- NA_real_
    dm      <- edu_demographics(d,
                                table_variables     = c("Sepal.Length", "bad"),
                                paragraph_variables = character(0))
    expect_true("bad" %in% dm$table_omit)
    chars <- vapply(dm$table_rows, `[[`, character(1), "characteristic")
    expect_false("bad" %in% chars)
})

test_that("all-NA paragraph variable is added to para_omit", {
    d     <- iris
    d$bad <- NA_real_
    dm    <- edu_demographics(d,
                              table_variables     = character(0),
                              paragraph_variables = c("Sepal.Length", "bad"))
    expect_true("bad" %in% dm$para_omit)
})

test_that("no omit lists when all variables have data", {
    dm <- edu_demographics(iris,
                           table_variables     = "Sepal.Length",
                           paragraph_variables = "Species")
    expect_length(dm$table_omit, 0L)
    expect_length(dm$para_omit,  0L)
})

# --------------------------------------------------------------------------
# Custom rows (new Char/Val/Pct/Note schema)
# --------------------------------------------------------------------------

test_that("non-blank custom rows appear at the end of table_rows", {
    cr <- list(
        list(characteristic = "Education", value = "45", pct = "60", note = ""),
        list(characteristic = "",          value = "",   pct = "",   note = "")
    )
    dm <- edu_demographics(iris[, character(0)],
                           table_variables = character(0),
                           custom_rows     = cr)
    expect_equal(length(dm$table_rows), 1L)
    expect_equal(dm$table_rows[[1]]$characteristic, "Education")
    expect_match(dm$table_rows[[1]]$value, "45 \\(60%\\)")
})

test_that("blank custom rows (all fields empty) are silently omitted", {
    cr <- lapply(1:5, function(i)
        list(characteristic = "", value = "", pct = "", note = ""))
    dm <- edu_demographics(iris, table_variables = "Sepal.Length", custom_rows = cr)
    expect_equal(length(dm$table_rows), 1L)
})

test_that("custom row with only characteristic is kept", {
    cr <- list(list(characteristic = "Site", value = "", pct = "", note = ""))
    dm <- edu_demographics(iris[, character(0)],
                           table_variables = character(0),
                           custom_rows     = cr)
    expect_equal(length(dm$table_rows), 1L)
    expect_equal(dm$table_rows[[1]]$characteristic, "Site")
})

test_that("custom row pct-only (no value) formats as '(X%)'", {
    cr <- list(list(characteristic = "X", value = "", pct = "42.5", note = ""))
    dm <- edu_demographics(iris[, character(0)],
                           table_variables = character(0),
                           custom_rows     = cr)
    expect_match(dm$table_rows[[1]]$value, "^\\(42.5%\\)$")
})

test_that("custom row note appended to value with semicolon separator", {
    cr <- list(list(characteristic = "X", value = "30", pct = "", note = "see Table 2"))
    dm <- edu_demographics(iris[, character(0)],
                           table_variables = character(0),
                           custom_rows     = cr)
    expect_match(dm$table_rows[[1]]$value, "see Table 2")
    expect_match(dm$table_rows[[1]]$value, ";")
})

test_that("custom rows are appended after variable rows", {
    cr <- list(list(characteristic = "Site", value = "Online", pct = "", note = ""))
    dm <- edu_demographics(iris,
                           table_variables = "Sepal.Length",
                           custom_rows     = cr)
    last_row <- tail(dm$table_rows, 1L)[[1L]]
    expect_equal(last_row$characteristic, "Site")
})

# --------------------------------------------------------------------------
# eduDemographics jamovi entry point (new API)
# --------------------------------------------------------------------------

test_that("eduDemographics entry point runs on iris with table and paragraph vars", {
    expect_no_error(
        eduDemographics(iris_mixed(),
                        tableVariables     = c("Sepal.Length", "Species"),
                        paragraphVariables = c("Sepal.Length", "Species", "large_petal"))
    )
})

test_that("eduDemographics with table-only variable runs without error", {
    expect_no_error(
        eduDemographics(iris, tableVariables = "Sepal.Length")
    )
})

test_that("eduDemographics with paragraph-only variable runs without error", {
    expect_no_error(
        eduDemographics(iris, paragraphVariables = "Species")
    )
})

test_that("eduDemographics stat toggles reach the output", {
    expect_no_error(
        eduDemographics(iris,
                        tableVariables = "Sepal.Length",
                        statMean = TRUE, statSD = TRUE,
                        statMedian = TRUE, statIQR = TRUE,
                        statRange = TRUE)
    )
})

test_that("eduDemographics custom rows run without error", {
    expect_no_error(
        eduDemographics(
            iris,
            tableVariables = "Sepal.Length",
            customRow1Char = "Education",
            customRow1Val  = "45",
            customRow1Pct  = "30",
            customRow1Note = "self-reported"
        )
    )
})

test_that("eduDemographics with all blank custom rows does not crash", {
    expect_no_error(
        eduDemographics(
            iris,
            tableVariables = "Sepal.Length",
            customRow1Char = "", customRow1Val = "", customRow1Pct = "", customRow1Note = "",
            customRow2Char = "", customRow2Val = "", customRow2Pct = "", customRow2Note = ""
        )
    )
})

test_that("eduDemographics returns a Group result object", {
    result <- eduDemographics(iris, tableVariables = "Sepal.Length")
    expect_s3_class(result, "Group")
})

test_that("eduDemographics showTable=FALSE runs without error", {
    expect_no_error(
        eduDemographics(iris, tableVariables = "Sepal.Length", showTable = FALSE)
    )
})

test_that("eduDemographics showParagraph=FALSE runs without error", {
    expect_no_error(
        eduDemographics(iris,
                        tableVariables     = "Sepal.Length",
                        paragraphVariables = "Sepal.Length",
                        showParagraph = FALSE)
    )
})

# --------------------------------------------------------------------------
# Helper functions
# --------------------------------------------------------------------------

test_that(".dm_paragraph_html wraps text in div and p", {
    html <- jReport:::.dm_paragraph_html("Sample text.")
    expect_match(html, "<div")
    expect_match(html, "<p>")
    expect_match(html, "Sample text\\.")
})

test_that(".dm_paragraph_html HTML-escapes special characters", {
    html <- jReport:::.dm_paragraph_html("Result <5 & ok >0.")
    expect_match(html, "&lt;")
    expect_match(html, "&gt;")
    expect_match(html, "&amp;")
    expect_false(grepl("<5", html, fixed = TRUE))
})

test_that(".dm_omit_note_html returns empty string when nothing omitted", {
    html <- jReport:::.dm_omit_note_html(character(0), character(0))
    expect_equal(html, "")
})

test_that(".dm_omit_note_html mentions omitted variable names", {
    html <- jReport:::.dm_omit_note_html(c("age", "income"), character(0))
    expect_match(html, "age")
    expect_match(html, "income")
    expect_match(html, "<em>Note\\.</em>")
})

test_that(".dm_omit_note_html uses singular grammar for one variable", {
    html <- jReport:::.dm_omit_note_html("age", character(0))
    expect_match(html, "was omitted")
    expect_false(grepl("were", html, fixed = TRUE))
})

test_that(".dm_omit_note_html uses plural grammar for multiple variables", {
    html <- jReport:::.dm_omit_note_html(c("a", "b"), character(0))
    expect_match(html, "were omitted")
})

# --------------------------------------------------------------------------
# mtcars integration
# --------------------------------------------------------------------------

test_that("edu_demographics works on mtcars numeric columns", {
    dm    <- edu_demographics(mtcars, table_variables = c("mpg", "hp", "wt"))
    chars <- vapply(dm$table_rows, `[[`, character(1), "characteristic")
    expect_true("mpg" %in% chars)
    expect_true("hp"  %in% chars)
    expect_true("wt"  %in% chars)
})

test_that("edu_demographics works on mtcars factor columns", {
    d      <- mtcars
    d$cyl  <- factor(d$cyl)
    d$gear <- factor(d$gear)
    dm     <- edu_demographics(d, table_variables = c("mpg", "cyl", "gear"))
    chars  <- vapply(dm$table_rows, `[[`, character(1), "characteristic")
    expect_true("mpg"  %in% chars)
    expect_true("cyl"  %in% chars)
    expect_true("gear" %in% chars)
})

test_that("level rows for mtcars cyl sum to nrow(mtcars)", {
    d     <- mtcars
    d$cyl <- factor(d$cyl)
    dm    <- edu_demographics(d, table_variables = "cyl",
                              stat_n = TRUE, stat_pct = FALSE)
    # header is first row; remaining rows are levels
    level_rows <- dm$table_rows[-1L]
    ns <- vapply(level_rows, function(r) as.integer(r$value), integer(1))
    expect_equal(sum(ns), nrow(mtcars))
})

test_that("single-level factor does not crash", {
    d    <- iris[iris$Species == "setosa", ]
    d$sp <- droplevels(d$Species)
    dm   <- edu_demographics(d, table_variables = "sp")
    expect_equal(length(dm$table_rows), 2L)   # header + 1 level
})
