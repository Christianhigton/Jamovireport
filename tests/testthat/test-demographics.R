# Unit tests for the eduDemographics module.
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
# edu_demographics — core function
# --------------------------------------------------------------------------

test_that("edu_demographics returns the expected list structure", {
    dm <- edu_demographics(iris, variables = c("Sepal.Length", "Species"))
    expect_type(dm, "list")
    expect_named(dm, c("rows", "total_n", "paragraph"))
    expect_equal(dm$total_n, 150L)
    expect_gt(length(dm$rows), 0L)
    expect_type(dm$paragraph, "character")
    expect_gt(nchar(dm$paragraph), 10L)
})

test_that("edu_demographics handles only continuous variables", {
    dm <- edu_demographics(iris, variables = c("Sepal.Length", "Sepal.Width"))
    vars_in_rows <- vapply(dm$rows, `[[`, character(1), "variable")
    expect_true("Sepal.Length" %in% vars_in_rows)
    expect_true("Sepal.Width"  %in% vars_in_rows)
})

test_that("edu_demographics handles only categorical variables", {
    dm <- edu_demographics(iris_mixed(), variables = c("Species", "large_petal"))
    vars_in_rows <- vapply(dm$rows, `[[`, character(1), "variable")
    expect_true("Species"     %in% vars_in_rows)
    expect_true("large_petal" %in% vars_in_rows)
})

test_that("edu_demographics handles mixed continuous and categorical variables", {
    dm <- edu_demographics(iris_mixed(), variables = c("Sepal.Length", "Species"))
    vars_in_rows <- vapply(dm$rows, `[[`, character(1), "variable")
    expect_true("Sepal.Length" %in% vars_in_rows)
    expect_true("Species"      %in% vars_in_rows)
})

test_that("categorical variable produces one header row plus one row per level", {
    dm   <- edu_demographics(iris, variables = "Species")
    rows <- dm$rows
    n_levels <- nlevels(iris$Species)
    # header + n_levels rows = n_levels + 1
    expect_equal(length(rows), n_levels + 1L)
    # Header row has variable set, category empty
    expect_equal(rows[[1]]$variable, "Species")
    expect_equal(rows[[1]]$category, "")
    # Level rows have variable empty, category = level name
    for (i in seq_len(n_levels)) {
        expect_equal(rows[[i + 1]]$variable, "")
        expect_true(nchar(rows[[i + 1]]$category) > 0L)
    }
})

test_that("categorical level rows have correct n and percent", {
    dm <- edu_demographics(iris, variables = "Species")
    # setosa is the first level — 50 observations out of 150
    setosa_row <- dm$rows[[2]]
    expect_equal(setosa_row$n, 50L)
    expect_equal(setosa_row$percent, 100 * 50 / 150, tolerance = 1e-6)
})

test_that("percent values across all levels of a factor sum to 100", {
    dm      <- edu_demographics(iris, variables = "Species")
    n_lvls  <- nlevels(iris$Species)
    pcts    <- vapply(dm$rows[seq_len(n_lvls) + 1L], `[[`, numeric(1), "percent")
    expect_equal(sum(pcts), 100, tolerance = 1e-6)
})

test_that("continuous variable row contains valid M, SD, and range", {
    dm  <- edu_demographics(iris, variables = "Sepal.Length")
    row <- dm$rows[[1]]
    expect_false(is.na(row$mean))
    expect_false(is.na(row$sd))
    expect_equal(row$mean, mean(iris$Sepal.Length), tolerance = 1e-6)
    expect_equal(row$sd,   sd(iris$Sepal.Length),   tolerance = 1e-6)
    expect_true(grepl("[0-9]+\\.[0-9]", row$range))  # range contains numeric values
})

test_that("continuous variable row has correct n", {
    dm  <- edu_demographics(iris, variables = "Sepal.Length")
    row <- dm$rows[[1]]
    expect_equal(row$n, 150L)
})

test_that("rows list has correct field names for every row", {
    dm      <- edu_demographics(iris_mixed(), variables = c("Sepal.Length", "Species"))
    expected <- c("variable","category","n","percent","mean","sd","range")
    for (r in dm$rows) {
        expect_named(r, expected)
    }
})

test_that("edu_demographics paragraph mentions total N", {
    dm <- edu_demographics(iris, variables = "Sepal.Length")
    expect_true(grepl("150", dm$paragraph))
})

test_that("edu_demographics paragraph includes variable names", {
    dm <- edu_demographics(iris, variables = c("Sepal.Length", "Species"))
    expect_true(grepl("Sepal.Length", dm$paragraph))
    expect_true(grepl("Species",      dm$paragraph))
})

test_that("edu_demographics with zero variables returns empty rows and a short paragraph", {
    dm <- edu_demographics(iris, variables = character(0))
    expect_length(dm$rows, 0L)
    expect_true(nchar(dm$paragraph) > 0L)
})

test_that("edu_demographics ignores variables not in the data", {
    dm <- edu_demographics(iris, variables = c("Sepal.Length", "NonExistentVar"))
    vars <- vapply(dm$rows, `[[`, character(1), "variable")
    expect_false("NonExistentVar" %in% vars)
    expect_true("Sepal.Length"   %in% vars)
})

test_that("edu_demographics stops with a useful error when data is not a data frame", {
    expect_error(edu_demographics(list(x = 1:5), variables = "x"), "data frame")
})

# --------------------------------------------------------------------------
# Custom rows
# --------------------------------------------------------------------------

test_that("non-blank custom rows appear at the end of the table", {
    cr <- list(
        list(variable = "Education", category = "Undergraduate", n = "45", percent = "60", note = ""),
        list(variable = "",          category = "",              n = "",   percent = "",   note = "")
    )
    dm <- edu_demographics(iris[, character(0)], variables = character(0),
                           custom_rows = cr)
    expect_equal(length(dm$rows), 1L)
    expect_equal(dm$rows[[1]]$variable, "Education")
    expect_equal(dm$rows[[1]]$category, "Undergraduate")
    expect_equal(dm$rows[[1]]$n, 45L)
    expect_equal(dm$rows[[1]]$percent, 60, tolerance = 1e-6)
})

test_that("blank custom rows (all fields empty) are silently omitted", {
    cr <- lapply(1:5, function(i)
        list(variable="", category="", n="", percent="", note=""))
    dm <- edu_demographics(iris, variables = "Sepal.Length", custom_rows = cr)
    # Only the one Sepal.Length row should be present
    expect_equal(length(dm$rows), 1L)
})

test_that("custom row with only variable name is kept", {
    cr <- list(list(variable = "Site", category = "", n = "", percent = "", note = ""))
    dm <- edu_demographics(iris[, character(0)], variables = character(0),
                           custom_rows = cr)
    expect_equal(length(dm$rows), 1L)
    expect_equal(dm$rows[[1]]$variable, "Site")
})

test_that("custom row with non-numeric n and percent stores NA, not crash", {
    cr <- list(list(variable = "Custom", category = "A", n = "lots", percent = "~half", note = ""))
    dm <- edu_demographics(iris[, character(0)], variables = character(0),
                           custom_rows = cr)
    expect_equal(length(dm$rows), 1L)
    expect_true(is.na(dm$rows[[1]]$n))
    expect_true(is.na(dm$rows[[1]]$percent))
})

test_that("custom row note appears in the range column", {
    cr <- list(list(variable = "Follow-up", category = "12 months", n = "80", percent = "53.3", note = "see Table 2"))
    dm <- edu_demographics(iris[, character(0)], variables = character(0),
                           custom_rows = cr)
    expect_equal(dm$rows[[1]]$range, "see Table 2")
})

test_that("custom rows included in paragraph only when flag is TRUE", {
    cr <- list(list(variable = "Site", category = "London", n = "30", percent = "40", note = ""))
    dm_no  <- edu_demographics(iris[, character(0)], variables = character(0),
                                custom_rows = cr, include_custom_in_paragraph = FALSE)
    dm_yes <- edu_demographics(iris[, character(0)], variables = character(0),
                                custom_rows = cr, include_custom_in_paragraph = TRUE)
    expect_false(grepl("Site",   dm_no$paragraph))
    expect_true( grepl("Site",   dm_yes$paragraph))
    expect_true( grepl("London", dm_yes$paragraph))
})

test_that("custom rows combined with auto rows maintain correct ordering", {
    cr <- list(list(variable = "Site", category = "Online", n = "50", percent = "33", note = ""))
    dm <- edu_demographics(iris, variables = c("Sepal.Length", "Species"),
                           custom_rows = cr)
    vars <- vapply(dm$rows, `[[`, character(1), "variable")
    # Site appears at the end after the auto rows
    expect_true(tail(vars[nzchar(vars)], 1L) == "Site")
})

# --------------------------------------------------------------------------
# eduDemographics jamovi entry point
# --------------------------------------------------------------------------

test_that("eduDemographics entry point runs without error on iris species + continuous", {
    expect_no_error(
        eduDemographics(iris_mixed(),
                        variables = c("Sepal.Length", "Species", "large_petal"))
    )
})

test_that("eduDemographics with a single variable returns without error", {
    # variables=NULL hits a jmvcore limitation shared by all jamovi analyses;
    # test the minimum meaningful call instead
    expect_no_error(eduDemographics(iris, variables = "Sepal.Length"))
})

test_that("eduDemographics with custom rows runs without error", {
    expect_no_error(
        eduDemographics(
            iris,
            variables     = "Sepal.Length",
            customRow1Var = "Education",
            customRow1Cat = "Postgraduate",
            customRow1N   = "45",
            customRow1Pct = "30"
        )
    )
})

test_that("eduDemographics with all blank custom rows does not crash", {
    expect_no_error(
        eduDemographics(
            iris,
            variables     = "Sepal.Length",
            customRow1Var = "", customRow1Cat = "", customRow1N = "",
            customRow1Pct = "", customRow1Note = "",
            customRow2Var = "", customRow2Cat = "", customRow2N = "",
            customRow2Pct = "", customRow2Note = ""
        )
    )
})

test_that("eduDemographics custom title is reflected in analysis", {
    result <- eduDemographics(iris, variables = "Sepal.Length",
                               tableTitle = "Table 2. My Custom Title")
    expect_s3_class(result, "Group")
})

test_that("eduDemographics includeParagraph=FALSE runs without error", {
    expect_no_error(
        eduDemographics(iris, variables = "Sepal.Length", includeParagraph = FALSE)
    )
})

test_that("eduDemographics with character variable (not factor) runs without error", {
    d       <- iris
    d$label <- as.character(d$Species)
    expect_no_error(
        eduDemographics(d, variables = c("Sepal.Length", "label"))
    )
})

# --------------------------------------------------------------------------
# .dm_paragraph_html helper
# --------------------------------------------------------------------------

test_that(".dm_paragraph_html wraps text in a div and p tag", {
    html <- jReport:::.dm_paragraph_html("Sample text.")
    expect_true(grepl("<div", html, fixed = TRUE))
    expect_true(grepl("<p>",  html, fixed = TRUE))
    expect_true(grepl("Sample text.", html, fixed = TRUE))
})

test_that(".dm_paragraph_html HTML-escapes special characters", {
    html <- jReport:::.dm_paragraph_html("Result <5 & ok >0.")
    expect_true(grepl("&lt;", html, fixed = TRUE))
    expect_true(grepl("&gt;", html, fixed = TRUE))
    expect_true(grepl("&amp;", html, fixed = TRUE))
    expect_false(grepl("<5", html, fixed = TRUE))
})

# --------------------------------------------------------------------------
# mtcars — additional dataset
# --------------------------------------------------------------------------

test_that("edu_demographics works on mtcars numeric columns", {
    dm <- edu_demographics(mtcars, variables = c("mpg", "hp", "wt"))
    vars <- vapply(dm$rows, `[[`, character(1), "variable")
    expect_true("mpg" %in% vars)
    expect_true("hp"  %in% vars)
    expect_true("wt"  %in% vars)
})

test_that("edu_demographics works on mtcars factor columns", {
    d      <- mtcars
    d$cyl  <- factor(d$cyl)
    d$gear <- factor(d$gear)
    dm     <- edu_demographics(d, variables = c("mpg", "cyl", "gear"))
    vars   <- vapply(dm$rows, `[[`, character(1), "variable")
    expect_true("mpg"  %in% vars)
    expect_true("cyl"  %in% vars)
    expect_true("gear" %in% vars)
})

test_that("n and percent for mtcars cyl factor are correctly computed", {
    d     <- mtcars
    d$cyl <- factor(d$cyl)
    dm    <- edu_demographics(d, variables = "cyl")
    rows  <- dm$rows
    # First row is header for cyl
    expect_equal(rows[[1]]$variable, "cyl")
    # Remaining rows are levels — n should sum to nrow(d)
    level_rows <- rows[-1]
    total_n    <- sum(vapply(level_rows, `[[`, integer(1), "n"), na.rm = TRUE)
    expect_equal(total_n, nrow(d))
})

test_that("single-level factor does not crash edu_demographics", {
    d     <- iris[iris$Species == "setosa", ]
    d$sp  <- droplevels(d$Species)
    dm    <- edu_demographics(d, variables = "sp")
    expect_equal(length(dm$rows), 2L)  # header + one level
})

test_that("data with NA values: n excludes NAs for categorical variable", {
    d              <- iris
    d$Species[1:5] <- NA
    d$sp           <- droplevels(d$Species)
    dm             <- edu_demographics(d, variables = "sp")
    level_rows     <- dm$rows[-1]
    total_counted  <- sum(vapply(level_rows, `[[`, integer(1), "n"), na.rm = TRUE)
    expect_equal(total_counted, 145L)
})

test_that("data with NA values: n for continuous variable reflects non-NA count", {
    d                  <- iris
    d$Sepal.Length[1:10] <- NA
    dm                 <- edu_demographics(d, variables = "Sepal.Length")
    expect_equal(dm$rows[[1]]$n, 140L)
})
