guided_failure_root <- function() {
    dir <- getwd()
    while (!file.exists(file.path(dir, "DESCRIPTION"))) {
        parent <- dirname(dir)
        if (identical(parent, dir))
            skip("Source package root is not available in this installed-package check context.")
        dir <- parent
    }
    if (!dir.exists(file.path(dir, "R")))
        skip("Source R files are not available in this installed-package check context.")
    dir
}

test_that("guided computation wrapper shows a friendly generic failure", {
    skip_if_not_installed("jmvcore")

    d <- data.frame(
        x = c(1, NA),
        y = c(NA, 2)
    )

    expect_error(
        .jr_guided_computation(edu_correlation(d, "x", "y")),
        "enough complete cases",
        fixed = TRUE
    )
})

test_that("guided logistic failures show a binary-outcome message", {
    skip_if_not_installed("jmvcore")

    d <- data.frame(
        outcome = factor(c("low", "medium", "high", "low")),
        predictor = c(1, 2, 3, 4)
    )

    expect_error(
        .jr_guided_computation(edu_logistic_regression(d, outcome ~ predictor)),
        "Logistic regression requires a binary outcome variable.",
        fixed = TRUE
    )
})

test_that("guided model fitting failures do not expose raw R errors", {
    skip_if_not_installed("jmvcore")

    d <- data.frame(
        y = c(1, 2),
        x = c(1, 2)
    )

    error_message <- tryCatch(
        suppressMessages(suppressWarnings(.jr_guided_computation(edu_lm(d, y ~ x)))),
        error = conditionMessage
    )
    expect_match(error_message, "The analysis could not be completed.", fixed = TRUE)
    expect_false(grepl("missing value where TRUE/FALSE needed", error_message, fixed = TRUE))
})

test_that("guided jamovi backends route computations through the safe helper", {
    root <- guided_failure_root()
    backend_files <- list.files(file.path(root, "R"), pattern = "^edu.*\\.b\\.R$", full.names = TRUE)
    backend_text <- vapply(backend_files, function(file) paste(readLines(file, warn = FALSE), collapse = "\n"), character(1))

    expect_true(length(backend_files) > 0L)
    expect_true(all(grepl("\\.jr_guided_computation\\(", backend_text)), info = paste(basename(backend_files), collapse = ", "))
    expect_false(any(grepl("\\bresult <- edu_", backend_text)), info = paste(basename(backend_files), collapse = ", "))
})

test_that("fixed diagnostics update stable rows without duplication", {
    skip_if_not_installed("jmvcore")

    table <- jmvcore::Table$new(
        name = "diagnostics",
        title = "Diagnostics",
        columns = list(
            list(name = "check", title = "Check", type = "text"),
            list(name = "tested", title = "Tested?", type = "text"),
            list(name = "statistic", title = "Statistic", type = "number"),
            list(name = "p", title = "p", type = "number"),
            list(name = "status", title = "Status", type = "text"),
            list(name = "interpretation", title = "Interpretation", type = "text"),
            list(name = "action", title = "Recommended Action", type = "text")
        )
    )
    diagnostics <- data.frame(
        check = c("First check", "Second check"),
        statistic = c(1, 2),
        p = c(.1, .2),
        status = c("Acceptable", "Caution"),
        interpretation = c("First interpretation", "Second interpretation"),
        action = c("First action", "Second action"),
        stringsAsFactors = FALSE
    )

    .jr_populate_diagnostics(table, diagnostics, fixed = TRUE)
    .jr_populate_diagnostics(table, transform(diagnostics, status = c("Caution", "Acceptable")), fixed = TRUE)

    table_df <- table$asDF
    expect_equal(table$rowCount, 2L)
    expect_equal(unlist(table$rowKeys), c(1L, 2L))
    expect_equal(table_df$status, c("Caution", "Acceptable"))
    expect_length(table$notes, 2L)
    expect_match(table$notes[["guidance-1"]]$note, "First interpretation", fixed = TRUE)
    expect_match(table$notes[["guidance-2"]]$note, "Second action", fixed = TRUE)
})
