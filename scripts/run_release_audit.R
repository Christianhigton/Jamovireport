#!/usr/bin/env Rscript

args <- commandArgs(trailingOnly = TRUE)
root <- if (length(args)) normalizePath(args[[1]]) else normalizePath(".")
setwd(root)

cat("jReport release behaviour audit\n")
cat("Repository:", root, "\n\n")

analysis_files <- list.files("jamovi", pattern = "\\.a\\.yaml$", full.names = TRUE)
test_files <- list.files("tests/testthat", pattern = "^test-.*\\.R$", full.names = TRUE)
cat("Analysis definitions:", length(analysis_files), "\n")
cat("Automated test files:", length(test_files), "\n\n")

required <- c(
    "tests/testthat/test-release-behaviour-audit.R",
    "qa/manual-test-matrix.csv",
    "qa/bug-report-template.md"
)
missing <- required[!file.exists(required)]
if (length(missing))
    stop("Missing release-audit files: ", paste(missing, collapse = ", "))

devtools::test(".", reporter = "summary", stop_on_failure = TRUE)

cat("\nAutomated audit passed.\n")
cat("Desktop checks remain in qa/manual-test-matrix.csv.\n")
