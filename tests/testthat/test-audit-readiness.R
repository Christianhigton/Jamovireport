audit_root <- function() {
    dir <- getwd()
    while (!file.exists(file.path(dir, "DESCRIPTION"))) {
        parent <- dirname(dir)
        if (identical(parent, dir))
            stop("Could not locate package root.")
        dir <- parent
    }
    dir
}

test_that("package and jamovi metadata use the release name and version", {
    root <- audit_root()
    description <- read.dcf(file.path(root, "DESCRIPTION"))[1, ]
    manifest <- readLines(file.path(root, "jamovi", "0000.yaml"), warn = FALSE)
    analysis_files <- list.files(file.path(root, "jamovi"), pattern = "\\.a\\.yaml$", full.names = TRUE)
    analysis_text <- unlist(lapply(analysis_files, readLines, warn = FALSE))

    expect_equal(unname(description[["Package"]]), "jReport")
    expect_equal(unname(description[["Version"]]), "1.0.0")
    expect_true(any(grepl("^name: jReport$", manifest)))
    expect_true(any(grepl("^version: 1\\.0\\.0$", manifest)))
    expect_false(any(grepl("0\\.1\\.0|0\\.1\\.0\\.9013|0\\.1\\.0\\.9015", c(manifest, analysis_text))))
    expect_equal(
        length(grep("^version: '1\\.0\\.0'$", analysis_text)),
        length(analysis_files)
    )
})

test_that("active package files no longer refer to the old module name", {
    root <- audit_root()
    active_files <- list.files(
        root,
        pattern = "\\.(R|Rd|md|yaml|yml)$|^DESCRIPTION$|^NAMESPACE$",
        recursive = TRUE,
        full.names = TRUE,
        all.files = FALSE
    )
    active_relative <- sub(paste0("^", gsub("\\\\", "/", root), "/?"), "", gsub("\\\\", "/", active_files))
    active_files <- active_files[!grepl("^(\\.git|notes)/", active_relative)]
    text <- unlist(lapply(active_files, readLines, warn = FALSE))

    old_names <- c(
        paste0("Jamovi", "Report"),
        paste("jamovi", "Report"),
        paste("Jamovi", "Report")
    )
    expect_false(any(Reduce(`|`, lapply(old_names, grepl, x = text, fixed = TRUE))))
})

test_that("package code does not mutate global library paths", {
    root <- audit_root()
    package_code <- unlist(lapply(
        list.files(file.path(root, "R"), pattern = "\\.R$", full.names = TRUE),
        readLines,
        warn = FALSE
    ))

    expect_false(any(grepl("\\.libPaths\\(", package_code)))
})

test_that("automatic report dependency preload avoids library path mutation", {
    before <- .libPaths()
    expect_true(isTRUE(.jr_addon_enable_library()))
    expect_identical(.libPaths(), before)
    expect_true(requireNamespace("effectsize", quietly = TRUE))
})

test_that("jamovi YAML files parse and analysis references are declared", {
    skip_if_not_installed("yaml")

    root <- audit_root()
    yaml_files <- list.files(file.path(root, "jamovi"), pattern = "\\.yaml$", full.names = TRUE)
    parsed <- lapply(yaml_files, yaml::read_yaml)
    names(parsed) <- basename(yaml_files)

    declared_refs <- names(parsed[["00refs.yaml"]])
    analysis_files <- list.files(file.path(root, "jamovi"), pattern = "\\.a\\.yaml$", full.names = TRUE)
    for (file in analysis_files) {
        refs <- parsed[[basename(file)]][["description"]][["references"]]
        expect_true(length(refs) > 0L, info = basename(file))
        expect_true(all(refs %in% declared_refs), info = basename(file))
    }
})

test_that("guided UI reporting sections are collapsed and labels are polished", {
    root <- audit_root()
    ui_files <- list.files(file.path(root, "jamovi"), pattern = "^edu.*\\.u\\.yaml$", full.names = TRUE)
    for (file in ui_files) {
        text <- readLines(file, warn = FALSE)
        reporting <- grep("label: Reporting", text)
        expect_true(length(reporting) == 1L, info = basename(file))
        expect_true(any(grepl("collapsed: true", text[seq(reporting, min(reporting + 3L, length(text)))])),
                    info = basename(file))
    }

    analysis_text <- unlist(lapply(
        list.files(file.path(root, "jamovi"), pattern = "^edu.*\\.a\\.yaml$", full.names = TRUE),
        readLines,
        warn = FALSE
    ))
    expect_false(any(grepl("title: (Include|Display|Use|Show) ", analysis_text)))
    expect_false(any(grepl("title: [a-z]", analysis_text)))
})

test_that("guided computational result elements declare clearWith", {
    skip_if_not_installed("yaml")

    root <- audit_root()
    result_files <- list.files(file.path(root, "jamovi"), pattern = "^edu.*\\.r\\.yaml$", full.names = TRUE)
    for (file in result_files) {
        parsed <- yaml::read_yaml(file)
        computational <- Filter(function(item) {
            identical(item[["type"]], "Table") || identical(item[["type"]], "Image")
        }, parsed[["items"]])
        missing_clear_with <- vapply(computational, function(item) {
            length(item[["clearWith"]]) == 0L
        }, logical(1))

        expect_false(
            any(missing_clear_with),
            info = paste(basename(file), paste(vapply(computational[missing_clear_with], `[[`, character(1), "name"), collapse = ", "))
        )
    }
})

test_that("fixed diagnostics use stable row updates and dynamic diagnostics remain append-based", {
    root <- audit_root()
    fixed_backends <- c(
        "eduAnova.b.R",
        "eduBetweenAnova.b.R",
        "eduAncova.b.R",
        "eduCorrelation.b.R",
        "eduChiSquareIndependence.b.R",
        "eduChiSquareGoodness.b.R",
        "eduRMAnova.b.R",
        "eduMixedAnova.b.R",
        "eduReliabilityOmega.b.R"
    )
    dynamic_backends <- c(
        "eduTTest.b.R",
        "eduRegression.b.R",
        "eduLogistic.b.R",
        "eduMancova.b.R"
    )

    for (file in fixed_backends) {
        text <- paste(readLines(file.path(root, "R", file), warn = FALSE), collapse = "\n")
        expect_true(grepl("\\.jr_populate_diagnostics\\(self\\$results\\$diagnostics, result\\$diagnostics, fixed = TRUE\\)", text),
                    info = file)
    }

    for (file in dynamic_backends) {
        text <- paste(readLines(file.path(root, "R", file), warn = FALSE), collapse = "\n")
        expect_true(grepl("\\.jr_populate_diagnostics\\(self\\$results\\$diagnostics, result\\$diagnostics\\)", text),
                    info = file)
        expect_false(grepl("fixed = TRUE", text, fixed = TRUE), info = file)
    }
})
