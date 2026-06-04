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

test_that("jamovi YAML files parse and analysis refs are declared", {
    skip_if_not_installed("yaml")

    root <- audit_root()
    yaml_files <- list.files(file.path(root, "jamovi"), pattern = "\\.yaml$", full.names = TRUE)
    parsed <- lapply(yaml_files, yaml::read_yaml)
    names(parsed) <- basename(yaml_files)

    declared_refs <- names(parsed[["00refs.yaml"]])
    analysis_files <- list.files(file.path(root, "jamovi"), pattern = "\\.a\\.yaml$", full.names = TRUE)
    for (file in analysis_files) {
        refs <- parsed[[basename(file)]][["refs"]]
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
