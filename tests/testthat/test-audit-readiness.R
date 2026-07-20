audit_root <- function() {
    dir <- getwd()
    while (!file.exists(file.path(dir, "DESCRIPTION"))) {
        parent <- dirname(dir)
        if (identical(parent, dir))
            skip("Source package root is not available in this installed-package check context.")
        dir <- parent
    }
    if (!dir.exists(file.path(dir, "jamovi")))
        skip("Source jamovi metadata is not available in this installed-package check context.")
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

    declared_refs <- names(parsed[["00refs.yaml"]][["refs"]])
    analysis_files <- list.files(file.path(root, "jamovi"), pattern = "\\.a\\.yaml$", full.names = TRUE)
    for (file in analysis_files) {
        refs <- parsed[[basename(file)]][["description"]][["references"]]
        expect_true(length(refs) > 0L, info = basename(file))
        expect_true(all(refs %in% declared_refs), info = basename(file))
    }
    result_files <- list.files(file.path(root, "jamovi"), pattern = "^edu.*\\.r\\.yaml$", full.names = TRUE)
    for (file in result_files) {
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

test_that("guided UI variable assignment boxes preserve target mappings", {
    skip_if_not_installed("yaml")

    collect_nodes <- function(node) {
        if (!is.list(node))
            return(list())
        nodes <- list(node)
        for (child_name in c("children", "controls")) {
            children <- node[[child_name]]
            if (is.list(children)) {
                for (child in children)
                    nodes <- c(nodes, collect_nodes(child))
            }
        }
        nodes
    }

    root <- audit_root()
    ui_files <- list.files(file.path(root, "jamovi"), pattern = "^edu.*\\.u\\.yaml$", full.names = TRUE)
    for (ui_file in ui_files) {
        analysis_file <- sub("\\.u\\.yaml$", ".a.yaml", ui_file)
        if (!file.exists(analysis_file))
            next

        ui <- yaml::read_yaml(ui_file)
        analysis <- yaml::read_yaml(analysis_file)
        nodes <- collect_nodes(ui)
        node_types <- vapply(nodes, function(node) node[["type"]] %||% "", character(1))
        target_names <- vapply(nodes[node_types == "VariablesListBox"], function(node) {
            if (!isTRUE(node[["isTarget"]]))
                ""
            else if (is.null(node[["name"]]))
                ""
            else
                as.character(node[["name"]])
        }, character(1))
        target_names <- target_names[nzchar(target_names)]
        variable_options <- vapply(analysis[["options"]], function(option) {
            type <- if (is.null(option[["type"]])) "" else as.character(option[["type"]])
            name <- if (is.null(option[["name"]])) "" else as.character(option[["name"]])
            if (type %in% c("Variable", "Variables"))
                name
            else
                ""
        }, character(1))
        variable_options <- setdiff(variable_options[nzchar(variable_options)], "data")

        expect_true("VariableSupplier" %in% node_types, info = basename(ui_file))
        expect_true(all(variable_options %in% target_names), info = basename(ui_file))
    }
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

test_that("plot state is set unconditionally (no if-showPlot guard)", {
  files <- c(
    "R/eduTTest.b.R", "R/eduAnova.b.R", "R/eduBetweenAnova.b.R",
    "R/eduAncova.b.R", "R/eduCorrelation.b.R", "R/eduMixedAnova.b.R",
    "R/eduRMAnova.b.R", "R/eduRegression.b.R", "R/eduReliabilityOmega.b.R"
  )
  root <- audit_root()
  for (f in files) {
    code <- paste(readLines(file.path(root, f), warn = FALSE), collapse = "\n")
    expect_false(
      grepl("if\\s*\\(\\s*self\\$options\\$showPlot\\s*\\)\\s*self\\$results\\$plot\\$setState", code),
      info = paste("Conditional plot setState found in", f)
    )
    expect_true(
      grepl("self\\$results\\$plot\\$setState\\s*\\(", code),
      info = paste("No unconditional plot setState found in", f)
    )
  }
})

test_that("single-row primary result tables use setRow not addRow", {
  files <- c(
    "R/eduTTest.b.R", "R/eduAnova.b.R", "R/eduCorrelation.b.R",
    "R/eduTTestIndependent.b.R", "R/eduTTestPaired.b.R",
    "R/eduChiSquareGoodness.b.R",
    "R/eduChiSquareIndependence.b.R", "R/eduLogistic.b.R", "R/eduRegression.b.R"
  )
  forbidden <- c("self\\$results\\$main\\$addRow", "self\\$results\\$fit\\$addRow")
  root <- audit_root()
  for (f in files) {
    code <- paste(readLines(file.path(root, f), warn = FALSE), collapse = "\n")
    for (pat in forbidden) {
      expect_false(grepl(pat, code), info = paste("Fixed single-row table uses addRow in", f))
    }
  }
})

test_that("single-row r.yaml tables declare rows: 1", {
  yamls <- c(
    "jamovi/eduTTest.r.yaml", "jamovi/eduAnova.r.yaml",
    "jamovi/eduTTestIndependent.r.yaml", "jamovi/eduTTestPaired.r.yaml",
    "jamovi/eduCorrelation.r.yaml",
    "jamovi/eduChiSquareGoodness.r.yaml", "jamovi/eduChiSquareIndependence.r.yaml",
    "jamovi/eduLogistic.r.yaml", "jamovi/eduRegression.r.yaml"
  )
  root <- audit_root()
  for (f in yamls) {
    content <- paste(readLines(file.path(root, f), warn = FALSE), collapse = "\n")
    expect_true(grepl("rows:\\s*1", content), info = paste("Missing rows: 1 in", f))
  }
})

test_that(".gitignore excludes generated files", {
  root <- audit_root()
  expect_true(file.exists(file.path(root, ".gitignore")))
  gi <- readLines(file.path(root, ".gitignore"), warn = FALSE)
  for (entry in c("build/", "*.jmo", "*.tar.gz", "*.Rcheck/", ".Rproj.user/")) {
    expect_true(any(gi == entry), info = paste("Missing .gitignore entry:", entry))
  }
})

test_that("development artefacts are excluded", {
  root <- audit_root()
  rb <- readLines(file.path(root, ".Rbuildignore"), warn = FALSE)
  expect_true(any(grepl("AUDIT_FIXES_SUMMARY", rb)), info = "AUDIT_FIXES_SUMMARY not in .Rbuildignore")
  expect_true(any(grepl("audit-fixes\\.patch", rb)), info = "audit-fixes.patch not in .Rbuildignore")
  expect_false(file.exists(file.path(root, "AUDIT_FIXES_SUMMARY.md")), info = "AUDIT_FIXES_SUMMARY.md still exists")
  expect_false(file.exists(file.path(root, "audit-fixes.patch")), info = "audit-fixes.patch still exists")
})

test_that("stale PDF exclusion is not in .Rbuildignore", {
  root <- audit_root()
  rb <- readLines(file.path(root, ".Rbuildignore"), warn = FALSE)
  expect_false(any(grepl("Julie Pallant", rb)), info = "Stale Julie Pallant entry remains in .Rbuildignore")
})

test_that("core helper functions are available after refactor", {
  fns <- c(
    ".jr_html_card", ".jr_html_escape", ".jr_html_paragraphs", ".jr_html_bullets",
    ".jr_addon_insert_card", ".jr_addon_set_card", ".jr_addon_set_tables",
    ".jr_reference_entry_text", ".jr_text_reference_keys", ".jr_addon_inject_refs",
    ".jr_populate_diagnostics", ".jr_prefill_diagnostic_rows",
    ".jr_guided_error_message", ".jr_guided_computation"
  )
  for (fn in fns) {
    expect_true(exists(fn, envir = asNamespace("jReport"), mode = "function"),
                info = paste("Missing helper:", fn))
  }
})

test_that("guided methods reference panel renders formatted references", {
  html <- .jr_methods_references_html(keys = c("jReport", "Cohen1988", "Cumming2014"))
  expect_true(grepl("jReport: Automated Statistical Reporting for R and jamovi", html, fixed = TRUE))
  expect_true(grepl("Statistical power analysis for the behavioral sciences", html, fixed = TRUE))
  expect_true(grepl("The new statistics: Why and how", html, fixed = TRUE))
  expect_false(grepl("No additional references", html, fixed = TRUE))
})

test_that("analysis .b.R files are represented in manifest", {
  root <- audit_root()
  src <- sub("\\.b\\.R$", "", list.files(file.path(root, "R"), pattern = "\\.b\\.R$"))
  manifest <- paste(readLines(file.path(root, "jamovi", "0000.yaml"), warn = FALSE), collapse = "\n")
  # jrReport* add-ons use addonFor rather than name entries; edu* guided analyses
  # are intentionally hidden from the menu (see 0000.yaml) and are excluded here.
  addon_only <- src[grepl("^jrReport", src)]
  missing <- addon_only[!vapply(addon_only, function(x) grepl(x, manifest, fixed = TRUE), logical(1))]
  expect_equal(missing, character(0),
               info = paste("Missing from manifest:", paste(missing, collapse = ", ")))
})
