core_ttest_source_root <- function() {
    root <- getwd()
    while (!file.exists(file.path(root, "DESCRIPTION"))) {
        parent <- dirname(root)
        if (identical(parent, root))
            skip("Source package root is not available in this test context.")
        root <- parent
    }
    root
}

test_that("core t-test proof-of-concept remains a hidden add-on", {
    root <- core_ttest_source_root()
    module <- yaml::read_yaml(file.path(root, "jamovi", "0000.yaml"))
    analyses <- module$analyses
    index <- which(vapply(
        analyses,
        function(analysis) identical(analysis$name, "jrReportTTestIS"),
        logical(1)
    ))

    expect_length(index, 1L)
    registration <- analyses[[index]]
    expect_equal(registration$addonFor, "jmv::ttestIS")
    expect_true(registration$hidden)
})

test_that("core t-test proof-of-concept has an opt-in master control", {
    root <- core_ttest_source_root()
    analysis <- yaml::read_yaml(file.path(root, "jamovi", "jrReportTTestIS.a.yaml"))
    option_names <- vapply(analysis$options, `[[`, character(1), "name")
    enabled <- analysis$options[[match("jreportEnabled", option_names)]]

    expect_equal(enabled$title, "Generate jReport output")
    expect_equal(enabled$type, "Bool")
    expect_false(enabled$default)
})

test_that("core t-test proof-of-concept UI is collapsed by default", {
    root <- core_ttest_source_root()
    ui <- yaml::read_yaml(file.path(root, "jamovi", "jrReportTTestIS.u.yaml"))
    section <- ui$children[[1]]

    expect_equal(section$type, "CollapseBox")
    expect_equal(section$label, "jReport: Reporting and explanation")
    expect_true(section$collapsed)
})

test_that("core t-test reporting configuration has stable values and defaults", {
    root <- core_ttest_source_root()
    analysis <- yaml::read_yaml(file.path(root, "jamovi", "jrReportTTestIS.a.yaml"))
    options <- stats::setNames(analysis$options, vapply(
        analysis$options, `[[`, character(1), "name"
    ))

    expect_equal(
        vapply(options$reportStyle$options, `[[`, character(1), "name"),
        c("apaConcise", "apaDetailed", "plainLanguage", "teaching")
    )
    expect_equal(options$reportStyle$default, "apaConcise")
    expect_equal(
        vapply(options$explanationTone$options, `[[`, character(1), "name"),
        c("academic", "professional", "studentFriendly", "plainEnglish")
    )
    expect_equal(options$explanationTone$default, "professional")

    section_options <- c(
        "showSuggestedWording", "showInterpretation", "showEffectSizeGuidance",
        "showAssumptionGuidance", "showPracticalMeaning",
        "showCheckBeforeReporting", "showReferences"
    )
    expect_true(all(vapply(options[section_options], `[[`, logical(1), "default")))
})

test_that("all reporting configuration controls are gated by the master option", {
    root <- core_ttest_source_root()
    ui_text <- paste(
        readLines(file.path(root, "jamovi", "jrReportTTestIS.u.yaml"), warn = FALSE),
        collapse = "\n"
    )
    gated <- c(
        "reportStyle", "explanationTone", "showSuggestedWording",
        "showInterpretation", "showEffectSizeGuidance", "showAssumptionGuidance",
        "showPracticalMeaning", "showCheckBeforeReporting", "showReferences",
        "pAdjustment"
    )
    for (name in gated) {
        expect_match(
            ui_text,
            paste0("name: ", name, ", enable: \\(jreportEnabled\\)")
        )
    }
})

test_that("generated add-on options retain reporting selections in an instance", {
    options <- jrReportTTestISOptions$new(
        jreportEnabled = TRUE,
        reportStyle = "teaching",
        explanationTone = "studentFriendly",
        showSuggestedWording = FALSE,
        showInterpretation = TRUE,
        showEffectSizeGuidance = FALSE,
        showAssumptionGuidance = TRUE,
        showPracticalMeaning = FALSE,
        showCheckBeforeReporting = TRUE,
        showReferences = FALSE,
        pAdjustment = "bonferroni"
    )

    expect_true(options$jreportEnabled)
    expect_equal(options$reportStyle, "teaching")
    expect_equal(options$explanationTone, "studentFriendly")
    expect_false(options$showSuggestedWording)
    expect_true(options$showInterpretation)
    expect_false(options$showEffectSizeGuidance)
    expect_true(options$showAssumptionGuidance)
    expect_false(options$showPracticalMeaning)
    expect_true(options$showCheckBeforeReporting)
    expect_false(options$showReferences)
    expect_equal(options$pAdjustment, "bonferroni")
})
