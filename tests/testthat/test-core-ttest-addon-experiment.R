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
