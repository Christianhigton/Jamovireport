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

core_ttest_load_jmv <- function() {
    app_library <- "/Applications/jamovi.app/Contents/Resources/modules/jmv/R"
    if (!dir.exists(file.path(app_library, "jmv")))
        skip("The target jamovi jmv module is not installed.")
    if (!app_library %in% .libPaths())
        .libPaths(c(app_library, .libPaths()))
    if (!requireNamespace("jmv", quietly = TRUE))
        skip("The target jamovi jmv module could not be loaded.")
}

core_ttest_host_results <- function(result) {
    list(
        ttest = result$ttest,
        desc = result$desc,
        assum = list(norm = result$assum$norm, eqv = result$assum$eqv)
    )
}

test_that("core t-test adapter reads a stable list fixture", {
    fixture <- list(
        ttest = data.frame(
            `var[stud]` = "score", `stat[stud]` = -2.5,
            `df[stud]` = 18, `p[stud]` = .022,
            `md[stud]` = -3, `cil[stud]` = -5.5, `ciu[stud]` = -.5,
            `esType[stud]` = "Cohen's d", `es[stud]` = -.8,
            check.names = FALSE
        ),
        desc = data.frame(
            dep = "score", `group[1]` = "A", `num[1]` = 10,
            `mean[1]` = 7, `sd[1]` = 2, `group[2]` = "B",
            `num[2]` = 10, `mean[2]` = 10, `sd[2]` = 3,
            check.names = FALSE
        ),
        assum = list(eqv = data.frame(
            name = "score", f = 1.2, df = 1, df2 = 18, p = .29
        ))
    )
    adapted <- .jr_core_ttest_adapter(
        list(vars = "score", group = "condition", students = TRUE, welchs = FALSE),
        fixture, allow_fallback = FALSE
    )

    expect_s3_class(adapted, "jr_core_ttest_adapter")
    expect_length(adapted$analyses, 1L)
    result <- adapted$analyses[[1]]
    expect_equal(result$testType, "student")
    expect_equal(result$statistics$t, -2.5)
    expect_equal(result$statistics$df, 18)
    expect_equal(result$statistics$p, .022)
    expect_equal(result$descriptives$group, c("A", "B"))
    expect_equal(result$assumptions$levene$statistic, 1.2)
    expect_identical(result$source$primary, "host-results")
    expect_false(result$source$repeatedCalculation)
})

test_that("core t-test adapter fails gracefully when host structures are missing", {
    adapted <- expect_silent(.jr_core_ttest_adapter(
        list(vars = "score", group = "condition", students = TRUE),
        list(), allow_fallback = FALSE
    ))
    expect_length(adapted$analyses, 1L)
    expect_true(any(grepl("not found", adapted$warnings, fixed = TRUE)))
    expect_true(is.na(adapted$analyses[[1]]$statistics$t))
    expect_identical(adapted$analyses[[1]]$source$primary, "unavailable")

    no_variables <- expect_silent(.jr_core_ttest_adapter(list(), list()))
    expect_length(no_variables$analyses, 0L)
})

test_that("adapter matches live core Student and Welch results", {
    core_ttest_load_jmv()
    data <- ToothGrowth
    data$len2 <- data$len * .5 + rep(c(-1, 1), length.out = nrow(data))
    data$len[c(2, 17)] <- NA_real_
    host <- jmv::ttestIS(
        data = data, vars = c("len", "len2"), group = "supp",
        students = TRUE, welchs = TRUE, meanDiff = TRUE, ci = TRUE,
        effectSize = TRUE, ciES = TRUE, desc = TRUE, norm = TRUE, eqv = TRUE,
        miss = "perAnalysis"
    )
    options <- list(
        vars = c("len", "len2"), group = "supp", students = TRUE,
        welchs = TRUE, ciWidth = 95, ciWidthES = 95,
        hypothesis = "different", miss = "perAnalysis"
    )
    adapted <- .jr_core_ttest_adapter(options, core_ttest_host_results(host), data)
    core <- host$ttest$asDF

    expect_length(adapted$analyses, 4L)
    for (result in adapted$analyses) {
        suffix <- if (result$testType == "student") "stud" else "welc"
        index <- which(core[[paste0("var[", suffix, "]")]] == result$outcome)
        expect_length(index, 1L)
        expect_equal(result$statistics$t, core[[paste0("stat[", suffix, "]")]][index], tolerance = 1e-10)
        expect_equal(result$statistics$df, core[[paste0("df[", suffix, "]")]][index], tolerance = 1e-10)
        expect_equal(result$statistics$p, core[[paste0("p[", suffix, "]")]][index], tolerance = 1e-10)
        expect_equal(result$statistics$meanDifference, core[[paste0("md[", suffix, "]")]][index], tolerance = 1e-10)
        expect_equal(result$confidenceInterval$lower, core[[paste0("cil[", suffix, "]")]][index], tolerance = 1e-10)
        expect_equal(result$confidenceInterval$upper, core[[paste0("ciu[", suffix, "]")]][index], tolerance = 1e-10)
        expect_equal(result$effectSize$estimate, core[[paste0("es[", suffix, "]")]][index], tolerance = 1e-10)
        expect_identical(result$source$primary, "host-results")
    }
})

test_that("isolated fallback matches optional live core output", {
    core_ttest_load_jmv()
    skip_if_not_installed("effectsize")
    data <- data.frame(
        group = factor(c(rep("first", 8), rep("second", 13))),
        score = c(2, 4, 5, 6, 7, 8, 9, NA, 8, 9, 12, 13, 14, 15, 18, 19, 20, 22, 24, 26, 30)
    )
    minimal <- jmv::ttestIS(
        data = data, vars = "score", group = "group",
        students = TRUE, welchs = TRUE, meanDiff = FALSE, ci = FALSE,
        effectSize = FALSE, desc = FALSE, norm = FALSE, eqv = FALSE
    )
    full <- jmv::ttestIS(
        data = data, vars = "score", group = "group",
        students = TRUE, welchs = TRUE, meanDiff = TRUE, ci = TRUE,
        effectSize = TRUE, ciES = TRUE, desc = TRUE, norm = TRUE, eqv = TRUE
    )
    adapted <- .jr_core_ttest_adapter(
        list(
            vars = "score", group = "group", students = TRUE, welchs = TRUE,
            ciWidth = 95, ciWidthES = 95, hypothesis = "different",
            miss = "perAnalysis"
        ),
        core_ttest_host_results(minimal), data, allow_fallback = TRUE
    )
    core <- full$ttest$asDF

    for (result in adapted$analyses) {
        suffix <- if (result$testType == "student") "stud" else "welc"
        expect_true(result$source$repeatedCalculation)
        expect_equal(result$statistics$t, core[[paste0("stat[", suffix, "]")]][1], tolerance = 1e-10)
        expect_equal(result$statistics$df, core[[paste0("df[", suffix, "]")]][1], tolerance = 1e-10)
        expect_equal(result$statistics$p, core[[paste0("p[", suffix, "]")]][1], tolerance = 1e-10)
        expect_equal(result$statistics$meanDifference, core[[paste0("md[", suffix, "]")]][1], tolerance = 1e-10)
        expect_equal(result$confidenceInterval$lower, core[[paste0("cil[", suffix, "]")]][1], tolerance = 1e-8)
        expect_equal(result$confidenceInterval$upper, core[[paste0("ciu[", suffix, "]")]][1], tolerance = 1e-8)
        expect_equal(result$effectSize$estimate, core[[paste0("es[", suffix, "]")]][1], tolerance = 1e-8)
        expect_equal(result$descriptives$n, c(7L, 13L))
    }
})

test_that("optional fallback handles invalid data without throwing", {
    invalid <- list(
        one_group = data.frame(y = 1:4, g = factor(rep("a", 4))),
        three_groups = data.frame(y = 1:6, g = factor(rep(c("a", "b", "c"), each = 2))),
        too_small = data.frame(y = 1:3, g = factor(c("a", "b", "b"))),
        non_finite = data.frame(y = c(1, 2, Inf, 4), g = factor(c("a", "a", "b", "b"))),
        constant = data.frame(y = rep(1, 6), g = factor(rep(c("a", "b"), each = 3)))
    )
    for (data in invalid) {
        result <- expect_silent(.jr_core_ttest_supplement(data, "y", "g", "welch"))
        expect_false(result$ok)
        expect_true(length(result$warnings) > 0L)
    }
    missing_columns <- expect_silent(.jr_core_ttest_supplement(
        data.frame(x = 1:4), "y", "g", "student"
    ))
    expect_false(missing_columns$ok)
})
