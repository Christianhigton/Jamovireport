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

test_that("generated add-on UI declares the intended collapsed panel", {
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
    loaded <- tryCatch({
        loadNamespace(paste0("j", "mv"))
        TRUE
    }, error = function(error) FALSE)
    if (!loaded)
        skip("The target jamovi jmv module could not be loaded.")
}

core_ttest_run_jmv <- function(...) {
    get("ttestIS", envir = asNamespace(paste0("j", "mv")))(...)
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
    host <- core_ttest_run_jmv(
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
    minimal <- core_ttest_run_jmv(
        data = data, vars = "score", group = "group",
        students = TRUE, welchs = TRUE, meanDiff = FALSE, ci = FALSE,
        effectSize = FALSE, desc = FALSE, norm = FALSE, eqv = FALSE
    )
    full <- core_ttest_run_jmv(
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

core_ttest_render_fixture <- function(test_type = "student", outcome = "score",
                                      levene_p = .31, p = .004) {
    structure(list(
        analysis = "independentSamplesTTest",
        analyses = list(list(
            analysis = "independentSamplesTTest",
            outcome = outcome,
            group = "condition",
            testType = test_type,
            statistics = list(t = -2.9, df = if (test_type == "student") 31 else 27.4,
                              p = p, meanDifference = -1.63),
            descriptives = data.frame(
                group = c("males", "females"), n = c(16, 17),
                mean = c(25.79, 27.42), sd = c(5.41, 6.07)
            ),
            confidenceInterval = list(level = .95, lower = -2.74, upper = -.53),
            effectSize = list(
                type = "Cohen's d", estimate = -.28,
                lower = -.47, upper = -.09, level = .95
            ),
            assumptions = list(
                levene = list(
                    available = TRUE, statistic = 1.07,
                    df1 = 1, df2 = 31, p = levene_p
                ),
                normality = list(available = TRUE, statistic = .97, p = .21)
            ),
            warnings = character(),
            references = c(
                "RCore", "jReport", "jmvcore", "effectsize",
                "Cohen1988", "Cumming2014"
            ),
            source = list(primary = "host-results", repeatedCalculation = FALSE)
        )),
        warnings = character(),
        hostSchema = "jmv::ttestIS"
    ), class = c("jr_core_ttest_adapter", "list"))
}

core_ttest_render_config <- function(...) {
    defaults <- list(
        reportStyle = "apaConcise", explanationTone = "professional",
        showSuggestedWording = TRUE, showInterpretation = TRUE,
        showEffectSizeGuidance = TRUE, showAssumptionGuidance = TRUE,
        showPracticalMeaning = TRUE, showCheckBeforeReporting = TRUE,
        showReferences = TRUE, pAdjustment = "holm"
    )
    utils::modifyList(defaults, list(...))
}

test_that("core t-test renderer labels every reporting box with its analysis", {
    rendered <- .jr_core_ttest_render(
        core_ttest_render_fixture(), core_ttest_render_config()
    )
    sections <- c(
        "Suggested APA-style report wording", "Statistical interpretation",
        "Effect-size interpretation", "Assumption guidance",
        "Practical meaning", "Check before reporting"
    )
    for (section in sections)
        expect_match(
            rendered$report,
            paste(section, "score — Student's t-test", sep = " — "),
            fixed = TRUE
        )
    expect_match(rendered$report, "data-jr-copy-section='true'", fixed = TRUE)
    expect_match(rendered$references, "R packages and software", fixed = TRUE)
    expect_match(rendered$references, "Literature and reporting guidance", fixed = TRUE)
})

test_that("each core t-test reporting section can be selected independently", {
    controls <- c(
        showSuggestedWording = "Suggested APA-style report wording",
        showInterpretation = "Statistical interpretation",
        showEffectSizeGuidance = "Effect-size interpretation",
        showAssumptionGuidance = "Assumption guidance",
        showPracticalMeaning = "Practical meaning",
        showCheckBeforeReporting = "Check before reporting"
    )
    disabled <- as.list(stats::setNames(rep(FALSE, length(controls)), names(controls)))
    for (control in names(controls)) {
        config <- utils::modifyList(
            core_ttest_render_config(showReferences = FALSE), disabled
        )
        config[[control]] <- TRUE
        html <- .jr_core_ttest_render(core_ttest_render_fixture(), config)$report
        expect_match(html, controls[[control]], fixed = TRUE)
        for (other in setdiff(names(controls), control))
            expect_false(grepl(controls[[other]], html, fixed = TRUE))
    }

    all_off <- utils::modifyList(
        core_ttest_render_config(showReferences = FALSE), disabled
    )
    rendered <- .jr_core_ttest_render(core_ttest_render_fixture(), all_off)
    expect_match(rendered$report, "No reporting sections selected", fixed = TRUE)
    expect_identical(rendered$references, "")
})

test_that("all requested style and tone combinations change renderer text", {
    combinations <- list(
        c("apaConcise", "academic"),
        c("apaDetailed", "professional"),
        c("plainLanguage", "plainEnglish"),
        c("teaching", "studentFriendly")
    )
    output <- vapply(combinations, function(values) {
        .jr_core_ttest_render(
            core_ttest_render_fixture(),
            core_ttest_render_config(
                reportStyle = values[1], explanationTone = values[2]
            )
        )$report
    }, character(1))

    expect_length(unique(output), 4L)
    expect_match(output[1], "null hypothesis", fixed = TRUE)
    expect_match(output[2], "was conducted to examine", fixed = TRUE)
    expect_match(output[3], "underlying averages were the same", fixed = TRUE)
    expect_match(output[4], "This independent-samples comparison asks", fixed = TRUE)
})

test_that("all four styles and all four tones are individually exercised", {
    styles <- c("apaConcise", "apaDetailed", "plainLanguage", "teaching")
    tones <- c("academic", "professional", "studentFriendly", "plainEnglish")
    style_output <- vapply(styles, function(style) .jr_core_ttest_render(
        core_ttest_render_fixture(),
        core_ttest_render_config(reportStyle = style, showInterpretation = FALSE)
    )$report, character(1))
    tone_output <- vapply(tones, function(tone) .jr_core_ttest_render(
        core_ttest_render_fixture(),
        core_ttest_render_config(
            explanationTone = tone, showSuggestedWording = FALSE
        )
    )$report, character(1))

    expect_length(unique(style_output), 4L)
    expect_length(unique(tone_output), 4L)
})

test_that("assumption guidance identifies the applicable test and outcome", {
    student <- .jr_core_ttest_render(
        core_ttest_render_fixture("student", levene_p = .01),
        core_ttest_render_config()
    )$report
    welch <- .jr_core_ttest_render(
        core_ttest_render_fixture("welch", outcome = "stress", levene_p = .01),
        core_ttest_render_config()
    )$report

    expect_match(student, "Welch's result should normally be preferred", fixed = TRUE)
    expect_match(student, "Assumption guidance — score — Student's t-test", fixed = TRUE)
    expect_match(welch, "Welch's test does not require equal group variances", fixed = TRUE)
    expect_match(welch, "Assumption guidance — stress — Welch's t-test", fixed = TRUE)
})

test_that("multiple core t-test results remain visibly separated", {
    fixture <- core_ttest_render_fixture()
    second <- core_ttest_render_fixture("welch", outcome = "stress")$analyses[[1]]
    fixture$analyses <- c(fixture$analyses, list(second))
    rendered <- .jr_core_ttest_render(fixture, core_ttest_render_config())

    expect_match(rendered$report, "score — Student's t-test", fixed = TRUE)
    expect_match(rendered$report, "stress — Welch's t-test", fixed = TRUE)
    expect_match(rendered$references, "Holm", fixed = TRUE)
})

test_that("renderer handles empty and changed host structures without throwing", {
    empty <- structure(list(
        analysis = "independentSamplesTTest", analyses = list(),
        warnings = "Host result structure was unavailable.", hostSchema = "jmv::ttestIS"
    ), class = c("jr_core_ttest_adapter", "list"))
    rendered <- expect_silent(.jr_core_ttest_render(empty, core_ttest_render_config()))
    expect_match(rendered$report, "Host result structure was unavailable", fixed = TRUE)
    expect_identical(rendered$references, "")
})

core_ttest_analysis_instance <- function(data = ToothGrowth) {
    options <- get("ttestISOptions", asNamespace("jmv"))$new(
        vars = "len", group = "supp", students = TRUE, welchs = TRUE,
        meanDiff = TRUE, ci = TRUE, effectSize = TRUE, ciES = TRUE,
        desc = TRUE, norm = TRUE, eqv = TRUE
    )
    get("ttestISClass", asNamespace("jmv"))$new(options = options, data = data)
}

test_that("enabled add-on renders inside a live core analysis without changing statistics", {
    core_ttest_load_jmv()
    baseline <- core_ttest_analysis_instance()
    baseline$run()
    baseline_table <- baseline$results$get("ttest")$asDF

    host <- core_ttest_analysis_instance()
    add_on <- jrReportTTestISClass$new(
        options = jrReportTTestISOptions$new(
            jreportEnabled = TRUE, reportStyle = "apaDetailed",
            explanationTone = "studentFriendly"
        ),
        data = ToothGrowth
    )
    expect_identical(host$addAddon(add_on), host)
    host$run()
    with_addon <- host$results$get("ttest")$asDF
    card <- host$results$get("jReportCard")
    references <- host$results$get("methodsReferences")

    expect_equal(with_addon, baseline_table, tolerance = 1e-12)
    expect_identical(add_on$parent, host)
    expect_true(card$visible)
    expect_match(card$content, "len — Student's t-test", fixed = TRUE)
    expect_match(card$content, "len — Welch's t-test", fixed = TRUE)
    expect_true(references$visible)
})

test_that("disabled add-on leaves all jReport output hidden", {
    core_ttest_load_jmv()
    host <- core_ttest_analysis_instance()
    add_on <- jrReportTTestISClass$new(
        options = jrReportTTestISOptions$new(jreportEnabled = FALSE),
        data = ToothGrowth
    )
    expect_identical(host$addAddon(add_on), host)
    host$run()

    for (name in .jr_ttest_is_output_names)
        expect_false(host$results$get(name)$visible)
})

test_that("live add-on section selections control host output", {
    core_ttest_load_jmv()
    host <- core_ttest_analysis_instance()
    add_on <- jrReportTTestISClass$new(
        options = jrReportTTestISOptions$new(
            jreportEnabled = TRUE,
            showSuggestedWording = FALSE,
            showInterpretation = TRUE,
            showEffectSizeGuidance = FALSE,
            showAssumptionGuidance = FALSE,
            showPracticalMeaning = FALSE,
            showCheckBeforeReporting = FALSE,
            showReferences = FALSE
        ),
        data = ToothGrowth
    )
    expect_identical(host$addAddon(add_on), host)
    host$run()
    html <- host$results$get("jReportCard")$content

    expect_match(html, "Statistical interpretation", fixed = TRUE)
    expect_false(grepl("Suggested APA-style report wording", html, fixed = TRUE))
    expect_false(grepl("Effect-size interpretation", html, fixed = TRUE))
    expect_false(host$results$get("methodsReferences")$visible)
})
