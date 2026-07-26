structured_guidance_results <- function() {
    set.seed(20260726)
    paired <- data.frame(
        pre = stats::rnorm(36, 10, 2),
        post = stats::rnorm(36, 11, 2)
    )
    repeated <- data.frame(
        group = factor(rep(c("control", "treatment"), each = 18)),
        pre = stats::rnorm(36, 10, 2),
        mid = stats::rnorm(36, 11, 2),
        post = stats::rnorm(36, 12, 2)
    )
    between <- ToothGrowth
    between$dose <- factor(between$dose)
    between$dose_num <- as.numeric(as.character(between$dose))
    binary <- mtcars
    binary$am <- factor(binary$am, labels = c("automatic", "manual"))
    choices <- data.frame(
        choice = factor(c(rep("A", 35), rep("B", 20), rep("C", 15)))
    )
    contingency <- data.frame(
        row = factor(rep(c("A", "B", "C"), each = 2)),
        column = factor(rep(c("yes", "no"), 3)),
        n = c(35, 10, 12, 32, 22, 18)
    )

    results <- list(
        independent_t = edu_t_test(between, "len", "supp"),
        paired_t = edu_t_test(
            paired, "pre", paired_outcome = "post", type = "paired"
        ),
        mann_whitney = edu_mann_whitney(between, "len", "supp"),
        wilcoxon = edu_wilcoxon_signed_rank(paired, "pre", "post"),
        one_way = edu_anova_oneway(between, "len", "dose", posthoc = FALSE),
        between_anova = edu_anova_between(between, "len", c("dose", "supp")),
        ancova = edu_ancova(between, "len", "supp", "dose_num"),
        manova = edu_manova(
            iris, c("Sepal.Length", "Sepal.Width"), "Species"
        ),
        repeated_anova = edu_anova_rm(
            repeated, c("pre", "mid", "post"), c("Pre", "Mid", "Post")
        ),
        mixed_anova = edu_anova_mixed(
            repeated, c("pre", "mid", "post"), "group",
            c("Pre", "Mid", "Post")
        ),
        correlation = edu_correlation(mtcars, "mpg", "wt"),
        regression = edu_lm(mtcars, mpg ~ wt + hp),
        logistic = edu_logistic_regression(binary, am ~ wt + hp),
        multinomial = edu_multinomial_logistic(
            warpbreaks, tension ~ breaks + wool
        ),
        chi_independence = edu_chisq_independence(
            contingency, "row", "column", "n"
        ),
        chi_gof = edu_chisq_gof(choices, "choice"),
        reliability = edu_reliability_omega(
            psych::bfi[1:120, c("A1", "A2", "A3", "A4", "A5")],
            c("A1", "A2", "A3", "A4", "A5"),
            reverse_items = "A1", bootstrap = FALSE
        )
    )
    bayes_fallback <- results$independent_t
    bayes_fallback$analysis <- "bayes_ttest"
    bayes_fallback$label <- "Bayesian Independent Samples T-Test"
    bayes_fallback$question <- "How strongly do the data support one t-test model over another?"
    bayes_fallback$requirements <- "A justified prior and a two-group comparison."
    results$bayes_ttest <- bayes_fallback
    results
}

structured_guidance_root <- function() {
    root <- getwd()
    while (!file.exists(file.path(root, "DESCRIPTION"))) {
        parent <- dirname(root)
        if (identical(parent, root))
            skip("Source package root is not available in this installed-package check context.")
        root <- parent
    }
    root
}

test_that("every implemented analysis family renders one substantive collapsed panel", {
    results <- structured_guidance_results()
    required <- c(
        "What this analysis examines",
        "Check the variables and data",
        "Assumptions and diagnostics",
        "Main statistical findings",
        "Overall interpretation",
        "Check before using this result",
        "Literature and guidance"
    )

    for (result in results) {
        html <- .jr_interpretation_guidance_html(result)
        expect_equal(
            lengths(regmatches(html, gregexpr("<details", html, fixed = TRUE))),
            1L
        )
        expect_false(grepl("<details open", html, fixed = TRUE))
        for (heading in required)
            expect_match(html, heading, fixed = TRUE)
        expect_match(html, "aria-label='Copy text'", fixed = TRUE)
        expect_false(grepl("<h4[^>]*></h4>", html))
    }
})

test_that("independent t-test guidance handles Student, automatic Welch, sign and intervals", {
    set.seed(91)
    equal <- data.frame(
        score = c(stats::rnorm(50, 12, 2), stats::rnorm(50, 10, 2)),
        group = factor(rep(c("first", "second"), each = 50))
    )
    student <- edu_t_test(equal, "score", "group", var_equal = TRUE)
    student_html <- .jr_interpretation_guidance_html(student)
    expect_match(student_html, "Student's t-test", fixed = TRUE)
    expect_match(student_html, "first minus second", fixed = TRUE)
    expect_match(student_html, "positive sign", fixed = TRUE)
    expect_match(student_html, "excluded zero", fixed = TRUE)
    expect_match(student_html, "Cohen's d", fixed = TRUE)

    set.seed(99)
    unequal <- data.frame(
        score = c(stats::rnorm(40, 10, 1), stats::rnorm(40, 14, 9)),
        group = factor(rep(c("A", "B"), each = 40))
    )
    welch <- edu_t_test(unequal, "score", "group", var_equal = TRUE)
    welch_html <- .jr_interpretation_guidance_html(welch)
    expect_identical(welch$statistics$test, "Welch's t")
    expect_match(welch_html, "provided clear evidence that the group variances differed", fixed = TRUE)
    expect_match(welch_html, "The reported result is Welch's t-test", fixed = TRUE)
    expect_match(welch_html, "included zero", fixed = TRUE)
    expect_match(welch_html, "non-significant result is not proof", fixed = TRUE)
})

test_that("effect benchmark language is continuous rather than hard-binned", {
    thresholds <- c(small = .2, medium = .5, large = .8)
    expect_match(.jr_effect_position(.19, thresholds), "below")
    expect_match(.jr_effect_position(.20, thresholds), "at the conventional small")
    expect_match(.jr_effect_position(.34, thresholds), "closer to small")
    expect_match(.jr_effect_position(.45, thresholds), "closer to medium")
    expect_match(.jr_effect_position(.65, thresholds), "closer to medium")
    expect_match(.jr_effect_position(.75, thresholds), "closer to large")
    expect_match(.jr_effect_position(.81, thresholds), "above")
})

test_that("all diagnostic meanings and actions survive the redesign", {
    result <- edu_t_test(ToothGrowth, "len", "supp")
    html <- .jr_interpretation_guidance_html(result)
    diagnostics <- .jr_normalize_diagnostics(result$diagnostics)

    for (value in c(diagnostics$interpretation, diagnostics$action))
        expect_match(html, .jr_html_escape(value), fixed = TRUE)
    for (item in .jr_analysis_checklist("ttest"))
        expect_match(html, .jr_html_escape(item), fixed = TRUE)
})

test_that("multiple add-on analyses are individually labelled without extra guidance cards", {
    results <- list(
        edu_correlation(mtcars, "mpg", "wt"),
        edu_correlation(mtcars, "mpg", "hp", method = "spearman")
    )
    report <- .jr_addon_report_html(
        results, options = .jr_addon_reporting_options()
    )
    guidance <- .jr_addon_interpretation_html(
        results, adjustment = "holm"
    )

    expect_equal(
        lengths(regmatches(guidance, gregexpr("<details", guidance, fixed = TRUE))),
        2L
    )
    expect_match(guidance, "Interpretation guidance — Pearson: mpg with wt", fixed = TRUE)
    expect_match(guidance, "Interpretation guidance — Spearman: mpg with hp", fixed = TRUE)
    expect_false(grepl("Check before using</", guidance, fixed = TRUE))
    expect_false(grepl("Multiple comparisons and the family of tests", guidance, fixed = TRUE))
    expect_false(grepl("Interpretation guidance", report, fixed = TRUE))
    expect_false(grepl("Optional assumptions / diagnostic note", report, fixed = TRUE))
})

test_that("references are author-year, complete and not numerically enumerated", {
    result <- edu_t_test(ToothGrowth, "len", "supp")
    html <- .jr_methods_references_html(result)

    expect_match(html, "Cohen (1988)", fixed = TRUE)
    expect_match(html, "Cumming (2014)", fixed = TRUE)
    expect_match(html, "Lakens (2013)", fixed = TRUE)
    expect_match(html, "Statistical power analysis", fixed = TRUE)
    expect_false(grepl("<ol", html, fixed = TRUE))
    expect_false(grepl("\\[[0-9]+\\]", html))
})

test_that("manifest result order and stable output identifiers follow the standard", {
    skip_if_not_installed("yaml")
    root <- structured_guidance_root()
    manifest <- yaml::read_yaml(file.path(root, "jamovi", "0000.yaml"))
    names <- vapply(manifest$analyses, `[[`, character(1), "name")
    expect_length(names, 31L)

    for (name in names) {
        result_path <- file.path(root, "jamovi", paste0(name, ".r.yaml"))
        expect_true(file.exists(result_path), info = name)
        items <- yaml::read_yaml(result_path)$items
        item_names <- vapply(items, `[[`, character(1), "name")
        item_titles <- vapply(items, function(x) x$title %||% "", character(1))

        if (identical(name, "eduDemographics")) {
            expected <- c("paragraph", "bestPractices", "methodsReferences")
            titles <- c("Suggested APA Report", "Interpretation Guidance", "References")
        } else if (startsWith(name, "jrReport")) {
            expected <- c("jReportCard", "jReportInterpretation", "methodsReferences")
            titles <- c("Suggested APA Report (jReport)", "Interpretation Guidance (jReport)", "References")
        } else {
            expected <- c("report", "interpretation", "methodsReferences")
            titles <- c("Suggested APA Report", "Interpretation Guidance", "References")
        }
        positions <- match(expected, item_names)
        expect_true(all(is.finite(positions)), info = name)
        expect_true(all(diff(positions) > 0), info = name)
        expect_identical(item_titles[positions], titles, info = name)
    }
})

test_that("repository guidance audit covers every manifest and R-only analysis", {
    audit <- utils::read.csv(
        file.path(structured_guidance_root(), "qa", "guidance-initial-audit.csv"),
        stringsAsFactors = FALSE, check.names = FALSE
    )
    expect_equal(nrow(audit), 34L)
    expect_equal(length(unique(audit$analysis_id)), 34L)
    expect_true(all(nzchar(audit$recommended_action)))
    expect_true(all(nzchar(audit$implementation_status)))
})

test_that("final guidance QC records a passing disposition for every audited analysis", {
    root <- structured_guidance_root()
    initial <- utils::read.csv(
        file.path(root, "qa", "guidance-initial-audit.csv"),
        stringsAsFactors = FALSE, check.names = FALSE
    )
    final <- utils::read.csv(
        file.path(root, "qa", "guidance-final-qc.csv"),
        stringsAsFactors = FALSE, check.names = FALSE
    )

    expect_equal(nrow(final), 34L)
    expect_setequal(final$analysis_id, initial$analysis_id)
    expect_true(all(final$final_status == "passed"))
    expect_true(all(final$automated_tests == "pass"))
    expect_true(all(nzchar(final$notes)))
})
