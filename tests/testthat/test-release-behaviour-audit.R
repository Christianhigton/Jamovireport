release_audit_cache <- new.env(parent = emptyenv())

release_audit_results <- function() {
    if (exists("results", release_audit_cache, inherits = FALSE))
        return(release_audit_cache$results)

    two_species <- droplevels(iris[iris$Species != "virginica", ])
    factorial <- ToothGrowth
    factorial$dose <- factor(factorial$dose)
    factorial$dose_num <- as.numeric(as.character(factorial$dose))
    binary <- mtcars
    binary$am <- factor(binary$am, labels = c("automatic", "manual"))
    repeated <- data.frame(
        group = factor(rep(c("control", "treatment"), each = 15)),
        pre = seq(8, 13, length.out = 30),
        mid = seq(9, 14, length.out = 30) + rep(c(-.2, .2), 15),
        post = seq(10, 15, length.out = 30) +
            ifelse(rep(c("control", "treatment"), each = 15) == "treatment", 1, 0)
    )
    choices <- data.frame(
        choice = factor(c(rep("A", 20), rep("B", 12), rep("C", 8)))
    )
    binned <- data.frame(
        petal = factor(ifelse(iris$Petal.Length > 3.5, "large", "small")),
        sepal = factor(ifelse(iris$Sepal.Length > 5.8, "large", "small"))
    )

    results <- list(
        independent_t = edu_t_test(two_species, "Sepal.Length", "Species"),
        paired_t = edu_t_test(repeated, "pre", paired_outcome = "post", type = "paired"),
        mann_whitney = edu_mann_whitney(two_species, "Sepal.Length", "Species"),
        wilcoxon = edu_wilcoxon_signed_rank(repeated, "pre", "post"),
        one_way_anova = edu_anova_oneway(factorial, "len", "dose"),
        between_anova = edu_anova_between(factorial, "len", c("dose", "supp")),
        ancova = edu_ancova(factorial, "len", "supp", "dose_num"),
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
        multinomial = edu_multinomial_logistic(warpbreaks, tension ~ breaks + wool),
        manova = edu_manova(iris, c("Sepal.Length", "Sepal.Width"), "Species"),
        chi_square = edu_chisq_independence(binned, "petal", "sepal"),
        goodness_of_fit = edu_chisq_gof(choices, "choice", expected = c(1, 1, 1)),
        reliability = edu_reliability_omega(
            iris[, 1:4], names(iris)[1:4], bootstrap = FALSE
        )
    )
    release_audit_cache$results <- results
    results
}

release_audit_sentences <- function(text) {
    text <- gsub("\\s+", " ", trimws(text))
    sentences <- unlist(strsplit(text, "(?<=[.!?])\\s+", perl = TRUE))
    sentences[nchar(sentences) >= 45L]
}

test_that("representative release corpus covers every computation family", {
    results <- release_audit_results()
    expect_true(length(results) >= 17L)
    expect_true(all(vapply(results, inherits, logical(1), "edu_analysis")))
    expect_setequal(
        unique(vapply(results, `[[`, character(1), "analysis")),
        c(
            "ttest", "mann_whitney", "wilcoxon_signed_rank", "anova_oneway",
            "anova_between", "ancova", "anova_rm", "anova_mixed",
            "correlation", "regression", "logistic_regression",
            "multinomial_logistic", "manova", "chisq_independence",
            "chisq_gof", "reliability_omega"
        )
    )
})

test_that("generated reports contain no unfinished templates or invalid values", {
    forbidden <- paste(
        c(
            "\\bNaN\\b", "\\bInf\\b", "\\bundefined\\b", "\\bnull\\b",
            "X\\.XX", "\\.XXX", "\\{\\{", "\\}\\}", "\\[object Object\\]",
            "PLACEHOLDER", "TODO"
        ),
        collapse = "|"
    )
    for (name in names(release_audit_results())) {
        result <- release_audit_results()[[name]]
        for (style in c("apa7", "plain", "journal", "dissertation")) {
            text <- edu_report(result, style = style, format = "paragraph")
            expect_true(nzchar(text), info = paste(name, style))
            expect_false(grepl(forbidden, text, ignore.case = TRUE, perl = TRUE),
                         info = paste(name, style, text))
        }
    }
})

test_that("generated reports do not repeat complete explanatory sentences", {
    allowed <- c(
        "Assumptions and diagnostics have been reviewed."
    )
    for (name in names(release_audit_results())) {
        text <- edu_report(
            release_audit_results()[[name]], style = "apa7", format = "paragraph"
        )
        sentences <- release_audit_sentences(text)
        duplicates <- unique(sentences[duplicated(sentences)])
        duplicates <- setdiff(duplicates, allowed)
        expect_equal(
            length(duplicates), 0L,
            info = paste(name, duplicates, collapse = " | ")
        )
    }
})

test_that("diagnostic rows are complete, unique, and carry actionable guidance", {
    for (name in names(release_audit_results())) {
        diagnostics <- jReport:::.jr_normalize_diagnostics(
            release_audit_results()[[name]]$diagnostics
        )
        if (nrow(diagnostics) == 0L)
            next
        expect_false(anyDuplicated(diagnostics$check) > 0L, info = name)
        expect_true(all(nzchar(trimws(diagnostics$check))), info = name)
        expect_true(all(diagnostics$tested %in% c("Yes", "No")), info = name)
        expect_true(all(nzchar(trimws(diagnostics$status))), info = name)
        expect_true(all(nzchar(trimws(diagnostics$interpretation))), info = name)
        expect_true(all(nzchar(trimws(diagnostics$action))), info = name)
    }
})

test_that("hostile labels are escaped in HTML reporting output", {
    hostile <- ToothGrowth
    hostile$group <- factor(
        hostile$supp,
        labels = c("<script>alert('x')</script>", "A & B")
    )
    result <- edu_t_test(hostile, "len", "group")
    html <- jReport:::.jr_guided_report_sections_html(
        result,
        list(
            reportStyle = "apa7", reportFormat = "paragraph",
            reportTone = "student_friendly", reportDescriptives = TRUE,
            reportAssumptions = TRUE, reportStatistic = TRUE, reportDf = TRUE,
            reportP = TRUE, reportEffect = TRUE, reportCI = TRUE,
            reportInterpretation = TRUE, reportCautions = TRUE
        )
    )
    expect_false(grepl("<script>", html, fixed = TRUE))
    expect_match(html, "&lt;script&gt;", fixed = TRUE)
    expect_match(html, "A &amp; B", fixed = TRUE)
})
