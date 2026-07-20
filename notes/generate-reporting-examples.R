# Regenerate notes/reporting-qa-examples.md from deterministic fixtures.
devtools::load_all(quiet = TRUE)

d <- ToothGrowth
d$dose <- factor(d$dose)
d$dose_num <- as.numeric(as.character(d$dose))
paired <- data.frame(
    pre = c(10, 9, 8, 12, 7, 11, 8, 13, 9, 10),
    post = c(8, 8, 7, 9, 8, 9, 7, 10, 8, 8)
)
set.seed(20260720)
repeated <- data.frame(
    group = factor(rep(c("control", "treatment"), each = 18)),
    pre = rnorm(36, 10, 2), mid = rnorm(36, 11, 2), post = rnorm(36, 12, 2)
)
binary <- transform(mtcars, am = factor(am, labels = c("automatic", "manual")))
gof <- data.frame(choice = factor(rep(c("A", "B", "C"), c(20, 12, 8))))
data(bfi, package = "psych")
scale <- psych::bfi[1:100, c("A1", "A2", "A3", "A4", "A5")]
reliability <- edu_reliability_omega(
    scale, names(scale), reverse_items = "A1", bootstrap = FALSE
)

results <- list(
    "Independent-samples t test" = edu_t_test(d, "len", "supp", var_equal = FALSE),
    "Paired-samples t test" = edu_t_test(paired, "pre", paired_outcome = "post", type = "paired"),
    "One-way ANOVA" = edu_anova_oneway(d, "len", "dose", posthoc = FALSE),
    "Factorial ANOVA" = edu_anova_between(d, "len", c("dose", "supp")),
    "Repeated-measures ANOVA" = edu_anova_rm(repeated, c("pre", "mid", "post")),
    "Mixed ANOVA" = edu_anova_mixed(repeated, c("pre", "mid", "post"), "group"),
    "ANCOVA" = edu_ancova(d, "len", "supp", "dose_num"),
    "MANOVA" = edu_manova(iris, c("Sepal.Length", "Sepal.Width"), "Species"),
    "MANCOVA" = edu_manova(iris, c("Sepal.Length", "Sepal.Width"), "Species", "Petal.Length"),
    "Correlation" = edu_correlation(mtcars, "mpg", "wt"),
    "Chi-square independence" = edu_chisq_independence(d, "supp", "dose"),
    "Chi-square goodness-of-fit" = edu_chisq_gof(gof, "choice"),
    "Linear regression" = edu_lm(mtcars, mpg ~ wt + hp),
    "Binomial logistic regression" = edu_logistic_regression(binary, am ~ wt + hp),
    "Multinomial logistic regression" = suppressWarnings(
        edu_multinomial_logistic(iris, Species ~ Sepal.Length + Sepal.Width)
    ),
    "Reliability overview" = reliability,
    "Cronbach's alpha" = reliability,
    "McDonald's omega" = reliability
)

lines <- c(
    "# Guided reporting QA examples",
    "",
    "Generated from deterministic fixtures by `notes/generate-reporting-examples.R`.",
    "Each analysis includes the three acceptance-test presentation forms.",
    ""
)
for (name in names(results)) {
    result <- results[[name]]
    short <- edu_report(result, style = "apa7", format = "short", tone = "concise")
    paragraph <- edu_report(result, style = "apa7", format = "paragraph", tone = "student_friendly")
    table_paragraph <- edu_report(result, style = "journal", format = "table_paragraph", tone = "concise")
    table <- .jr_apa_table_html(result, "compact")
    lines <- c(
        lines,
        paste0("## ", name), "",
        "### Short APA sentence", "", short, "",
        "### Full results paragraph", "", paragraph, "",
        "### Table plus paragraph", "", table_paragraph, "", table, ""
    )
}

writeLines(lines, "notes/reporting-qa-examples.md", useBytes = TRUE)

