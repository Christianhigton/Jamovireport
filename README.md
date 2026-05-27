# JamoviReport

<p align="center">
  <img src="man/figures/JamoviReport-logo.png" alt="JamoviReport logo" width="220" />
</p>

`JamoviReport` is an early dual-use R package and jamovi module for guided
statistical reporting. It prioritises explanations, diagnostics, effect sizes,
confidence intervals, and report-ready prose over raw output alone.

## Implemented Analyses

- Independent and paired samples t-tests, including Bayesian report text
- Mann-Whitney U and Wilcoxon signed-rank tests with rank-biserial effects
- One-way ANOVA with optional Welch inference and post-hoc guidance
- Factorial between-subjects ANOVA with interaction-first interpretation
- Repeated-measures ANOVA with sphericity guidance
- Mixed ANOVA with within-subject correction and interaction guidance
- ANCOVA with homogeneity-of-regression-slopes diagnostics
- Reliability analysis reporting McDonald's omega total with item-quality guidance
- Pearson, Spearman, and Kendall correlation
- Chi-square test of independence with Cramer's V and expected-cell guidance
- Chi-square goodness-of-fit with Cohen's `w` and expected-cell guidance
- Multiple linear regression with residual and collinearity guidance
- Binomial logistic regression with odds ratios, confidence intervals, and
  diagnostic guidance

## R Usage

```r
library(JamoviReport)

t_test <- edu_t_test(ToothGrowth, "len", "supp")
edu_report(t_test, style = "apa7", format = "paragraph")
edu_plot(t_test)

mann_whitney <- edu_mann_whitney(ToothGrowth, "len", "supp")
edu_report(mann_whitney, style = "apa7", format = "paragraph")

bayes_t_test <- edu_bayes_t_test(ToothGrowth, "len", "supp")
edu_report(bayes_t_test, style = "apa7", format = "paragraph")

anova_data <- ToothGrowth
anova_data$dose <- factor(anova_data$dose)
anova_result <- edu_anova_oneway(anova_data, "len", "dose")
edu_report(anova_result, style = "plain", format = "bullets")

factorial <- edu_anova_between(anova_data, "len", c("dose", "supp"))
edu_report(factorial, style = "apa7", format = "paragraph")

ancova_data <- transform(ToothGrowth, dose_num = as.numeric(as.character(dose)))
ancova <- edu_ancova(ancova_data, "len", "supp", "dose_num")
edu_report(ancova, style = "dissertation", format = "paragraph")

regression <- edu_lm(mtcars, mpg ~ wt + hp)
edu_report(regression, style = "dissertation", format = "paragraph")

binary <- transform(mtcars, am = factor(am, labels = c("automatic", "manual")))
logistic <- edu_logistic_regression(binary, am ~ wt + hp)
edu_report(logistic, style = "apa7", format = "paragraph")

association_data <- transform(ToothGrowth, dose = factor(dose))
independence <- edu_chisq_independence(association_data, "supp", "dose")
edu_report(independence, style = "apa7", format = "paragraph")

preference <- data.frame(choice = factor(c(rep("A", 20), rep("B", 10), rep("C", 10))))
goodness <- edu_chisq_gof(preference, "choice", expected = c(1, 1, 1))
edu_report(goodness, style = "plain", format = "paragraph")

data(bfi, package = "psych")
scale_items <- psych::bfi[, c("A1", "A2", "A3", "A4", "A5")]
reliability <- edu_reliability_omega(scale_items, names(scale_items), reverse_items = "A1")
edu_report(reliability, style = "apa7", format = "paragraph")
```

## jamovi Module

The jamovi module exposes twelve analyses under **Educational Analyses**, including
between-subjects ANOVA, repeated-measures ANOVA, mixed ANOVA, ANCOVA, and
omega-based reliability analysis. Guided analyses now show overview,
interpretation, and copy-ready report text as separate formatted result cards.
Each includes a consistent **Reporting** section with style, format, tone, and
included-content controls. Reports update when options change because they are
rendered from the same structured result object used by the R API.

## Report Add-ons

The module follows the `moretests` pattern by registering **Report Add-ons**
for standard jamovi analyses. Once installed, supported built-in analyses
automatically append an **APA Results Summary**, an **Assumptions and Recommended
Actions** table, and an **Automatic Report (JamoviReport)** output block after
valid variables have been entered. Native add-on reports use a consistent APA
paragraph profile containing descriptives, assumption guidance, test
statistics, effect sizes, confidence intervals where available,
interpretation, and cautions.

jamovi does not allow an `addonFor` analysis to insert new report controls into
a built-in analysis options panel. Configurable style/tone controls therefore
remain available only in the module's optional Educational Analyses entries.

Initial report add-ons cover:

- Independent samples t-test, including selected Bayes factor and Mann-Whitney U output
- Paired samples t-test, including selected Bayes factor and Wilcoxon signed-rank output
- One-way ANOVA
- Factorial ANOVA
- Repeated-measures ANOVA and one-factor mixed ANOVA
- ANCOVA
- Correlation matrix reporting for Pearson, Spearman, and Kendall analyses
- Chi-square tests of independence and goodness-of-fit, with observed-versus-
  expected counts and follow-up residual information
- Linear regression
- Binomial logistic regression, with an additional odds-ratio and coefficient
  table
- Reliability analysis, reporting McDonald's omega rather than alpha

When post hoc comparisons are selected in supported native ANOVA analyses, the
add-on also appends an **APA Post Hoc Comparisons** table and explanatory
follow-up interpretation using the selected correction procedure.

For ANOVA, ANCOVA, repeated-measures, linear regression, and logistic regression
outputs with model choices that may change interpretation, the report card
states that generated reporting output **must be checked for accuracy** against
the final jamovi analysis before use in assessed, clinical, or published work.
This includes checking model terms, interactions, contrasts, reference levels,
correction choices, and other settings that affect wording.

Assumption diagnostics are presented as interpretation guidance. They do not
silently replace a planned analysis solely because a preliminary diagnostic
test is significant.

## Licence and Sharing

JamoviReport is licensed under the GNU General Public License version 3
(`GPL-3`). See [LICENSE.md](LICENSE.md) for the full repository copy of the
licence and [THIRD-PARTY-NOTICES.md](THIRD-PARTY-NOTICES.md) for the current
jamovi module dependency audit. When a `.jmo` build is shared, its corresponding
source code should be shared from the same release location.
