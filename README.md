# jReport

<p align="center">
  <img src="man/figures/jReport-logo.png" alt="jReport logo" width="220" />
</p>

`jReport` is an early dual-use R package and jamovi module for guided
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
- MANOVA/MANCOVA automatic reporting using Pillai's trace with automatic
  Holm-adjusted univariate follow-up analyses after significant omnibus effects
- Reliability analysis reporting McDonald's omega total with item-quality guidance
- Pearson, Spearman, and Kendall correlation
- Chi-square test of independence with Cramer's V and expected-cell guidance
- Chi-square goodness-of-fit with Cohen's `w` and expected-cell guidance
- Multiple linear regression with residual and collinearity guidance
- Binomial logistic regression with odds ratios, confidence intervals, and
  diagnostic guidance

## R Usage

```r
library(jReport)

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

The jamovi module exposes supported analyses under **jReport**, including
between-subjects ANOVA, repeated-measures ANOVA, mixed ANOVA, ANCOVA, and
omega-based reliability analysis. Guided analyses now show overview,
interpretation, and copy-ready report text as separate formatted result cards.
Each includes a consistent **Reporting** section with independent style,
format, detail-level, included-content, and APA results-table controls. Compact
and detailed tables use schemas appropriate to the selected analysis, and
**Table plus paragraph** always displays a table with a short non-duplicative
narrative. Reports update when options change because narratives and tables are
rendered from the same validated result model used by the R API.

Simplified report-style output is available only for supported analyses and
will appear when those analyses are run from their respective jamovi analysis
menus. Assumption tables state whether each assumption was tested, the statistic
and p value where available, whether the assumption appears to have been met,
and guidance for interpreting violations or checks that require design review.
Effect sizes are named and reported without automatically assigning generic
small, medium, or large labels. Practical importance remains a substantive
judgement tied to the study context and uncertainty in the estimate.
Where jamovi variables include a description, jReport uses that
description in report text, assumption tables, headings, and interpretation
notes. Blank descriptions or descriptions identical to the raw variable name
fall back to the variable name, and calculations continue to use the original
variable names internally.

## Installing in jamovi

A compiled `.jmo` installer can be provided for direct download as an asset on
the [GitHub Releases page](https://github.com/Christianhigton/jReport/releases).
Compiled installers are not kept in the source-code branch because they are
build artifacts. If no `.jmo` asset is shown on the Releases page, a
downloadable build has not yet been published.

To install a released build:

1. Open the [Releases page](https://github.com/Christianhigton/jReport/releases).
2. Download the `jReport_*.jmo` file that matches your system and jamovi version.
3. In jamovi, open the module manager, choose to sideload/install a module, and select the downloaded `.jmo` file.

A `.jmo` build is platform-specific. For example, the current development
build was produced on macOS Apple Silicon for the current jamovi 2.7 series;
it should not be assumed to work on Windows, Linux, Intel Mac, or the jamovi
solid series. Separate compatible builds or a jamovi Library release are
required for those environments.

Windows x64 development builds use the filename
`jReport_1.0.0_windows_x64_jamovi-2.7.jmo`, generated on a Windows
runner for the jamovi 2.7 series.

## Report Add-ons

The module follows the `moretests` pattern by registering **Report Add-ons**
for standard jamovi analyses. Once installed, supported built-in analyses
automatically append an **APA Results Summary**, an **Assumptions and Recommended
Actions** table, and an **Automatic Report (jReport)** output block after
valid variables have been entered. Native add-on reports use a consistent APA
paragraph profile containing descriptives, assumption guidance, test
statistics, effect sizes, confidence intervals where available,
interpretation, and cautions.

jamovi does not allow an `addonFor` analysis to insert new report controls into
a built-in analysis options panel. Configurable style/tone controls therefore
remain available only in the module's optional **jReport**
entries. jReport labels this distinction in its results output.

Initial report add-ons cover:

- Independent samples t-test, including selected Bayes factor and Mann-Whitney U output
- Paired samples t-test, including selected Bayes factor and Wilcoxon signed-rank output
- One-way ANOVA
- Factorial ANOVA
- Repeated-measures ANOVA and one-factor mixed ANOVA
- ANCOVA
- MANOVA/MANCOVA, reporting Pillai's trace for the multivariate model and
  automatic Holm-adjusted univariate follow-up analyses when the omnibus
  effect is significant
- Correlation matrix reporting for Pearson, Spearman, and Kendall analyses
- Chi-square tests of independence and goodness-of-fit, with observed-versus-
  expected counts and follow-up residual information
- Linear regression
- Binomial logistic regression, with an additional odds-ratio and coefficient
  table
- Reliability analysis, reporting McDonald's omega rather than alpha

When post hoc comparisons are selected in supported native ANOVA analyses, the
add-on only reports comparisons for a statistically significant omnibus main
effect. If a factorial interaction is significant, it reports follow-up
comparisons for that interaction so main effects are not interpreted without
examining the conditional pattern. Selected correction procedures are retained.

For MANOVA/MANCOVA, significant Pillai omnibus effects trigger automatic
follow-up univariate ANOVA/ANCOVA models for each dependent variable using the
same factor and covariate structure. Raw p values and Holm-adjusted p values
are reported to control the family-wise error rate across dependent variables.
These follow-ups are exploratory defaults: planned contrasts cannot be
generated automatically because the module cannot know which hypotheses were
specified before data collection.

For ANOVA, ANCOVA, repeated-measures, linear regression, and logistic regression
outputs with model choices that may change interpretation, the report card
states that generated reporting output **must be checked for accuracy** against
the final jamovi analysis before use in assessed, clinical, or published work.
This includes checking model terms, interactions, contrasts, reference levels,
correction choices, and other settings that affect wording.

Assumption diagnostics are presented as interpretation guidance. They do not
silently replace a planned analysis solely because a preliminary diagnostic
test is significant. Some assumptions, such as independence, category
exclusivity, model specification, and measurement-scale suitability, cannot be
verified from the selected columns alone and are therefore flagged as requiring
research-design review.

## Effect Size Benchmarks

Effect size benchmarks follow common conventions such as Cohen's small, medium,
and large guidelines. The module reports them as rough interpretive aids rather
than strict thresholds. Practical importance should be judged in the context of
the research area, measurement scale, and existing literature.

References:

- Cohen, J. (1988). *Statistical Power Analysis for the Behavioral Sciences* (2nd ed.). Lawrence Erlbaum Associates.
- Cumming, G. (2014). The new statistics: Why and how. *Psychological Science, 25*(1), 7-29.

## Licence and Sharing

jReport is licensed under the GNU General Public License version 3
(`GPL-3`). See [LICENSE.md](LICENSE.md) for the full repository copy of the
licence and [THIRD-PARTY-NOTICES.md](THIRD-PARTY-NOTICES.md) for the current
jamovi module dependency audit. When a `.jmo` build is shared, its corresponding
source code should be shared from the same release location.
