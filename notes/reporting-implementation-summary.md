# Guided reporting implementation summary

## Shared implementation

- Added a normalized `jr_report_model` to every `edu_analysis`. It retains raw
  numeric frames and exposes analysis context, sample sizes, tests, degrees of
  freedom, p values, effect sizes, intervals, corrections, coefficients,
  follow-ups, assumptions, warnings, table data, event/reference categories,
  narrative units, and validation issues.
- Routed all guided analyses, including factorial ANOVA, through the same
  reporting-output contract. Legacy result fields and public R functions remain
  available.
- Added central finite-number and p-value validation. User-facing formatters no
  longer emit `NaN`, `Inf`, invalid p values, or `p = .000`.
- Changed detail rendering from generic appended reminders to conditional
  descriptive, assumption, interpretation, caution, and warning units. Severe
  warnings remain visible in concise output.
- Removed automatic small/medium/large effect-size benchmark prose from default
  narratives and automatic add-on effect labels.
- Added HTML-escaped, APA-styled table rendering with compact and detailed
  analysis-specific schemas.
- Added `reportTable` and `reportTableDetail` to every guided analysis while
  retaining `reportTone` internally for saved-analysis compatibility. The UI
  label is now **Detail level** and the short format is **Short results
  sentence**.
- `table_paragraph` always activates the table panel and uses a short narrative
  that does not repeat the inferential values.

## Per-analysis table coverage

| Analysis | Compact table | Detailed additions / reporting behavior |
|---|---|---|
| Independent t | Test, t, Welch/Student df, p, d, CI | Group n, M, SD and conditional diagnostics. |
| Paired t | Paired t, paired df, p, paired effect, CI | Condition n, M, SD and paired-data diagnostics. |
| One-way ANOVA | Omnibus F, dfs, p, effect, CI | Group descriptives and available follow-up data. |
| Factorial ANOVA | Effect rows with F, dfs, p, effect, CI | Cell descriptives; interaction-first narrative retained through the shared renderer. |
| Repeated-measures ANOVA | Within-effect rows with corrected dfs, p, partial/generalized effects | Condition descriptives and sphericity/correction diagnostics. |
| Mixed ANOVA | Within, between, and interaction rows | Cell descriptives and detailed warnings; interaction interpretation is prioritized. |
| ANCOVA | Factor/covariate effects, F, dfs, p, effect, CI | Descriptives and homogeneity-of-slopes diagnostics. |
| MANOVA | Pillai's trace, approximate F, dfs, p | Holm-adjusted exploratory univariate follow-ups. |
| MANCOVA | Pillai's trace for the adjusted model | Covariate-aware labeling and adjusted exploratory follow-ups. |
| Correlation | Method, coefficient, df, p, CI | Variable n, M, SD and non-causal interpretation. |
| Chi-square independence | Chi-square, df, p, N, Cramer's V | Observed/expected cells and standardized residuals. |
| Chi-square goodness-of-fit | Chi-square, df, p, N, Cohen's w | Category observed/expected counts and residuals. |
| Linear regression | Model F, dfs, p, R-squared, adjusted R-squared | B, SE, beta, t, p, and coefficient CIs. |
| Binomial logistic | Model chi-square, df, p, pseudo R-squared | Event/reference-aware B, SE, z, p, OR, and CIs. |
| Multinomial logistic | Model fit | Coefficients grouped by outcome comparison with RRRs and reference category. |
| Reliability | Omega and alpha, N, item count, available CIs | Item scoring, M, SD, corrected item-total correlations, and loadings. |
| Cronbach's alpha | Alpha now has its own visible row | Missing alpha CI is shown as unavailable rather than invented. |
| McDonald's omega | Omega and bootstrap CI | Omega-specific precision and dimensionality/scoring cautions. |

## Warnings and edge cases

Warnings are derived centrally from diagnostic rows and classified as
`warning` or `severe`. Covered cases include assumption cautions, sparse cells,
small samples/categories, missing complete cases, nonconvergence, unstable
bootstrap intervals, scoring-direction problems, unavailable values, and
non-estimable effects. The existing analysis-specific diagnostics continue to
provide correction, slope, sphericity, covariance, collinearity, and residual
guidance.

All user-provided variable descriptions, factor levels, categories, conditions,
and labels are escaped before insertion into generated HTML.

## QA artifacts

- Pre-change inventory: `notes/reporting-architecture-audit.md`
- Reproducible representative outputs: `notes/reporting-qa-examples.md`
- Output generator: `notes/generate-reporting-examples.R`
- Automated architecture/table matrix:
  `tests/testthat/test-report-model-tables.R`

## Remaining limitations

- Native jamovi add-ons cannot expose new style/table selectors in the parent
  analysis options panel; they retain a fixed automatic APA profile.
- MANOVA/MANCOVA follow-ups remain exploratory defaults because the software
  cannot infer preregistered contrasts.
- Alpha confidence intervals are not invented when only the omega bootstrap was
  requested.
- Study-design standards such as CONSORT, STROBE, and PRISMA remain guidance
  prompts; an analysis result is never described as automatically compliant.

