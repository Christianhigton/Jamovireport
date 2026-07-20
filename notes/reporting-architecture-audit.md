# Guided reporting pre-change audit

Audit date: 2026-07-20

This audit records the state of the guided analyses before the reporting
architecture and APA-table implementation. The existing R test suite passed;
three optional tests were skipped because `BayesFactor` and `emmeans` were not
installed. `R CMD check` completed with no errors, two warnings, and four
notes.

## Shared findings

- `edu_analysis` stores raw tables alongside separately authored APA and plain
  text blocks, but it does not expose one normalized reporting model.
- Style and detail selectors mostly choose or append prose. Inclusion options
  remove text with regular expressions, so numerical invariance is not
  guaranteed by construction.
- `table_paragraph` adds an instruction to consult a table without producing a
  reporting table.
- No guided analysis exposes compact/detailed APA-table controls.
- Factorial ANOVA has a separate reporting path; the remaining guided analyses
  use the generic renderer.
- Effect-size benchmark text is added automatically when effect sizes are
  selected, even when the reporting mode does not need threshold guidance.
- `.jr_num()` and `.jr_p()` do not reject all non-finite or invalid values.
- Reference keys are deduplicated, but native add-ons currently mutate the
  parent reference registry at runtime.

## Analysis inventory

| Analysis | Current narrative and statistics | Current table / principal gap |
|---|---|---|
| Independent-samples t test | APA/plain blocks include group descriptives, t, df, p, d, and CI. Style/format/detail use the shared text selector. | Computational tables only; no selectable APA table or table-plus-paragraph output. |
| Paired-samples t test | Shares the t-test object and renderer; paired descriptives and test values are present. | No APA table; paired effect terminology is not represented in a normalized schema. |
| One-way ANOVA | Includes omnibus inference, group descriptives, effect size, diagnostics, and optional post-hoc text. | No APA table; compact/detailed post-hoc presentation is absent. |
| Factorial ANOVA | Has a bespoke APA renderer with improved omnibus wording. | Renderer diverges from every other analysis; no selectable table and interaction priority is not a shared rule. |
| Repeated-measures ANOVA | Stores corrected ANOVA rows and diagnostic prose. | No reporting table; correction metadata is embedded in analysis-specific output rather than normalized. |
| Mixed ANOVA | Stores within, between, and interaction rows plus diagnostics. | No reporting table or shared interaction/simple-effect presentation. |
| ANCOVA | Stores model effects, descriptives, and slope diagnostics. | No reporting table; heterogeneous-slope warnings are not centrally classified. |
| MANOVA | Pillai statistics and exploratory univariate follow-ups are available. | Same analysis type is used for MANOVA/MANCOVA; no variant-specific table. |
| MANCOVA | Label changes when covariates are present and the narrative mentions adjustment. | No distinct normalized variant, table schema, or warning policy. |
| Correlation | Method, coefficient, n, p, CI, descriptives, and diagnostics are available. | No APA correlation table; causal-language constraints are not a central validation rule. |
| Chi-square independence | Omnibus statistics, Cramer's V, cells, and expected-count diagnostics are available. | No compact/detailed observed/expected/residual table. |
| Chi-square goodness-of-fit | Omnibus statistics, Cohen's w, and category cells are available. | No selectable APA table; sparse expected counts are only diagnostic prose. |
| Linear regression | Model fit and coefficient tables contain the required raw values. | No combined APA reporting table; generic renderer does not structure model-fit and coefficient findings separately. |
| Binomial logistic regression | Model fit, coefficients, ORs, CIs, and diagnostics are present. | No APA table; event/reference category is not a required normalized field. |
| Multinomial logistic regression | Overall fit and category-comparison coefficients are present. | No grouped APA table and incomplete checklist/reference specialization. |
| Reliability overview | Omega, alpha, item diagnostics, scoring, missing-data information, and bootstrap output are calculated. | Guided result definition has one fixed row and backend populates only omega. |
| Cronbach's alpha | Alpha is calculated and mentioned in prose. | Alpha is silently omitted from the main guided result table; no alpha-specific table row/limitations. |
| McDonald's omega | Omega and optional bootstrap CI are reported with item diagnostics. | No compact/detailed APA table; coefficient guidance still uses generic threshold language. |

## Baseline check issues

- Non-ASCII statistical symbols in R source trigger a portability warning.
- Exported demographics and multinomial objects lack generated documentation.
- Demographics helpers call `head`, `median`, `quantile`, and `sd` without
  namespace qualification/import declarations.
- Optional-package, timestamp, and non-standard top-level-file notes are
  environmental or inherent to the combined R/jamovi source layout.

