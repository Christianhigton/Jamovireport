# Audit Fixes Summary

## What Was Fixed

| Audit issue | Status | Files changed | Notes |
| --- | --- | --- | --- |
| Version consistency | Fixed | `DESCRIPTION`, `jamovi/0000.yaml`, `jamovi/*.a.yaml`, `.github/workflows/windows-jmo-release.yml`, tests | Public package/module/analysis versions now use `1.0.0`; static tests cover consistency. |
| Rename to `jReport` | Fixed | `DESCRIPTION`, `jamovi/0000.yaml`, generated `R/*.h.R`, `R/jamovi-helpers.R`, docs, tests, logo path | Package and module metadata now use `jReport`; user-facing module text was updated. |
| Guided error handling | Fixed | `R/edu*.b.R`, `R/jamovi-helpers.R`, `tests/testthat/test-guided-failures.R` | Core guided computation calls are wrapped in a shared friendly jamovi rejection helper; representative failure tests cover incomplete data, model-fitting failure, logistic non-binary outcomes, and wrapper coverage across guided backends. |
| Method/package references | Fixed | `jamovi/00refs.yaml`, `jamovi/*.a.yaml`, tests | Added citation keys and attached relevant refs to analyses. |
| Global library path mutation | Fixed | `R/bayesian.R`, `R/jamovi-helpers.R`, `tests/testthat/test-reporting.R`, `tests/testthat/test-audit-readiness.R` | Removed package-code `.libPaths()` mutation and added static coverage. |
| Expensive result stability | Fixed | `jamovi/edu*.r.yaml`, generated `R/edu*.h.R`, tests | Added targeted `clearWith` lists for guided computational tables/images while leaving report HTML free to update from reporting options; static tests cover guided computational result elements. |
| UI wording polish | Fixed | `jamovi/edu*.a.yaml` | Removed leading action verbs from guided checkbox labels. |
| Collapsed reporting options | Fixed | `jamovi/edu*.u.yaml` | Guided `Reporting` sections are collapsed by default. |
| Variable-box label title case | Fixed | `jamovi/edu*.u.yaml` | Variable target-box labels now use jamovi-style title case. |
| Diagnostics table layout | Fixed | `R/edu*.b.R`, `R/jamovi-helpers.R`, `tests/testthat/test-audit-readiness.R`, `tests/testthat/test-guided-failures.R` | Fixed-count guided diagnostics now prefill stable row keys and update rows with `setRow()`; dynamic diagnostics remain append-based. |
| Missing `utils` import | Fixed | `DESCRIPTION` | Added `utils` to `Imports`. |
| Stray root memo | Fixed | `notes/statistical-test-reporting-standards.md`, `.Rbuildignore` | Moved the accidental root `.R` memo into non-shipped notes. |
| Package housekeeping | Fixed | `.Rbuildignore`, docs, workflow | Ignored non-package folders/artifacts and updated build naming. |

## What Was Deferred

- Exhaustive jamovi UI-level failure-case testing for every guided model failure remains a manual verification task, but representative package-level failure coverage has been added for the shared guided computation path.
- `jmvtools::install()` could not complete because jamovi was not found on this machine.

## Tests Run

- `pkgload::load_all("."); testthat::test_dir("tests/testthat", reporter = "summary")`
  - Result: passed.
- `R CMD build . --no-build-vignettes`
  - Result: passed; built `jReport_1.0.0.tar.gz`.
- `R CMD check --no-manual --no-build-vignettes jReport_1.0.0.tar.gz`
  - Result: initially blocked by unavailable suggested packages (`BayesFactor`, `emmeans`) in the sandbox; rerun with `_R_CHECK_FORCE_SUGGESTS_=false` completed with `Status: 2 NOTEs`.
- `pkgload::load_all("."); jmvtools::install()`
  - Result: ran, but reported `jamovi could not be found!`.

## Static Checks Added

- Metadata name/version consistency.
- No old requested release versions in package/module metadata.
- No active references to the previous package/module display names.
- No `.libPaths()` calls in package R code.
- YAML parsing and reference-key validation when `yaml` is installed.
- Guided computational tables/images declare `clearWith`.
- Guided computation failures use friendly rejection messages.
- Fixed-count guided diagnostics use stable row updates; dynamic diagnostics remain append-based.
- Guided Reporting sections collapsed by default.
- Guided checkbox labels do not start with action verbs.

## Remaining Manual Checks Needed In jamovi

- Open each guided analysis panel and verify the renamed menu group, collapsed Reporting section, and title-case variable boxes.
- Trigger representative invalid guided models to confirm the friendly rejection message appears in the UI.
- Confirm add-on report cards still attach to supported native jamovi analyses.
- Run `jmvtools::install()` again after jamovi is installed or discoverable on this machine.
