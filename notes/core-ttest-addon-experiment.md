# Core Independent Samples T-Test Add-on Experiment

## Safety baseline

- Original branch: `main`
- Original commit: `a6dca6f058bb6563900be528622513606f22d7cb`
- Safety tag: `backup-before-core-ttest-addon`
- Experimental branch: `experiment/jreport-core-ttest-addon`
- Module version: `1.0.0`
- Baseline date: 2026-07-21 (Europe/London)

The original worktree had no modified tracked files. The unrelated untracked
`.claude/` directory and `jReport_1.0.0_macos.jmo.bak` file were present before
the experiment and are outside its scope.

## Baseline tests

Command:

```sh
Rscript -e 'devtools::test(stop_on_failure=TRUE)'
```

Result: 1,164 passed, 0 failed, 0 warnings, and 3 skipped. The skips were the
existing optional-package tests: one requiring `emmeans` and two requiring
`BayesFactor`.

## Baseline jamovi build

Command:

```sh
/Library/Frameworks/R.framework/Versions/4.4-arm64/Resources/library/node/node-darwin/bin/node \
  /Library/Frameworks/R.framework/Versions/4.4-arm64/Resources/library/jmvtools/node_modules/jamovi-compiler/index.js \
  --build . \
  --home /Applications/jamovi.app \
  --assume-app-version 2.7.24 \
  --jmo jReport_1.0.0_core-ttest-baseline_macos.jmo
```

Result: build and embedded R-package installation succeeded. The compiler
reported its pre-existing Node `po2json` deprecation warning. It introduced no
new build warning. The ignored build artifact had SHA-256:

```text
dec8067161f6e25f9fc4ba36c1fd6643129a53314d4251ee99dd6711c96b1cde
```

The compiler mechanically changed whitespace in two generated `.h.R` files;
those whitespace-only changes were removed before the baseline commit.

## Initial architecture observation

The repository already contains a hidden `jrReportTTestIS` analysis with
`addonFor: jmv::ttestIS`. It reads the host variable selections and test options
through `self$parent$options` and appends jReport output to the core analysis.
It does not create a ribbon entry because its module registration is hidden.

At baseline, the add-on has one local option (`pAdjustment`) and one visible
combo box. It recalculates the selected tests from the host-selected data using
jReport functions; it does not read statistics from the host result tree.
Whether a richer collapsed configuration section and stable host-result adapter
are supported remains to be established by the experiment.

## Host-result extraction decision

The add-on lifecycle exposes the host through the public `parent` binding. Host
options can be read from `parent$options`, and named host results can be
retrieved from `parent$results`. The core `ttest`, `desc`, and `assum` result
items expose data-frame representations, so their values can be reused without
re-running the primary t-test.

The remaining compatibility risk is the core table schema: column keys such as
`stat[stud]` and `stat[welc]` are owned by `jmv` and could change between host
versions. These mappings will therefore live only in a defensive adapter. A
missing table or column must produce a warning object rather than an error in
the host analysis.

The core result table only contains optional mean-difference intervals, effect
sizes, descriptives, and assumption tests when the corresponding core controls
are enabled. To keep jReport useful without changing core options, the adapter
may supplement only unavailable fields from the host-selected data. Such values
are marked as repeated calculations, use the host's selected Student/Welch test,
confidence level, hypothesis direction, and missing-data mode, and never
overwrite a finite host value. This isolated fallback is covered by numerical
comparison tests against `jmv::ttestIS`.

## Experiment outcome

The add-on binding, UI controls, host-result adapter, and renderer all work in a
live in-process `jmv::ttestIS` host. The add-on is registered with
`addonFor: jmv::ttestIS` and `hidden: true`; installed module metadata therefore
attaches it to the core analysis without adding another ribbon item. The built
UI contains a collapsed `jReport: Reporting and explanation` panel. With the
master option disabled, all jReport result items remain hidden. With it enabled,
the configured reporting boxes and references appear while the core t-test
table remains numerically unchanged.

The proof-of-concept reached a saved-state stop condition. In jamovi 2.7.24,
the server constructs add-ons as nested analysis objects and sends their options
to the engine, but `Analysis.serialize()` writes only the host analysis options
to the analysis payload stored in an `.omv`. When the host is reconstructed,
the add-on is created again from module defaults. The audit script
`scripts/check_core_ttest_addon_saved_state.py` confirms that selections such as
`jreportEnabled = true` and `reportStyle = teaching` are absent from the saved
host payload and return to `false` and `apaConcise` after reconstruction.

This payload audit is strong architectural evidence, but a desktop `.omv` was
not manually saved and reopened. Saved-file compatibility is therefore **not
claimed**. Persisting add-on UI selections would require a supported jamovi
mechanism beyond the present `addonFor` option model, or a brittle custom state
workaround. The latter is outside this experiment's safety constraints.

## Validation record

- Focused proof-of-concept suite: 207 expectations passed.
- Full test suite after implementation: 1,380 passed, 0 failed, 0 warnings,
  and 1 existing optional `BayesFactor` skip.
- jamovi compiler build: succeeded for target 2.7.24; the existing `po2json`
  Node deprecation warning remained.
- `R CMD check`: 0 errors, 0 warnings, and 4 environmental/project-layout
  notes (unavailable suggested packages, installed size, clock verification,
  and existing non-standard top-level project files).
- Final experimental package: `jReport_1.0.0_core-ttest-poc-final_macos.jmo`;
  SHA-256 `6142a8f92dbf806bfe923ed09253ee024389f459b2a47d36eb2f7fae4c88b142`.
- Installed-package metadata and generated UI were inspected successfully.
  The previous local module was backed up at
  `/private/tmp/jReport-poc-install.F37Jkd/jReport-before-experiment` before the
  experimental files were copied into the jamovi module directory.

## Manual verification status

- [x] Core `ttestIS` runs with the add-on attached in the target `jmv` package.
- [x] Core statistics are unchanged with the add-on attached.
- [x] Installed metadata declares the add-on hidden and targets `jmv::ttestIS`.
- [x] Generated installed UI declares the requested panel collapsed by default.
- [x] Master and individual reporting controls change live host output.
- [x] Multiple outcomes/tests are identified in every reporting-box title.
- [ ] Desktop GUI screenshot and pointer-driven interaction were not captured.
- [ ] A desktop `.omv` was not reopened; the payload audit instead identified
  that add-on selections are not serialised and would reset to defaults.
