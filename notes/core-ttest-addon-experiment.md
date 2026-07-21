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
