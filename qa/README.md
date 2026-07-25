# jReport release audit

Run the automated release gate from the repository root:

```sh
Rscript scripts/run_release_audit.R
```

Regenerate the non-confidential desktop test data when needed:

```sh
Rscript scripts/create_qa_dataset.R
```

Then complete `manual-test-matrix.csv` in a freshly restarted jamovi session.
Use one row per analysis entry and record `Pass`, `Fail`, or `Blocked` in each
check column. A release candidate is ready only when:

- the automated audit passes;
- every supported entry has been tested with valid and problematic data;
- no Critical or High bugs remain open;
- saved analyses reopen with the same statistics and visible report content;
- no duplicated, contradictory, placeholder, or unattributed reporting remains.

Create one copy of `bug-report-template.md` for every failure. Screenshots and
the smallest reproducible `.omv` file should be stored beside the completed
matrix, but should not contain confidential participant data.
