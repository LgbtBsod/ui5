# QA Rules

Each QA gate should report actionable diagnostics with:
- rule id
- severity
- file/line evidence
- fix hint
- examples
- optional patch suggestion path

## Rule docs
- dead-code.unreferenced-module.md
- drift.forbidden-edge.md
- shadow-dup.duplicate-critical-module.md

## QA runner usage
`python3 scripts/qa-runner.py` supports automation-oriented switches:
- `--changed` and `--changed-base <git-ref>`: include changed JS summary against a configurable diff base.
- `--json`: print machine-readable payload to stdout.
- `--json-file <path>`: write payload JSON artifact to disk.
- `--failfast`: execute `npm run qa` end-to-end command path.
- `--emit-artifacts`: regenerate architecture audit docs before running gates.
- `--cycle-log <path>`, `--no-cycle-log`, `--no-cycle-dedupe`: control cycle log behavior.
- `--cycle-log-raw`: opt-in to include raw QA output snippets in cycle entries.
- `--cycle-tail-lines <n>`: bound raw QA output lines when `--cycle-log-raw` is enabled.
