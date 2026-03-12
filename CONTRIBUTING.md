# Contributing

## Workflow

1. Create a branch from `main`.
2. Implement changes with tests/gates.
3. Run `node scripts/qa-all.js` locally.
4. Open a PR with architecture/runtime impact notes.

## Required Checks

- QA pipeline: `node scripts/qa-all.js`
- License/SBOM gate: `node scripts/ci/license-sbom-gate.js`

## Coding Rules

- Keep domain/runtime constants in canonical contract modules.
- Avoid inline workflow/route literals when a contract exists.
- Keep CSS color tokens in token files; avoid raw hex in module files.
