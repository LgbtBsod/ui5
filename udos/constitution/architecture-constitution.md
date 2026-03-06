# Architecture Constitution

## Principle 1 — Layer Integrity
Controllers must never import adapters.

## Principle 2 — Domain Isolation
Domain layer cannot depend on UI or infrastructure.

## Principle 3 — State Authority
`/lockOperationState` is the single source of lock truth.

## Principle 4 — Autosave Safety
Autosave may run only when:
- `mode = EDIT`
- `lockOperationState = LOCKED`
- `dirty = true`

## Principle 5 — Workflow Safety
Lock/autosave/cache workflows must preserve invariants.

---
These principles are immutable.
Policy/case law cannot override constitutional violations.
