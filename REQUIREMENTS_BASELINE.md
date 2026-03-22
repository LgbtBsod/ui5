# Requirements Baseline

## Scope
- Bring the project to production-ready state.
- Stay aligned with SAP UI5 1.71, SAP Gateway OData V2, CDS/SADL read path, BOPF modify path.
- Reduce overengineering and duplicated ownership.

## Non-negotiable rules
- `DB_KEY` and `PARENT_KEY` stay canonical technical keys.
- JSONModel runtime pattern stays the frontend state pattern.
- `masterData>/runtime/*` stays the canonical runtime settings source.
- Search semantics stay `EXACT = AND`, `INEXACT/LOOSE = OR`.
- Backend fetch limit and UI display chunk size stay separate.
- Attachments stay metadata-first and lazy-only.
- Delete target flow stays canonical modification pipeline, not direct OData DELETE.
- Lock flow stays acquire/status/heartbeat/release with self-steal only through `ForceTakeover`.

## Engineering rules
- Do not invent new framework layers.
- Prefer merge/delete/simplify over adding abstraction.
- One owner per responsibility.
- Repeated technical strings must move into constants modules.
- Unsupported SAP internal CSS/DOM patching must be reduced or documented as residual risk.

## Evidence constraint
- If behavior is not provable from code, mark it `Unknown`/`Not proven`.
- Do not invent backend semantics that are not evidenced by code or metadata.
