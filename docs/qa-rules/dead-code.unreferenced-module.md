# dead-code.unreferenced-module

Fails when a runtime module in `controller/`, `service/`, `infra/`, `util/`, `manager/` has zero reverse dependencies and is not allowlisted.

## Fix
- migrate callers to canonical module, then remove orphan module
- or add explicit allowlist reason when module is a framework/runtime entrypoint
