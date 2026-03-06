# Final Stabilization Notes

- Canonicalized `StatePaths` usage via shared module export.
- Removed production writes to `/isLocked` in domain/edit-lock flows.
- Normalized lock-lost flow to `READ + IDLE + autosave disabled`.
- Updated create flow to initialize `CREATE` mode and disabled autosave.
- Added architecture gates under `scripts/gates/` for lock enum, invariant, layers, and code size.
