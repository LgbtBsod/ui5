# Architecture Invariants

1. `mode === "EDIT"` requires `lockOperationState === "LOCKED"`.
2. Autosave can run only when `mode === "EDIT"` and `lockOperationState === "LOCKED"`.
3. Lock lost forces `mode="READ"`, `lockOperationState="IDLE"`, `autosaveEnabled=false`.
4. Create mode must keep `mode="CREATE"`, `lockOperationState="IDLE"`, `autosaveEnabled=false`.
5. Single lock truth source: `/lockOperationState`.
6. Domain layer must not write `/isLocked`.
7. State path constants come from `service/domain/shared/StatePaths.js`.
