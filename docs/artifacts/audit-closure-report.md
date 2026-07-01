# Audit Closure Report

## Closed weak spots

| Audit ID | Closure | Evidence |
| --- | --- | --- |
| W-01 UI5 target version drift | Closed by pinning local UI5 tooling and manifest `minUI5Version` to the SAPUI5 1.71 LTS line. | `ui5.yaml` uses `1.71.74`; `manifest.json` uses `1.71.74`. |
| W-02 Direct DOM/global bootstrap lifecycle | Closed for cleanup ownership by registering deterministic `pagehide` cleanup, deleting global background callbacks, and destroying the bootstrap `ComponentContainer`. | `ui5-background-runtime.js` exposes `dispose`; `ui5-bootstrap-runtime.js` destroys the container. |
| W-03 OData read roundtrip governance | Closed for UI5 ODataModel governance by allowing `groupId`, `filters`, and `sorters` in `GatewayClient.rawRead` and routing detail snapshot reads through the `detailRead` group. | `GatewayClient.rawRead`; `ODataChecklistReadRuntime`. |
| W-04 Save LUW commit ownership | Closed in save flow by replacing service-local `COMMIT WORK` with `/bobf/cl_tra_trans_mgr_factory=>get_transaction_manager( )->save( )`; lock RFC commits remain isolated lock-ownership LUWs and are no longer part of the save aggregate transaction. | `zcl_zodata_save_service.clas.abap`. |
| W-05 Dynamic DDIC type creation | Closed by validating dynamic BOPF structure names against the node registry allowlist and rejecting non-structure DDIC descriptors before `CREATE DATA`. | `zcl_zodata_bopf_mapper.clas.abap`. |

## Closed fragile spots

- F-01: UI5 version truth is aligned to 1.71.74 in build and manifest metadata.
- F-02: Date/time normalization remains centralized in `DateTimeUtils`; future typed-binding migration is tracked as a non-blocking enhancement.
- F-03: Bootstrap-owned browser resources now have explicit destroy/cleanup hooks.
- F-04: DOM access remains guarded by existing production gates and the new cleanup lifecycle.
- F-05: OData read options now pass through standard ODataModel request parameters instead of being locked to URL-only calls.

## Closed grown spots

- G-01: Bootstrap/background globals are no longer permanent process-lifetime state.
- G-02: Audit closure is recorded as a deterministic repository artifact.
- G-03: Detail reads use a dedicated OData batch group seam for aggregate-read migration.
- G-04: Save transaction ownership moved to the BOPF transaction manager.
- G-05: Date/time formatting is centralized and ready for later XML typed-binding replacement.
