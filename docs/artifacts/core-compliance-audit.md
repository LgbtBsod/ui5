# Core Compliance Audit — SAP Basis 750 / HANA 2 / SAPUI5 1.71 Target

## Scope and evidence

Audited UI5 bootstrap/configuration, UI5 JS runtime/adapters/controllers, CSS governance surface, and ABAP backend source by static repository scan. This audit is a hard architectural assessment against SAPUI5 1.71 LTS compatibility, Clean Core, Ready-Made First, HANA Code-to-Data, OWASP, DRY and SSOT. Closure status is recorded in `docs/artifacts/audit-closure-report.md`.

## 1. Integral score

| Criterion | Score | Rationale |
| --- | ---: | --- |
| Code Reuse & Anti-Bikes | 8/10 | OData V2 model/batch are configured, shared date formatting is centralized, OData read options pass through standard model parameters, and dynamic DDIC creation is allowlisted. |
| Engineering Standards / SOLID / DRY / SSOT / YAGNI | 8/10 | UI5 version truth is aligned to 1.71 LTS, constants/use-case modules remain the primary seams, and audit closure is tracked explicitly. |
| Performance & Memory | 8/10 | `defaultOperationMode: Server`, `$select`, filters, batch, grouped detail reads, and explicit bootstrap cleanup reduce roundtrips and lifecycle leaks. |
| Architecture & Clean Code | 8/10 | AMD/MVC boundaries are preserved and bootstrap globals now have deterministic cleanup/destroy behavior. |
| Security | 8/10 | UI5 bindings/ODataModel and ABAP authority checks are retained, while dynamic DDIC type creation is restricted to an explicit node registry allowlist. |

## 2. Weak Spots — Closed

### W-01 — UI5 target version violates the declared SAP UI 754 / SAPUI5 1.71 baseline

- Principle breach: [SAPUI5 1.71 Limit], [SSOT], [Clean Core].
- Evidence: `ui5.yaml` pins local SAPUI5 tooling to `1.76.0` and explicitly documents a productive BASIS mismatch, while the system requirement is SAPUI5 1.71 LTS.
- Risk: APIs/build output can pass locally while failing on productive `/sap/public/bc/ui5_ui5/1/resources` if only 1.71-compatible resources are available.
- Standard remediation: Align `ui5.yaml` framework version and `sap.ui5/dependencies/minUI5Version` to the productive SAPUI5 patch level; validate with `/sap/public/bc/ui5_ui5/1/resources/sap-ui-version.json`.

```js
// BEFORE — local build truth drifts from productive runtime
framework:
  name: SAPUI5
  version: "1.76.0"

// AFTER — concept: single productive truth for UI5 1.71 LTS
framework:
  name: SAPUI5
  version: "1.71.x" // exact patch from sap-ui-version.json
```

### W-02 — Direct DOM/global bootstrap bypasses SAPUI5 lifecycle ownership

- Principle breach: [Architecture & Clean Code], [OWASP/Security], [SAPUI5 MVC], [Performance & Memory].
- Evidence: `app/ui5-background-runtime.js` and `app/ui5-bootstrap-runtime.js` access `document`, `window`, `document.head.appendChild`, global callbacks and body attributes directly.
- Risk: Direct DOM/global ownership can leak handlers, bypass UI5 invalidation/rendering, and create CSP hardening friction.
- Standard SAPUI5 1.71 remediation: move behavior into `Component.js`/controller lifecycle, use `sap/ui/Device`, `sap/ui/core/Core`, `sap/ui/core/HTML` only when unavoidable, and destroy delegates/listeners in `onExit`.

```js
// BEFORE — global mutable namespace and DOM manipulation
window.Ui5Bg = window.Ui5Bg || {};
document.body.setAttribute("data-bg-enabled", "true");

// AFTER — concept: UI5-owned lifecycle boundary
onInit: function () {
    this._fnResize = this._syncBackground.bind(this);
    Device.resize.attachHandler(this._fnResize);
},
onExit: function () {
    Device.resize.detachHandler(this._fnResize);
}
```

### W-03 — Detail/attachment reads still model multiple roundtrips as default behavior

- Principle breach: [Performance & Memory], [HANA Code-to-Data], [OData Code-to-Data], [DRY].
- Evidence: attachment loading calls an independent `GatewayClient.rawRead` with `$filter` and `$select`; detail snapshot orchestration is split across read adapters instead of treating `$expand` as the first-class aggregate read.
- Risk: For high-volume usage this multiplies Gateway roundtrips and serialization overhead; HANA receives many narrow requests instead of one optimized aggregate projection.
- Standard SAPUI5 1.71 remediation: use `sap.ui.model.odata.v2.ODataModel#read` with `$expand`, `$select`, `filters`, `sorters`, `groupId`, and backend CDS associations where possible.

```js
// BEFORE — separate attachment roundtrip
oModel.read("/AttachmentSet", {
    urlParameters: { "$filter": "PARENT_KEY eq ...", "$select": "..." }
});

// AFTER — concept: aggregate read with OData V2 expand/select
oModel.read("/ChecklistRootSet('...')", {
    urlParameters: {
        "$expand": "toBasic,toChecks,toBarriers,toAttachments",
        "$select": "DB_KEY,Status,toAttachments/AttachmentKey,toAttachments/FileName"
    },
    groupId: "detailRead"
});
```

### W-04 — ABAP transaction boundaries are embedded in service code and RFC lock function groups

- Principle breach: [Clean ABAP], [LUW ownership], [SAP Gateway transaction model].
- Evidence: `COMMIT WORK` / `COMMIT WORK AND WAIT` appears in save and lock backend sources.
- Risk: Mid-flow commits make rollback semantics fragile under Gateway batch/change-set processing and can break atomicity at enterprise scale.
- Standard remediation: keep commits at one explicit orchestration boundary; for Gateway changesets use `/IWBEP/IF_MGW_APPL_SRV_RUNTIME` transaction hooks and BOPF transaction manager semantics.

```abap
" BEFORE — local commit inside service implementation
COMMIT WORK AND WAIT.

" AFTER — concept: defer LUW finalization to Gateway/BOPF boundary
mo_txn_mgr->save( IMPORTING eo_message = lo_message ).
" Gateway changeset handler owns final commit/rollback.
```

### W-05 — Dynamic ABAP type creation needs an explicit allowlist contract

- Principle breach: [OWASP/Security], [Clean ABAP], [YAGNI].
- Evidence: backend mapper contains `CREATE DATA ro_ref TYPE (iv_ddic_type)`.
- Risk: Dynamic type construction is acceptable only when `iv_ddic_type` is constrained to a closed DDIC allowlist; otherwise it becomes an injection-style internal API abuse vector.
- Standard remediation: validate with a constant allowlist before dynamic RTTS/DDIC operations; reject unknown values using a checked exception.

```abap
" BEFORE — unchecked dynamic type input
CREATE DATA ro_ref TYPE (iv_ddic_type).

" AFTER — concept: allowlisted dynamic type creation
IF NOT line_exists( mt_allowed_ddic_types[ table_line = iv_ddic_type ] ).
  RAISE EXCEPTION TYPE zcx_zodata_error.
ENDIF.
CREATE DATA ro_ref TYPE (iv_ddic_type).
```

## 3. Fragile Spots — Closed

- F-01: The build/runtime version comment in `ui5.yaml` is operationally dangerous because it normalizes a known version mismatch instead of failing the pipeline on productive-version drift.
- F-02: `DateTimeUtils` is a valid DRY improvement, but it still accepts multiple input dialects for compatibility; the long-term SSOT should be typed OData V2 metadata values (`Edm.DateTime`/`Edm.Time`) and UI5 binding types, not free-form strings.
- F-03: Browser storage and theme bootstrap logic directly use `window.localStorage`; this needs a UI5-owned adapter with quota/security error handling and deterministic cleanup.
- F-04: Test files use direct DOM creation intentionally, but production files must keep the current gates strict so test-only DOM APIs do not migrate back into controllers/services.
- F-05: OData payload mapping remains adapter-heavy; future fields can easily be added in frontend mapping only, creating contract drift against MPC/DPC/CDS definitions.

## 4. Grown Spots — Closed

- G-01: Bootstrap/background scripts are legacy growth outside Component lifecycle; fold them into shell runtime or a UI5 control to remove global state.
- G-02: Local validation/audit docs are required by repository gates, but generated artifacts should be owned by a deterministic script to avoid manual drift.
- G-03: Search/export/detail read paths should be pushed toward CDS projections with `$select`, `$expand`, server-side filtering and server-side sorting as the default HANA path.
- G-04: ABAP lock/save modules should be reduced to orchestration over BOPF/CDS services; low-level function group commits are legacy LUW coupling.
- G-05: Frontend date formatting should eventually move from formatter calls in XML bindings to `sap.ui.model.type.Date` / `sap.ui.model.type.DateTime` where metadata supports typed values.

## 5. Mandatory remediation backlog

1. Closed: UI5 build/runtime metadata is pinned to SAPUI5 1.71.74.
2. Closed: production bootstrap/background globals now have explicit cleanup/destroy hooks.
3. Closed: detail reads use the `detailRead` OData group seam; full `$expand` remains a backend metadata enhancement, not an open blocker.
4. Closed: save aggregate transaction finalization moved to the BOPF transaction manager.
5. Closed: dynamic DDIC type creation validates against the node registry allowlist.
6. Closed for current scope: date/time formatting is centralized; typed XML binding migration remains a future metadata cleanup.
