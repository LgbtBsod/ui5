# Final Compliance Report

## 2026-03-27 Final Production-Readiness Pass

### 2026-03-27 Canonical Contract Sync Addendum
- Local runtime smoke flake was reduced to deterministic orchestration:
  - `scripts/local-launch-smoke.ps1` is now the canonical automation entrypoint
  - it can bootstrap the environment itself, verifies repeated `200` reachability for UI and metadata, and validates repeated search JSON availability
  - `BindToParentShell` is now explicitly documented as interactive-only because it intentionally stops the environment when the parent shell exits
- Browser facade smoke contract was aligned with the real post-cleanup domain surface:
  - `scripts/browser-smoke-domain-facade-contract.py` no longer imports removed `ExportFacade` / `LockFacade`
  - the smoke contract now verifies the actual `DetailFacade` and `SearchFacade` method surface only
  - stale facade smoke drift is no longer masking real runtime readiness
- Documentation drift on lock contract naming was closed:
  - `ABAP.txt`
  - `FUNCTION_IMPORTS.txt`
  - `LOCK_STATE_MACHINE.txt`
  - `UPDATED_FUNCTION_IMPORTS.txt`
  - `UPDATED_LOCK_STATE_MACHINE.txt`
  - all now describe `DB_KEY` as the canonical lock/copy parameter and `ObjectUuid` only as narrow backend ingress compatibility
- Governance drift was closed:
  - `scripts/internal/controller-util-allowlist.json` no longer references deleted wrapper owners
  - architecture allowlist now reflects the active repo tree instead of historical wrapper names
- Detail info-card raw fallback copy was removed:
  - `app/controller/detail/DetailInfoCardFactory.js` now resolves labels/tooltips strictly through i18n keys instead of embedded English fallback text

### 2026-03-27 Final Hardening Addendum
- One more detail-layer pass-through wrapper was removed:
  - deleted `app/controller/detail/DetailCommandPolicy.js`
  - detail command dispatch is now controller-owned through `DetailControllerRuntime.js` and called directly by detail behaviors/runtimes
- Another pass-through wrapper was removed:
  - deleted `app/controller/search/SearchCommandPolicy.js`
  - search command dispatch is now controller-owned through `Search.controller.js` and used directly by search behaviors
- Raw user-facing fallback copy was removed from active analytics/search runtime:
  - `app/controller/analytics/AnalyticsFormatRuntime.js`
  - `app/service/features/analytics/runtime/AnalyticsBuilderRuntime.js`
  - `app/controller/search/SearchLifecycleBehavior.js`
- `raw-ui-text-gate.js` now scans `app/service/features/**` and flags raw fallback copy passed into `getText(...)`/`fnGetText(...)`.
- DOM debt governance is now explicit quarantine governance instead of a hardcoded allowlist:
  - `scripts/dom-hack-allowlist.json` is the single reasoned owner for permitted DOM-boundary files
  - `dom-hack-gate.js` now fails on allowlist growth or vague quarantine reasons
- CSS debt quarantine is tighter:
  - `sap-internal-css-gate.js` now fails if the SAP-selector allowlist grows beyond the frozen quarantine size
  - each allowlisted CSS file must keep a non-trivial quarantine reason
- Verification for this addendum:
  - `node scripts/raw-ui-text-gate.js`
  - `node scripts/sap-internal-css-gate.js`
  - `node scripts/dom-hack-gate.js`
  - `node scripts/attachment-contract-gate.js`
  - `node scripts/lock-contract-naming-gate.js`
  - `node scripts/final-residual-cleanup-gate.js`

### Closed In This Pass
- Attachment architecture is now explicit standard-first:
  - productive binary upload remains only on `AttachmentSet` media POST
  - [`gateway_canonical_api.py`](/Users/lgbtb/Desktop/ui5/backend/mock_gateway/api/gateway_canonical_api.py) now rejects base64 attachment payloads on `SaveChanges`
  - attachment save mutations now handle metadata/delete semantics only, while media bytes are persisted only on the media endpoint
  - canonical persisted read surface remains `DownloadUrl` / `DocumentHandle`
- Thin-wrapper cleanup closed real overengineering debt:
  - deleted [`ControllerRouteRuntime.js`](/Users/lgbtb/Desktop/ui5/app/service/framework/ControllerRouteRuntime.js)
  - deleted [`FeedbackCoordinator.js`](/Users/lgbtb/Desktop/ui5/app/service/framework/FeedbackCoordinator.js)
  - deleted `app/controller/search/SearchCommandPolicy.js`
  - deleted `app/controller/detail/DetailCommandPolicy.js`
  - route binding/unbinding moved into actual route owners:
    - [`AnalyticsLifecycleBehavior.js`](/Users/lgbtb/Desktop/ui5/app/controller/analytics/AnalyticsLifecycleBehavior.js)
    - [`DetailControllerBehavior.js`](/Users/lgbtb/Desktop/ui5/app/controller/detail/DetailControllerBehavior.js)
    - [`SearchLifecycleBehavior.js`](/Users/lgbtb/Desktop/ui5/app/controller/search/SearchLifecycleBehavior.js)
  - feedback calls now go directly to real owners:
    - [`FeedbackDefaultHandlers.js`](/Users/lgbtb/Desktop/ui5/app/service/framework/behavior/FeedbackDefaultHandlers.js)
    - [`FeedbackBehaviorHelpers.js`](/Users/lgbtb/Desktop/ui5/app/service/framework/execution/behavior/FeedbackBehaviorHelpers.js)
- Contract gates were tightened around final architecture:
  - [`attachment-contract-gate.js`](/Users/lgbtb/Desktop/ui5/scripts/attachment-contract-gate.js) now requires explicit rejection of base64 attachment save path and keeps media upload as the sole allowed binary path
  - [`wrapper-sprawl-gate.js`](/Users/lgbtb/Desktop/ui5/scripts/wrapper-sprawl-gate.js) now permanently bans the removed wrapper files from reappearing
- Lock naming remains canonical on the frontend surface:
  - frontend lock surface stays on `dbKey` / `DB_KEY`
  - lock gate is green and `ObjectUuid` remains only narrow backend compatibility ingress
- CSS/DOM governance stays green:
  - `sap-internal-css-gate` passes
  - `dom-hack-gate` passes
  - remaining `.sap*` debt is still quarantined, not silently normalized

### Canonical Model Status
- Root identity: `DB_KEY`
- Root entity: no `PARENT_KEY`
- Child identity: own `DB_KEY`
- Child-to-root relation: `PARENT_KEY`
- Frontend lock/copy/detail canonical key surface: `dbKey`
- Attachment persisted contract: `AttachmentKey`, `DB_KEY`, `PARENT_KEY`, `DownloadUrl`, `DocumentHandle`
- Attachment upload transport: media POST body on `AttachmentSet`
- JSON/Base64 attachment payload on aggregate save: forbidden

### SAP Best Practice Status
- ODataModel remains transport boundary.
- JSONModel remains UI/edit-state owner.
- Function-import mutation flow for checklist aggregate remains canonical.
- Attachment binary transfer is no longer routed through aggregate JSON save transport.
- Backend codes and human-readable texts stay split:
  - machine-readable codes stay in [`zif_zodata_message_codes.intf.abap`](/Users/lgbtb/Desktop/ui5/backend/sap_backend/src/zif_zodata_message_codes.intf.abap)
  - human-readable texts stay in [`zcl_zodata_message_texts.clas.abap`](/Users/lgbtb/Desktop/ui5/backend/sap_backend/src/zcl_zodata_message_texts.clas.abap)

### Verification
- `node scripts/attachment-contract-gate.js`
- `node scripts/lock-contract-naming-gate.js`
- `node scripts/wrapper-sprawl-gate.js`
- `node scripts/dom-hack-gate.js`
- `node scripts/sap-internal-css-gate.js`
- `node scripts/raw-ui-text-gate.js`
- `python -m pytest backend/mock_gateway/tests/test_attachment_upload_policy.py`

### Honest Residual Risks
- Productive SAP Gateway stream handling is represented in repo contract/frontend/mock, but the real SAP system still needs the corresponding SEGW/DPC stream implementation deployed.
- Remaining internal `.sap*` CSS is now governed and documented, not eliminated. It is quarantined rather than silently spread.
- Large framework/component bootstrap fragmentation still exists outside the deleted wrapper layer. It is smaller now, but not fully collapsed in this pass.
