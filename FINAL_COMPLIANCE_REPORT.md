# Final Compliance Report

## Closed Gaps
- Attachment productive flow now prefers `DownloadUrl` / `DocumentHandle`; `Value` is no longer part of the default attachment contract or open path.
- App-side canonical key model is now `DB_KEY` for root identity and `PARENT_KEY` for child relations.
- Mock gateway metadata/API/test surface now follows the same canonical `DB_KEY` / `PARENT_KEY` model on the productive path.
- `RootId` no longer propagates through `WorkspaceRouteNavigation` analytics snapshots as a productive navigation field; `activeObjectId` / `selectedId` are the working keys.
- Frontend message-key ownership remains in `MessageKeyConstants.js`.
- Frontend machine-readable code ownership remains in `MessageCodeConstants.js`.
- Backend human-readable texts are isolated in `zif_zodata_message_texts.intf.abap`.
- Backend machine-readable codes remain in `zif_zodata_message_codes.intf.abap`.
- Shell message-key ownership was merged into the canonical frontend message-key owner.
- `ComponentBootRuntime`, `ComponentListenerBindingRuntime`, and `ComponentNavigationGuardRuntime` were removed as thin wrappers.
- Residual cleanup gates now pass on the current repository state.

## Remaining Risk
- Some `RootId` compatibility usage still exists in boundary-adjacent runtime/model plumbing and should remain compatibility-only.
- Some backend/mock transport seams still accept legacy aliases at the ingress boundary, but productive contracts now prefer `DB_KEY` / `PARENT_KEY`.
- Component/runtime and detail controller clusters are smaller, but not fully collapsed to a single-file normal form.
- UI5 internal selector usage is reduced but not eliminated across the whole repo.
