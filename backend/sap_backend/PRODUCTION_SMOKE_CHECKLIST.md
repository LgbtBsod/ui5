# Production Smoke Checklist

Run this checklist on the real Gateway / ABAP contour after deployment.

Preferred automated entry point for QA Gateway evidence:

- `npm run smoke:gateway:live-evidence`
- review `docs/artifacts/gateway-live-evidence-summary.json`
- validate it with `npm run gate:gateway-live-evidence`

## Local Pre-Flight Baseline

- [ ] Start local static runtime and run `python scripts/interaction-smoke.py http://127.0.0.1:8080/index.html`.
- [ ] Record `resultClass` from the JSON output:
  - `PASS_LOCAL_BASELINE` means local UI/runtime contract is healthy.
  - `BLOCKED_BACKEND` means local UI shell is healthy but static localhost cannot prove Gateway-dependent flows.
  - `FAIL_UI_CONTRACT` means the repo has a real local runtime regression and release must stop.
- [ ] Record backend blocker URLs and statuses when `resultClass=BLOCKED_BACKEND`; they are environment evidence, not product failure.
- [ ] Treat local baseline as pre-flight only; it does not replace SAP / FLP evidence below.

- [ ] App startup succeeds from FLP / productive URL without console bootstrap errors.
- [ ] Search loads results and respects filters / paging.
- [ ] Open detail from search and confirm FCL navigation state is correct.
- [ ] Edit and save a checklist successfully.
- [ ] Lock acquire, heartbeat, and release complete without ownership drift.
- [ ] Attachment add/view/remove flow works end-to-end.
- [ ] Analytics load succeeds for the default year/source selection.
- [ ] Analytics export produces a spreadsheet file.
- [ ] Search export produces a spreadsheet file from both selected rows and full-result mode.
- [ ] Shell refresh of current user / permissions / runtime settings works after startup.
- [ ] Session timeout and CSRF refresh recover gracefully.
- [ ] Two-tab conflict scenario shows correct lock/concurrency behavior.
- [ ] FCL navigation between search/detail/analytics remains stable.
- [ ] Role-based visibility and permissions are correct for at least one read-only and one edit-capable user.

## Suggested evidence capture

- [ ] Startup screenshot or FLP launch proof.
- [ ] Search request/response sample.
- [ ] Save response sample with version / timestamp change.
- [ ] Lock lifecycle proof with acquire/heartbeat/release timestamps.
- [ ] Attachment proof.
- [ ] Analytics load + export proof.
- [ ] Search export proof.
- [ ] Permission visibility proof for both user roles.
