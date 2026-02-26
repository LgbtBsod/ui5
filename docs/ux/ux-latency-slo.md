# UX latency SLO + telemetry (C3)

## SLO targets
- search.execute p95 <= 1200ms (warn > 1000ms, fail > 1200ms)
- detail.save p95 <= 1500ms (warn > 1200ms, fail > 1500ms)
- dialog.workflowAnalytics.open p95 <= 400ms (warn > 300ms, fail > 400ms)

## Instrumentation
- Client timing events are recorded with operation tags in `util/UxTelemetry.js`.
- Search and Detail flows emit telemetry into `state>/uxTelemetry/events`.

## Reporting
Run:
`node scripts/ux-slo-report.js`

Optional custom input:
`node scripts/ux-slo-report.js --input /path/to/telemetry.json`
