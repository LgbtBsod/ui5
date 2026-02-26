# SLO breach handling runbook

1. Identify breached operation from SLO report (`search.execute`, `detail.save`, `dialog.workflowAnalytics.open`).
2. Correlate with deployment/build and backend mode.
3. Apply mitigation:
   - search: reduce payload, fallback to cached mode, narrow filter defaults.
   - save: disable heavy post-save hooks, verify lock/etag retries.
   - dialog: lazy-load analytics payload and defer non-critical cards.
4. Open incident note with timestamp, p95 values, and owner.
5. Add corrective action to next sprint and keep temporary guardrail until resolved.
