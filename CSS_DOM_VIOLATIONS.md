# CSS/DOM Violations

- Reduced private UI5 selector usage in search/detail/analytics/shell styling surfaces.
- Remaining private selector usage is intentionally whitelisted only where UI5 1.71 lacks a stable public alternative.
- The allowlist was tightened to remove stale CSS exceptions that are no longer needed.
