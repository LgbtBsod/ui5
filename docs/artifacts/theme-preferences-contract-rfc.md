# RFC: `/user/theme-preferences` Contract (Capability-gated)

## Purpose
Define a stable backend contract for user-level theme persistence that can be enabled only when capability `themePreferences` is available.

## Capability Flag
- Feature key: `themePreferences`
- Location: backend capabilities payload `features.themePreferences`
- Client behavior:
  - `true`: enable preference read/write flow.
  - `false` or missing: keep local-only theme mode and skip persistence calls.

## Endpoint
`/user/theme-preferences`

### GET
Returns current user preference snapshot.

Response example:
```json
{
  "theme": "morning",
  "motion": "standard",
  "updatedAt": "2026-02-26T00:00:00Z"
}
```

### PUT
Upserts theme preference for current user.

Request example:
```json
{
  "theme": "night",
  "motion": "reduced"
}
```

Response example:
```json
{
  "ok": true,
  "theme": "night",
  "motion": "reduced",
  "updatedAt": "2026-02-26T00:00:00Z"
}
```

## Error Semantics
- `400`: invalid enum or malformed payload.
- `401/403`: unauthorized.
- `409`: optimistic-lock conflict (optional ETag profile).
- `503`: temporary backend unavailability.

## Compatibility Notes
- Backward compatible with clients that do not request this endpoint.
- UI must not block rendering if call fails; fallback to local preference and show non-blocking info toast.
