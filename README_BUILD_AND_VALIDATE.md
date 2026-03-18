# Build and Validate

## Productive baseline
- Productive SAPUI5 baseline: `1.71.28`
- Productive stack target: SAP BASIS `750 SP15`, SAP HANA `2 SP6`
- OData contract: `v2`

## Local tooling and version skew
- `app/manifest.json` is aligned to the productive minimum baseline `1.71.28`.
- `ui5.yaml` intentionally stays on `1.71.70` for local UI5 Tooling compatibility in this repository.
- Test/bootstrap core URLs are intentionally not changed here.
- Productive regression must be confirmed on the real `1.71.28` SAP contour, not only through local tooling.

## Commands
```bash
npm install
npm run build
npm run build:dist
npm run validate
```

## Expected output
- Build output is generated in `dist/`.
- UI5 Tooling generates component preload assets during `build:dist` / `build:preload`.
- `validate` runs EOL check, CSS linting, and preload/build verification.

## Manual post-build verification
1. App startup
2. Search
3. Detail open/edit/save
4. Analytics load
5. Analytics export
6. Search export

## Constraint
- Test bootstrap URL is intentionally unchanged in this repository.
