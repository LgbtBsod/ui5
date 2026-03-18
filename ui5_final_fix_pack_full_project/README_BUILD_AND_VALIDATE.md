# Build and Validate

## Productive baseline
- Productive SAPUI5 target: **1.71.28**
- Local/test bootstrap URL may still point to **1.71.70** because the test harness URL is intentionally not changed in this repository.
- Treat **1.71.28** as the minimum supported productive baseline and regression-test critical flows on the real SAP landscape.

## Local commands
```bash
npm install
npm run build
npm run build:dist
npm run validate
```

## Expected output
- `npm run build` and `npm run build:dist` create the `dist/` folder
- Component preload is generated through UI5 Tooling
- `npm run validate` runs CSS linting and XML view checks

## Productive verification
After building, verify on the real SAP stack:
1. app startup
2. search
3. detail open/edit/save
4. attachment flow
5. analytics load and export
6. lock acquire / heartbeat / release
7. session timeout / CSRF refresh
8. routing/FCL flows
