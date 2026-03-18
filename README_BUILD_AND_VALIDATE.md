# Build and Validate

## Productive UI5 baseline

- Productive target baseline: `UI5 1.71.28`.
- `app/manifest.json` is aligned to `1.71.28` for the productive support contract.
- `ui5.yaml` intentionally stays on `1.71.70` for local UI5 Tooling resolution because `@sapui5/distribution-metadata@1.71.28` is not currently resolvable in this environment.
- Test harness bootstrap pages and the local bootstrap helper still point to `https://ui5.sap.com/1.71.70/resources/sap-ui-core.js` because that contour URL is externally constrained for the current sandbox/test setup.
- Do **not** change those test bootstrap URLs as part of normal productive hardening; treat them as temporary harness skew and verify productive compatibility against the 1.71.28 contract.

## Install

```sh
npm install
```

## Build

```sh
npm run build
npm run build:dist
npm run build:preload
```

- `npm run build` delegates to the single dist build path.
- Dist output is written to `dist/`.
- `Component-preload.js` and related preload assets should be generated under `dist/` after `npm run build:dist` or `npm run build:preload`.

## Validate

```sh
npm run validate
npm run lint:css
```

- `npm run validate` runs the EOL check, CSS lint, and preload build in one pass.
- If you need a narrower check during local iteration, run `npm run lint:css` and `npm run build:preload` separately.
