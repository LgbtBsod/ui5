# Project Structure

- `app/`
  UI5 runtime root. Contains `index.html`, `Component.js`, `manifest.json`, preload, controllers, views, services, styles, i18n, and all app-facing modules.

- `backend/`
  Backend-side assets and mock infrastructure.
  `backend/mock_gateway/` contains the local SAP Gateway mock.
  `backend/sap_backend/` contains backend reference material that should stay isolated from frontend runtime code.

- `scripts/`
  Build, smoke, audit, and local environment tooling.

- `docs/`
  Architecture notes, runtime logs, QA evidence, and captured artifacts.

- `architecture/`
  Experimental architecture tooling and analysis flows.

- `udos/`
  UDOS-related governance, evolution, and court tooling.

- Root files
  Only workspace-level control files stay at the repository root: `package.json`, lockfile, `start.bat`, `.gitignore`, style config, and high-level docs/tooling folders.
