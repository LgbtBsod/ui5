# Production cleanup audit

## 1) Candidate files/folders for removal (before execution)

### A. Views/Controllers not registered in routing
- **Result:** no extra top-level views/controllers detected for deletion.
- Routing uses `search`, `detail`, `detailLayout`; active view/controller set is:
  - `view/App.view.xml` + `controller/App.controller.js`
  - `view/Search.view.xml` + `controller/Search.controller.js`
  - `view/Detail.view.xml` + `controller/Detail.controller.js`
- Fragment files are referenced from Search/Detail controllers and views.

### B. Tests (`webapp/test`)
- `webapp/test` directory is absent in this repository, so there is nothing to clean there.

### C. Runtime/temp artifacts (recommended for removal)
- `tmp_ui5.log`
- `tmp_ui5.err.log`
- `tmp_ui_attachment.txt`
- `docs/runtime/tmp_ui_attachment.txt`
- `tmp_frames/sap_ui5_startup_001.png` … `tmp_frames/sap_ui5_startup_006.png`
- `gateway.db`

> These look like generated runtime artifacts and local state, not source-of-truth project code.

## 2) package.json dependencies audit
- `dependencies`: empty.
- `devDependencies`: build/qa tooling (`husky`, `lint-staged`, `stylelint`, `stylelint-config-standard`) already correctly placed.
- No dependency migration required.

## 3) manifest.json audit
- `sap.ui5/dependencies/libs`: all declared libs are used in XML views/fragments (`sap.m`, `sap.ui.core`, `sap.ui.unified`, `sap.f`, `sap.ui.layout`, `sap.ui.comp`, `sap.ui.table`, `sap.uxap`).
- Removed unused models from manifest:
  - `data`
  - `mpl`

## 4) JS cleanup
- Replaced garbled hardcoded error message in `Component.js` with readable Russian text.
- Removed dead model wiring in `Component.js` for unused models (`data`, `hierarchy`, `mpl`).
- Replaced console fallback logger in `util/DebugLogger.js` with `sap/base/Log`.

## 5) Git ignore policy
- Updated `.gitignore` for production hygiene to exclude:
  - `dist/`
  - `node_modules/`
  - `.vscode/`
  - local logs (`*.log`, temp log patterns) and temp frame folder.
