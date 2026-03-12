# Constants Governance

## Keep

- `C:\Users\lgbtb\Desktop\ui5\app\model\StatePaths.js`
  - Canonical source for state-model paths.
- `C:\Users\lgbtb\Desktop\ui5\app\service\domain\shared\ViewPathContracts.js`
  - Canonical source for view-model paths.
- `C:\Users\lgbtb\Desktop\ui5\app\util\runtime\FrontendConfigConstants.js`
  - Canonical source for normalized runtime/config values.
- `C:\Users\lgbtb\Desktop\ui5\app\controller\support\DetailActionConstants.js`
  - Keep as detail-local feature constants.
- `C:\Users\lgbtb\Desktop\ui5\app\service\framework\UiBehaviorConstants.js`
  - Keep as behavior-operation contract.
- `C:\Users\lgbtb\Desktop\ui5\app\service\framework\DetailRuntimeConstants.js`
  - Keep as detail-runtime semantic contract.

## Merge

- `C:\Users\lgbtb\Desktop\ui5\app\service\domain\shared\ModelPathContracts.js`
  - Merge into `StatePaths` by reference; stop duplicating path literals.
- Navigation route/layout/page ids
  - Canonical source: `C:\Users\lgbtb\Desktop\ui5\app\service\contracts\NavigationContracts.js`
- Dialog ids and fragment names
  - Canonical source: `C:\Users\lgbtb\Desktop\ui5\app\service\contracts\DialogContracts.js`
- Analytics presets/source/status/builder text-key maps
  - Canonical source: `C:\Users\lgbtb\Desktop\ui5\app\service\contracts\AnalyticsContracts.js`

## Drop

- Inline route names such as `"search"`, `"analytics"`, `"detail"`, `"detailLayout"`
- Inline layout literals such as `"MidColumnFullScreen"` and `"OneColumn"`
- Inline fragment names such as `PRODUCTION_CONTROL_CHECKLIST.view.fragment.AnalyticsReportDialog`
- Inline analytics task/source/status literals such as `"ANALYTICS_REFRESH"`, `"WEB"`, `"REQUESTED"`, `"RUNNING"`

## Rule

- User-facing text belongs in i18n.
- Repeated code literals belong in the nearest canonical contract module.
- Feature-local constants stay local unless reused across modules.
