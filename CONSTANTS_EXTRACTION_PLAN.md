# Constants Extraction Plan

## Existing good anchors
- `app/constants/ModelConstants.js`
- `app/constants/NavigationContracts.js`
- `app/constants/GatewayContractConstants.js`
- `app/constants/WorkflowContracts.js`
- `app/constants/DetailContracts.js`
- `app/constants/SearchContracts.js`
- `app/constants/UiControlIds.js`

## Raw technical strings to extract next

### Identity and payload mapping
- repeated alias field names for root/parent/attachment/file metadata have been moved into `app/infra/adapters/shared/ODataEntityContracts.js`
- follow-up cleanup should reuse `ODataEntityContracts.IDENTITY` instead of adding new local alias arrays
- remove any remaining duplicate field-name arrays if they still appear outside the current payload/snapshot/delta owner files

### Detail/UI ownership
- attachment/drop zone class names used across detail controller files
- repeated detail command names and effect ids still outside canonical contracts
- repeated view model paths that are used in more than one detail runtime/controller file

### Search
- search-related app-owned CSS classes repeated in controller/runtime interaction points
- repeated technical strings for selection/viewport runtime paths and operation sources

### CSS
- app-owned CSS class names should be centralized where reused in JS and XML
- only app-owned class names, not SAP internal selectors

## Do not extract
- backend-driven runtime values from `RuntimeSettingsSet`
- i18n texts
- one-off local business labels

## Target constants grouping
- keep using existing semantic modules rather than creating many micro-files
- prefer extending:
  - `DetailContracts.js`
  - `SearchContracts.js`
  - `NavigationContracts.js`
  - `GatewayContractConstants.js`
  - `UiControlIds.js`

## Cleanup after extraction
- remove duplicated raw strings from controllers/runtimes
- remove stale parallel constants if same meaning already exists in `app/constants/*`
