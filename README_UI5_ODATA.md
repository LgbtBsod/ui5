# UI5 OData V2 runtime notes

## Dev proxy
Use UI5 dev server/proxy so all requests go to relative service root:

- OData service URL: `/sap/opu/odata/sap/Z_UI5_SRV/`
- Ensure local proxy forwards `/sap/opu/odata/sap/Z_UI5_SRV/*` to FastAPI backend.

Example (ui5.yaml custom middleware) should proxy the `/sap` path to backend host/port.

## How to test Function Imports from UI
1. Open app and login test user.
2. Open detail in edit mode:
   - UI calls `LockAcquire` FI.
3. Keep object open > heartbeat interval:
   - UI calls `LockHeartbeat` FI every 4 minutes.
4. Change fields:
   - AutoSave sends delta via `AutoSave` FI (autosave group).
5. Press Save:
   - Full save calls `SaveChanges` FI and resets GCD timer.
6. Leave detail or close tab:
   - UI calls `LockRelease` FI (`try_save=true` for unload beacon payload).
7. MPL usage:
   - Location tree loads via `MplTree` FI and is cached in `mpl` model per tab lifetime.
