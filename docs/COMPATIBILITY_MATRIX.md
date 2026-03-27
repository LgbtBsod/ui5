# Compatibility Matrix

## Runtime Baseline

| Area | Canonical source | Notes |
| --- | --- | --- |
| UI5 metadata contract | `app/localService/metadata.xml` | Frontend local metadata and mock Gateway metadata must stay byte-compatible for active entities and function imports. |
| Productive Gateway contract | `backend/sap_backend/src/zcl_zodata_mpc_ext.clas.abap` and `backend/sap_backend/src/zcl_zodata_dpc_ext.clas.abap` | Productive SAP implementation is the release source of truth. |
| Mock Gateway contract surface | `backend/mock_gateway/api/gateway_canonical_api.py` | Must expose the same active entity sets and function imports used by the frontend. |
| Frontend OData consumption | `app/constants/GatewayContractConstants.js` and `app/service/backend/GatewayClient.js` | Non-canonical alias paths are forbidden. |

## Browser Support

| Browser / device | Status | Notes |
| --- | --- | --- |
| Microsoft Edge (evergreen, desktop) | Supported | Formal production target. |
| Internet Explorer | Not supported | No compatibility obligations. |
| Tablet browsers | Best effort only | Not release-blocking. |
| Phone browsers | Best effort only | Not release-blocking. |

## Contract Parity Rules

1. `app/localService/metadata.xml` and mock Gateway `$metadata` must agree on active entity types, entity sets, and function imports.
2. Frontend state aliases may exist only as internal mapping helpers; UI bindings and tests must prefer canonical fields.
3. Legacy action routes such as `/actions/*`, `/lock/*`, or `/config/frontend` must not reappear.
4. Save and lock flows must agree across frontend, mock Gateway, and SAP backend on `DB_KEY`, `SessionGuid`, `ClientVersion`, and returned lock metadata.
5. Attachment save semantics are canonical only through `DeltaPayloadBuilder` payloads; payload mappers may normalize rows but must not introduce a second attachment mutation source.
6. Renderer-coupled CSS is limited to a reviewed UI5 1.71 compatibility allowlist. Dialog and ObjectPage overrides must be scoped through app-owned host classes where possible.

## Verification Commands

- `cmd /c npm.cmd run lint:js`
- `cmd /c npm.cmd run validate:local`
- `python -m pytest tests -q` from `backend/mock_gateway`
