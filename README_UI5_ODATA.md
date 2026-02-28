# UI5 + FastAPI Gateway-like OData V2 (1.71 compatible)

## Canonical OData root
- Service root: `/sap/opu/odata/sap/Z_UI5_SRV/`
- Metadata: `/sap/opu/odata/sap/Z_UI5_SRV/$metadata`
- Batch: `/sap/opu/odata/sap/Z_UI5_SRV/$batch`

Deprecated aliases are kept for migration (`/ChecklistRoots`, `/SearchRows`, `/actions/*`, `/lock/*`).

- ChecklistSearchSet is the canonical search entity set; `SearchRows`/`ChecklistRoots` are migration aliases only.
- `$expand` is rejected for search/checks/barriers/attachments collections in canonical routes.

### Legacy lock aliases (migration-safe)
Legacy clients may still use `/lock/*`; backend now forwards these aliases to canonical lock semantics:
- `POST /lock/acquire` -> `LockControl(Action=ACQUIRE)`
- `POST /lock/heartbeat` -> `LockControl(Action=HEARTBEAT)`
- `POST /lock/release` -> `LockControl(Action=RELEASE)`
- `GET /lock/status?object_uuid=<RootKey>&session_guid=<SessionGuid>` -> canonical lock status mapping

Service-root alias variants are also available for proxy-only landscapes:
- `/sap/opu/odata/sap/Z_UI5_SRV/lock/acquire`
- `/sap/opu/odata/sap/Z_UI5_SRV/lock/heartbeat`
- `/sap/opu/odata/sap/Z_UI5_SRV/lock/release`
- `/sap/opu/odata/sap/Z_UI5_SRV/lock/status`

## Run backend
```bash
cd mock_gate_way
uvicorn main:app --reload --host 0.0.0.0 --port 8000
```

## UI proxy (UI5 tooling)
Proxy `/sap` to backend `http://localhost:8000` so UI can call relative OData URLs.

UI5 proxy example (`ui5.yaml` customMiddleware):
```yaml
server:
  customMiddleware:
    - name: ui5-middleware-simpleproxy
      mountPath: /sap
      configuration:
        baseUri: http://localhost:8000/sap
```

Runtime timers are read from `/sap/opu/odata/sap/Z_UI5_SRV/config/frontend`; backend now mirrors them in both `timers` and `variables` (including `cacheToleranceMs`) for migration-safe clients.

## CSRF
- Fetch token:
```bash
curl -i -H 'X-CSRF-Token: Fetch' http://localhost:8000/sap/opu/odata/sap/Z_UI5_SRV/ChecklistSearchSet
```
- Use returned `X-CSRF-Token` + cookie for all writes (`POST/PUT/DELETE`, including `$batch` changeset).

## cURL examples

### Search with paging/filter by DateCheck
```bash
curl -G 'http://localhost:8000/sap/opu/odata/sap/Z_UI5_SRV/ChecklistSearchSet' \
  --data-urlencode "$top=20" \
  --data-urlencode "$skip=0" \
  --data-urlencode "$inlinecount=allpages" \
  --data-urlencode "$select=Key,Id,DateCheck,Lpc,Profession,Status" \
  --data-urlencode "$filter=DateCheck ge datetime'2025-01-01T00:00:00'"
```

### LastChangeSet (cache-check source)
```bash
curl 'http://localhost:8000/sap/opu/odata/sap/Z_UI5_SRV/LastChangeSet('<ROOTKEY_HEX32>')'
```
UI cache reuse threshold is `15s` (`timers.cacheToleranceMs`).

### LastChangeSet collection
```bash
curl -G "http://localhost:8000/sap/opu/odata/sap/Z_UI5_SRV/LastChangeSet" \
  --data-urlencode "$top=20" \
  --data-urlencode "$skip=0" \
  --data-urlencode "$inlinecount=allpages"
```

### LockStatusSet
```bash
curl "http://localhost:8000/sap/opu/odata/sap/Z_UI5_SRV/LockStatusSet('<ROOTKEY_HEX32>')?SessionGuid=sess-1&Uname=DEMO"
```

### LockStatusSet collection
```bash
curl -G "http://localhost:8000/sap/opu/odata/sap/Z_UI5_SRV/LockStatusSet" \
  --data-urlencode "$filter=RootKey eq '<ROOTKEY_HEX32>'"
```

### LockControl acquire / heartbeat / release
```bash
curl -X POST 'http://localhost:8000/sap/opu/odata/sap/Z_UI5_SRV/LockControl' \
  -H 'Content-Type: application/json' \
  -d '{"Action":"ACQUIRE","RootKey":"<ROOTKEY_HEX32>","SessionGuid":"sess-1","Uname":"DEMO"}'

curl -X POST 'http://localhost:8000/sap/opu/odata/sap/Z_UI5_SRV/LockControl' \
  -H 'Content-Type: application/json' \
  -d '{"Action":"HEARTBEAT","RootKey":"<ROOTKEY_HEX32>","SessionGuid":"sess-1","Uname":"DEMO"}'

curl -X POST 'http://localhost:8000/sap/opu/odata/sap/Z_UI5_SRV/LockControl' \
  -H 'Content-Type: application/json' \
  -d '{"Action":"RELEASE","RootKey":"<ROOTKEY_HEX32>","SessionGuid":"sess-1","Uname":"DEMO"}'
```

### AutoSave (unified delta)
```bash
curl -X POST 'http://localhost:8000/sap/opu/odata/sap/Z_UI5_SRV/AutoSave' \
  -H 'Content-Type: application/json' \
  -d '{
    "RootKey":"<ROOTKEY_HEX32>",
    "ClientAggChangedOn":"/Date(1735689600000)/",
    "Changes":[
      {"Entity":"BASIC","Key":"<ROOTKEY_HEX32>","EditMode":"U","Fields":{"LocationKey":"LOC-100"}},
      {"Entity":"CHECK","Key":"<CHECKKEY_HEX32>","EditMode":"U","Fields":{"Comment":"updated","Result":true}}
    ]
  }'
```

### SaveChanges (full payload)
```bash
curl -X POST 'http://localhost:8000/sap/opu/odata/sap/Z_UI5_SRV/SaveChanges' \
  -H 'Content-Type: application/json' \
  -d '{
    "RootKey":"<ROOTKEY_HEX32>",
    "ClientAggChangedOn":"/Date(1735689600000)/",
    "FullPayload":{
      "root":{"Status":"02"},
      "basic":{"LocationKey":"LOC-100","EquipName":"Pump A"},
      "checks":[{"ChecksNum":10,"Comment":"ok","Result":true}],
      "barriers":[{"BarriersNum":20,"Comment":"warn","Result":false}]
    }
  }'
```

### SetChecklistStatus
```bash
curl -X POST 'http://localhost:8000/sap/opu/odata/sap/Z_UI5_SRV/SetChecklistStatus' \
  -H 'Content-Type: application/json' \
  -d '{"RootKey":"<ROOTKEY_HEX32>","NewStatus":"03","ClientAggChangedOn":"/Date(1735689600000)/"}'
```

### GetHierarchy
```bash
curl -G 'http://localhost:8000/sap/opu/odata/sap/Z_UI5_SRV/GetHierarchy' \
  --data-urlencode 'DateCheck=2025-01-01' \
  --data-urlencode 'Method=LOCATION'

curl -X POST 'http://localhost:8000/sap/opu/odata/sap/Z_UI5_SRV/GetHierarchy' \
  -H 'Content-Type: application/json' \
  -d '{"DateCheck":"/Date(1735689600000)/","Method":"MPL"}'
```

### ReportExport
```bash
curl -X POST 'http://localhost:8000/sap/opu/odata/sap/Z_UI5_SRV/ReportExport' \
  -H 'Content-Type: application/json' \
  -d '{"RootKeys":["<ROOTKEY_HEX32>","<ROOTKEY_HEX32_2>"]}'
```
