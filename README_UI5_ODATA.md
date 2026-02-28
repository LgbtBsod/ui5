# UI5 + FastAPI Gateway-like OData V2 (1.71 compatible)

## Canonical OData root
- Service root: `/sap/opu/odata/sap/Z_UI5_SRV/`
- Metadata: `/sap/opu/odata/sap/Z_UI5_SRV/$metadata`
- Batch: `/sap/opu/odata/sap/Z_UI5_SRV/$batch`

Deprecated aliases are kept for migration (`/ChecklistRoots`, `/SearchRows`, `/actions/*`, `/lock/*`).

## Run backend
```bash
cd mock_gate_way
uvicorn main:app --reload --host 0.0.0.0 --port 8000
```

## UI proxy (UI5 tooling)
Proxy `/sap` to backend `http://localhost:8000` so UI can call relative OData URLs.

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
  --data-urlencode "$filter=DateCheck ge datetime'2025-01-01T00:00:00'"
```

### LastChangeSet (cache-check source)
```bash
curl 'http://localhost:8000/sap/opu/odata/sap/Z_UI5_SRV/LastChangeSet('<ROOTKEY_HEX32>')'
```

### LockStatusSet
```bash
curl "http://localhost:8000/sap/opu/odata/sap/Z_UI5_SRV/LockStatusSet('<ROOTKEY_HEX32>')?SessionGuid=sess-1&Uname=DEMO"
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
```

### ReportExport
```bash
curl -X POST 'http://localhost:8000/sap/opu/odata/sap/Z_UI5_SRV/ReportExport' \
  -H 'Content-Type: application/json' \
  -d '{"RootKeys":["<ROOTKEY_HEX32>","<ROOTKEY_HEX32_2>"]}'
```
