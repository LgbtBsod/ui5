# mock_gate_way OData V2 (Gateway-like)

Канонический service root:
`/sap/opu/odata/sap/Z_UI5_SRV/`

## Запуск
```bash
cd /workspace/ui5/mock_gate_way
uvicorn main:app --reload --host 0.0.0.0 --port 8000
```

UI5 (из корня проекта) ожидает backend по `http://localhost:8000`.
Канонические URL использовать только под `/sap/opu/odata/sap/Z_UI5_SRV/`.
Legacy alias'ы оставлены: `/ChecklistRoots`, `/SearchRows`, `/actions/*`, `/lock/*`.
- ChecklistSearchSet — канонический источник поиска; `SearchRows`/`ChecklistRoots` только migration alias.
- `$expand` в канонических collection endpoints отклоняется (VALIDATION_ERROR/EXPAND_NOT_ALLOWED).

## Canonical entity sets

- `ChecklistSearchSet`
- `ChecklistRootSet`
- `ChecklistBasicInfoSet`
- `ChecklistCheckSet`
- `ChecklistBarrierSet`
- `DictionaryItemSet`
- `LastChangeSet`
- `LockStatusSet`
- `RuntimeSettingsSet`
- `AttachmentSet`
- `AttachmentFolderSet`

## CSRF fetch
```bash
curl -i "http://localhost:8000/sap/opu/odata/sap/Z_UI5_SRV/ChecklistSearchSet?$top=1" -H "X-CSRF-Token: Fetch"
```

## Search paging/filter by DateCheck
```bash
curl "http://localhost:8000/sap/opu/odata/sap/Z_UI5_SRV/ChecklistSearchSet?$top=20&$skip=0&$inlinecount=allpages&$filter=substringof('LPC',LpcText)%20and%20Status%20eq%20'DRAFT'&$orderby=ChangedOn%20desc"
```

## Runtime settings (GLOBAL)
```bash
curl "http://localhost:8000/sap/opu/odata/sap/Z_UI5_SRV/RuntimeSettingsSet(Key='GLOBAL')"
```

## Read LastChangeSet
```bash
curl "http://localhost:8000/sap/opu/odata/sap/Z_UI5_SRV/LastChangeSet('<ROOT_KEY_HEX>')"
```

## LockStatusSet
```bash
curl "http://localhost:8000/sap/opu/odata/sap/Z_UI5_SRV/LockStatusSet('<ROOT_KEY_HEX>')?SessionGuid=<SESSION_GUID>"
```

## LockControl acquire/heartbeat/release
```bash
curl -X POST "http://localhost:8000/sap/opu/odata/sap/Z_UI5_SRV/LockControl" \
  -H "Content-Type: application/json" -H "X-CSRF-Token: <token>" -b "SAP_SESSIONID=<sid>" \
  -d '{"Action":"ACQUIRE","RootKey":"<ROOT_KEY_HEX>","SessionGuid":"<SESSION_GUID>","Uname":"TESTUSER"}'

curl -X POST "http://localhost:8000/sap/opu/odata/sap/Z_UI5_SRV/LockControl" \
  -H "Content-Type: application/json" -H "X-CSRF-Token: <token>" -b "SAP_SESSIONID=<sid>" \
  -d '{"Action":"HEARTBEAT","RootKey":"<ROOT_KEY_HEX>","SessionGuid":"<SESSION_GUID>","Uname":"TESTUSER"}'

curl -X POST "http://localhost:8000/sap/opu/odata/sap/Z_UI5_SRV/LockControl" \
  -H "Content-Type: application/json" -H "X-CSRF-Token: <token>" -b "SAP_SESSIONID=<sid>" \
  -d '{"Action":"RELEASE","RootKey":"<ROOT_KEY_HEX>","SessionGuid":"<SESSION_GUID>","Uname":"TESTUSER"}'
```

## AutoSave payload sample (unified delta)
```bash
curl -X POST "http://localhost:8000/sap/opu/odata/sap/Z_UI5_SRV/AutoSave" \
  -H "Content-Type: application/json" -H "X-CSRF-Token: <token>" -b "SAP_SESSIONID=<sid>" \
  -d '{
    "RootKey":"<ROOT_KEY_HEX>",
    "ClientAggChangedOn":"/Date(1735689600000)/",
    "Changes":[
      {"Entity":"BASIC","Key":"<ROOT_KEY_HEX>","EditMode":"U","Fields":{"LocationKey":"LOC-100","LocationName":"Area A"}},
      {"Entity":"CHECK","EditMode":"U","Key":"<CHECK_KEY_HEX>","ParentKey":"<ROOT_KEY_HEX>","Fields":{"Comment":"Updated","Result":true}}
    ]
  }'
```

## SaveChanges payload sample (full payload)
```bash
curl -X POST "http://localhost:8000/sap/opu/odata/sap/Z_UI5_SRV/SaveChanges" \
  -H "Content-Type: application/json" -H "X-CSRF-Token: <token>" -b "SAP_SESSIONID=<sid>" \
  -d '{
    "ClientAggChangedOn":"/Date(1735689600000)/",
    "root":{"Key":"<ROOT_KEY_HEX>","Status":"SUBMITTED"},
      "basic":{"LocationKey":"LOC-100","LocationName":"Area A","EquipName":"Pump A"},
      "checks":[{"ChecksNum":10,"Comment":"ok","Result":true}],
      "barriers":[{"BarriersNum":20,"Comment":"warn","Result":false}]
  }'
```

## SetChecklistStatus sample
```bash
curl -X POST "http://localhost:8000/sap/opu/odata/sap/Z_UI5_SRV/SetChecklistStatus" \
  -H "Content-Type: application/json" -H "X-CSRF-Token: <token>" -b "SAP_SESSIONID=<sid>" \
  -d '{"RootKey":"<ROOT_KEY_HEX>","NewStatus":"SUBMITTED","ClientAggChangedOn":"/Date(1735689600000)/"}'
```

## GetHierarchy sample
```bash
curl "http://localhost:8000/sap/opu/odata/sap/Z_UI5_SRV/GetHierarchy?DateCheck=datetime'2025-01-01T00:00:00'&Method=MPL"

# canonical method value:
curl "http://localhost:8000/sap/opu/odata/sap/Z_UI5_SRV/GetHierarchy?DateCheck=datetime'2025-01-01T00:00:00'&Method=location_tree"
```

## ReportExport sample
```bash
curl -X POST "http://localhost:8000/sap/opu/odata/sap/Z_UI5_SRV/ReportExport" \
  -H "Content-Type: application/json" -H "X-CSRF-Token: <token>" -b "SAP_SESSIONID=<sid>" \
  -d '{"RootKeys":["<ROOT_KEY_1>","<ROOT_KEY_2>"]}'
```

## Batch sample
```bash
curl -X POST "http://localhost:8000/sap/opu/odata/sap/Z_UI5_SRV/$batch" \
  -H "Content-Type: multipart/mixed; boundary=batch_123" -H "X-CSRF-Token: <token>" -b "SAP_SESSIONID=<sid>" \
  --data-binary $'--batch_123\r\nContent-Type: application/http\r\nContent-Transfer-Encoding: binary\r\n\r\nGET /sap/opu/odata/sap/Z_UI5_SRV/LockStatusSet(\'<ROOT_KEY_HEX>\') HTTP/1.1\r\n\r\n--batch_123--\r\n'
```
