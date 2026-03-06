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
- ChecklistSearchSet — канонический источник поиска.
- `$expand` в канонических collection endpoints отклоняется (VALIDATION_ERROR/EXPAND_NOT_ALLOWED).

## Canonical entity sets

- `ChecklistSearchSet`
- `ChecklistRootSet`
- `ChecklistBasicInfoSet`
- `ChecklistCheckSet`
- `ChecklistBarrierSet`
- `DictionaryItemSet`
- `PersonVHSet`
- `LastChangeSet`
- `LockStatusSet`
- `ChecklistPermissionSet`
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

## Dictionary bootstrap in one read
```bash
curl "http://localhost:8000/sap/opu/odata/sap/Z_UI5_SRV/DictionaryItemSet?$orderby=Domain%20asc,Key%20asc&$top=2000"
```

`DictionaryItemSet` должен читаться одним запросом без доменных фильтров. Backend возвращает:
- обычные справочники (`LPC`, `PROFESSION`, `TIME_ZONE`, `ATF_CAT`)
- обязательные поля карточки в домене `WEB_REQUIRED_FIELD`
- web-переменные в домене `WEB_VARIABLE`

## Read LastChangeSet
```bash
curl "http://localhost:8000/sap/opu/odata/sap/Z_UI5_SRV/LastChangeSet('<ROOT_KEY_HEX>')"
```

## LockStatusSet
```bash
curl "http://localhost:8000/sap/opu/odata/sap/Z_UI5_SRV/LockStatusSet('<ROOT_KEY_HEX>')?SessionGuid=<SESSION_GUID>"
```

## ChecklistPermissionSet
```bash
curl "http://localhost:8000/sap/opu/odata/sap/Z_UI5_SRV/ChecklistPermissionSet('<ROOT_KEY_HEX>')?Uname=demoUser"
```

Для mock Gateway deny-сценарии можно воспроизводить через `Uname`:
- `noview_*` / `denyall_*` -> нет прав на открытие карточки
- `readonly_*` / `noedit_*` -> только read-only, edit запрещён
- `nodelete_*` -> delete запрещён

SAP-модель операций в entity:
- `ViewOperation=01` -> просмотр
- `ChangeOperation=02` -> создание/обновление
- `DeleteOperation=03` -> удаление
- `GrantedOperations` -> CSV со списком реально выданных операций для объекта полномочий

## LockAcquire / LockHeartbeat / LockRelease
```bash
curl -X POST "http://localhost:8000/sap/opu/odata/sap/Z_UI5_SRV/LockAcquire?RootId=<ROOT_KEY_HEX>&SessionGuid=<SESSION_GUID>&Uname=TESTUSER" \
  -H "X-CSRF-Token: <token>" -H "X-Requested-With: XMLHttpRequest" -b "SAP_SESSIONID=<sid>"

curl -X POST "http://localhost:8000/sap/opu/odata/sap/Z_UI5_SRV/LockHeartbeat?RootId=<ROOT_KEY_HEX>&SessionGuid=<SESSION_GUID>&Uname=TESTUSER" \
  -H "X-CSRF-Token: <token>" -H "X-Requested-With: XMLHttpRequest" -b "SAP_SESSIONID=<sid>"

curl -X POST "http://localhost:8000/sap/opu/odata/sap/Z_UI5_SRV/LockRelease?RootId=<ROOT_KEY_HEX>&SessionGuid=<SESSION_GUID>&Uname=TESTUSER" \
  -H "X-CSRF-Token: <token>" -H "X-Requested-With: XMLHttpRequest" -b "SAP_SESSIONID=<sid>"
```

## CopyChecklist sample
```bash
curl -X POST "http://localhost:8000/sap/opu/odata/sap/Z_UI5_SRV/CopyChecklist?RootId=<ROOT_KEY_HEX>&SessionGuid=<SESSION_GUID>&Uname=TESTUSER" \
  -H "X-CSRF-Token: <token>" -H "X-Requested-With: XMLHttpRequest" -b "SAP_SESSIONID=<sid>"
```

`CopyChecklist` создаёт новый persisted объект на backend, не копирует вложения, обновляет дату базовой информации на текущую и сразу ставит обычный lock в таблицу блокировок на переданную `SessionGuid`.

## AutoSave payload sample (SaveChangesRequest deep delta)
```bash
curl -X POST "http://localhost:8000/sap/opu/odata/sap/Z_UI5_SRV/AutoSave" \
  -H "Content-Type: application/json" -H "X-CSRF-Token: <token>" -b "SAP_SESSIONID=<sid>" \
  -d '{
    "root":{"pcct_uuid":"<ROOT_KEY_HEX>"},
    "checks":[
      {"check_uuid":"<CHECK_KEY_HEX>","edit_mode":"U","comment":"Updated","result":true},
      {"client_row_id":"<TEMP_ROW_UUID>","edit_mode":"C","checks_num":20,"text":"New row from client","result":false}
    ],
    "barriers":[],
    "client_version":3,
    "SessionGuid":"<SESSION_GUID>"
  }'
```

## SaveChanges payload sample (SaveChangesRequest deep delta)
```bash
curl -X POST "http://localhost:8000/sap/opu/odata/sap/Z_UI5_SRV/SaveChanges" \
  -H "Content-Type: application/json" -H "X-CSRF-Token: <token>" -b "SAP_SESSIONID=<sid>" \
  -d '{
    "root":{"pcct_uuid":"<ROOT_KEY_HEX>","status":"SUBMITTED","equipment":"Pump A","location_key":"LOC-100","location_name":"Area A"},
    "checks":[{"check_uuid":"<CHECK_KEY_HEX>","edit_mode":"U","checks_num":10,"comment":"ok","result":true}],
    "barriers":[{"barrier_uuid":"<BARRIER_KEY_HEX>","edit_mode":"U","barriers_num":20,"comment":"warn","result":false}],
    "client_version":3,
    "SessionGuid":"<SESSION_GUID>"
  }'
```

`SaveChanges` / `AutoSave` требуют активный lock этой же `SessionGuid`. Для новых child rows фронт шлёт только временный `client_row_id`; финальный key генерирует backend.

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
