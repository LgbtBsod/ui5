# UI5 + FastAPI SAP Gateway-like mock (OData V2)

Этот проект поднимает UI5 frontend и mock backend, который имитирует SAP Gateway сервис:

- Канон: `/sap/opu/odata/sap/Z_UI5_SRV/`
- Формат: OData V2 JSON verbose (`d/results/__count`)
- DateTime: SAP-стиль `/Date(<ms>)/` (UTC)
- Основной список поиска: `ChecklistSearchSet`
- `$expand` для checks/barriers/attachments не используется

## Быстрый запуск

### 1) Backend (FastAPI)

```bash
cd mock_gate_way
python -m venv .venv
source .venv/bin/activate
pip install -r requirements.txt
uvicorn main:app --host 0.0.0.0 --port 8000 --reload
```

Backend URL: `http://localhost:8000`

### 2) Frontend (UI5)

В `manifest.json` datasource уже смотрит на:

- `/sap/opu/odata/sap/Z_UI5_SRV/`

Для локального запуска UI5 используйте proxy (например ui5.yaml / approuter) чтобы `/sap/opu/odata/*` проксировался на `http://localhost:8000`.

Пример правила прокси (идея):

- source: `/sap`
- target: `http://localhost:8000/sap`

## Канонические endpoint'ы

- Service document: `GET /sap/opu/odata/sap/Z_UI5_SRV/`
- Metadata: `GET /sap/opu/odata/sap/Z_UI5_SRV/$metadata`
- Batch: `POST /sap/opu/odata/sap/Z_UI5_SRV/$batch`

### EntitySets

- `ChecklistSearchSet`
- `ChecklistRootSet`
- `ChecklistBasicInfoSet`
- `ChecklistCheckSet`
- `ChecklistBarrierSet`
- `DictionaryItemSet`
- `LastChangeSet`
- `LockStatusSet`
- `AttachmentFolderSet`
- `AttachmentSet`
- `RuntimeSettingsSet`

### Function imports

- `LockControl`
- `AutoSave`
- `SaveChanges`
- `SetChecklistStatus`
- `GetHierarchy`
- `ReportExport`

## CSRF

Для write-операций требуется CSRF.

1. Сделать `GET` с `X-CSRF-Token: Fetch`
2. Взять `X-CSRF-Token` из ответа и cookie `SAP_SESSIONID`
3. Передавать токен в `POST/PUT/DELETE/$batch`

## Примеры curl

> Ниже используется `BASE=http://localhost:8000/sap/opu/odata/sap/Z_UI5_SRV`

```bash
BASE='http://localhost:8000/sap/opu/odata/sap/Z_UI5_SRV'
```

### 1) Search filter by DateCheck + ProfessionText + LocationKey

```bash
curl -s "$BASE/ChecklistSearchSet?$filter=substringof('Operator',ProfessionText)%20and%20LocationKey%20eq%20'LOC-1'%20and%20DateCheck%20eq%20datetime'2025-01-10T00:00:00'&$top=20&$skip=0&$inlinecount=allpages"
```

### 2) Read LastChangeSet

```bash
ROOT='0123456789abcdef0123456789abcdef'
curl -s "$BASE/LastChangeSet('$ROOT')"
```

### 3) Read RuntimeSettingsSet

```bash
curl -s "$BASE/RuntimeSettingsSet('GLOBAL')"
```

### 4) LockStatusSet poll

```bash
curl -s "$BASE/LockStatusSet('$ROOT')?SessionGuid=sess-123"
```

### 5) LockControl acquire / heartbeat / release

```bash
# Fetch CSRF
curl -i -c cookies.txt -H 'X-CSRF-Token: Fetch' "$BASE/" | rg -i 'x-csrf-token'

TOKEN='<PUT_TOKEN_HERE>'

# Acquire
curl -s -b cookies.txt -H "X-CSRF-Token: $TOKEN" -H 'Content-Type: application/json' \
  -d '{"Action":"ACQUIRE","RootKey":"'$ROOT'","SessionGuid":"sess-123","Uname":"DEMO"}' \
  "$BASE/LockControl"

# Heartbeat
curl -s -b cookies.txt -H "X-CSRF-Token: $TOKEN" -H 'Content-Type: application/json' \
  -d '{"Action":"HEARTBEAT","RootKey":"'$ROOT'","SessionGuid":"sess-123","Uname":"DEMO"}' \
  "$BASE/LockControl"

# Release
curl -s -b cookies.txt -H "X-CSRF-Token: $TOKEN" -H 'Content-Type: application/json' \
  -d '{"Action":"RELEASE","RootKey":"'$ROOT'","SessionGuid":"sess-123","Uname":"DEMO"}' \
  "$BASE/LockControl"
```

### 6) AutoSave (unified delta `Changes[]`)

```bash
curl -s -b cookies.txt -H "X-CSRF-Token: $TOKEN" -H 'Content-Type: application/json' \
  -d '{
    "RootKey":"'$ROOT'",
    "ClientAggChangedOn":"/Date(1735689600000)/",
    "Changes":[
      {"Entity":"BASIC","Key":"'$ROOT'","ParentKey":"'$ROOT'","EditMode":"U","Fields":{"EquipName":"Pump-77"}},
      {"Entity":"CHECK","Key":"abc-client-temp","ParentKey":"'$ROOT'","EditMode":"C","Fields":{"ChecksNum":100,"Comment":"Autosave check","Result":true}}
    ]
  }' \
  "$BASE/AutoSave"
```

### 7) SaveChanges (full payload)

```bash
curl -s -b cookies.txt -H "X-CSRF-Token: $TOKEN" -H 'Content-Type: application/json' \
  -d '{
    "RootKey":"'$ROOT'",
    "ClientAggChangedOn":"/Date(1735689600000)/",
    "FullPayload":{
      "root":{"Status":"SUBMITTED"},
      "basic":{"LocationKey":"LOC-1","LocationName":"Area A","EquipName":"Boiler-9"},
      "checks":[{"ChecksNum":1,"Comment":"ok","Result":true}],
      "barriers":[{"BarriersNum":1,"Comment":"barrier ok","Result":true}]
    }
  }' \
  "$BASE/SaveChanges"
```

### 8) SetChecklistStatus

```bash
curl -s -b cookies.txt -H "X-CSRF-Token: $TOKEN" -H 'Content-Type: application/json' \
  -d '{"RootKey":"'$ROOT'","NewStatus":"DONE","ClientAggChangedOn":"/Date(1735689600000)/"}' \
  "$BASE/SetChecklistStatus"
```

### 9) GetHierarchy

```bash
curl -s "$BASE/GetHierarchy?DateCheck=datetime'2025-01-10T00:00:00'&Method=location_tree"
```

### 10) ReportExport (keys ordered)

```bash
curl -s -b cookies.txt -H "X-CSRF-Token: $TOKEN" -H 'Content-Type: application/json' \
  -d '{"RootKeys":["'$ROOT'","aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa"],"ColumnsPreset":"DEFAULT"}' \
  "$BASE/ReportExport"
```

## Aliases (для миграции)

Поддерживаются legacy пути, но deprecated:

- `/ChecklistRoots` -> `ChecklistSearchSet`
- `/SearchRows` -> `ChecklistSearchSet`
- `/actions/*` -> function imports
- `/lock/*` -> lock control/status



## Regression pack

```bash
./scripts/curl-regression-pack.sh
```

Скрипт проверяет service document, $metadata, csrf, $batch(read), search/$count, RuntimeSettings, LastChange, LockControl/LockStatus, GetHierarchy, ReportExport.

## Интеграционный smoke test (SmartFilterBar + SmartTable через OData)

1. Запустите backend и убедитесь, что доступен сервис-документ:
   - `GET http://localhost:8000/sap/opu/odata/sap/Z_UI5_SRV/`
2. Откройте UI и перейдите на экран Search.
3. Убедитесь в Network:
   - есть `POST /sap/opu/odata/sap/Z_UI5_SRV/$batch` со статусом 200;
   - нет запросов к `/checklist` и `/api/*` для списка поиска.
4. Примените фильтр в SmartFilterBar (например `Status` или `LocationKey`) и нажмите Go.
5. Убедитесь, что backend получает запрос c OData-параметрами `$filter`, `$top`, `$skip`, `$inlinecount` и SmartTable показывает отфильтрованные строки.
