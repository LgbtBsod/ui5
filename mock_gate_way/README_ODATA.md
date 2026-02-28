# mock_gate_way OData V2 (SAP Gateway-like)

Service root: `/sap/opu/odata/sap/Z_UI5_SRV/`

## CSRF fetch
```bash
curl -i "http://localhost:8000/sap/opu/odata/sap/Z_UI5_SRV/ChecklistFlatSet?$top=1" \
  -H "X-CSRF-Token: Fetch"
```

## LockAcquire (Function Import)
```bash
curl -X POST "http://localhost:8000/sap/opu/odata/sap/Z_UI5_SRV/LockAcquire" \
  -H "Content-Type: application/json" \
  -H "X-CSRF-Token: <token>" \
  -b "SAP_SESSIONID=<sid>" \
  -d '{"object_uuid":"<uuid>","session_guid":"<session>","uname":"USER1"}'
```

## LockHeartbeat
```bash
curl -X POST "http://localhost:8000/sap/opu/odata/sap/Z_UI5_SRV/LockHeartbeat" \
  -H "Content-Type: application/json" \
  -H "X-CSRF-Token: <token>" \
  -b "SAP_SESSIONID=<sid>" \
  -d '{"object_uuid":"<uuid>","session_guid":"<session>"}'
```

## LockRelease
```bash
curl -X POST "http://localhost:8000/sap/opu/odata/sap/Z_UI5_SRV/LockRelease" \
  -H "Content-Type: application/json" \
  -H "X-CSRF-Token: <token>" \
  -b "SAP_SESSIONID=<sid>" \
  -d '{"object_uuid":"<uuid>","session_guid":"<session>","try_save":false}'
```

## SaveDraft
```bash
curl -X POST "http://localhost:8000/sap/opu/odata/sap/Z_UI5_SRV/SaveDraft" \
  -H "Content-Type: application/json" \
  -H "X-CSRF-Token: <token>" \
  -b "SAP_SESSIONID=<sid>" \
  -d '{"session_guid":"<session>","uname":"USER1","payload":{"Uuid":"temp-1","ChecklistId":"TMP-001","Lpc":"LPC-02"}}'
```

## SaveChanges
```bash
curl -X POST "http://localhost:8000/sap/opu/odata/sap/Z_UI5_SRV/SaveChanges" \
  -H "Content-Type: application/json" \
  -H "X-CSRF-Token: <token>" \
  -b "SAP_SESSIONID=<sid>" \
  -d '{"session_guid":"<session>","client_version":1,"payload":{"Uuid":"<uuid>","Equipment":"Pump A"}}'
```

## AutoSave
```bash
curl -X POST "http://localhost:8000/sap/opu/odata/sap/Z_UI5_SRV/AutoSave" \
  -H "Content-Type: application/json" \
  -H "X-CSRF-Token: <token>" \
  -b "SAP_SESSIONID=<sid>" \
  -d '{"session_guid":"<session>","client_version":2,"payload":{"Uuid":"<uuid>","ObserverFullname":"John Doe"}}'
```

## CopyChecklist
```bash
curl -X POST "http://localhost:8000/sap/opu/odata/sap/Z_UI5_SRV/CopyChecklist" \
  -H "Content-Type: application/json" \
  -H "X-CSRF-Token: <token>" \
  -b "SAP_SESSIONID=<sid>" \
  -d '{"source_uuid":"<uuid>","session_guid":"<session>","uname":"USER1"}'
```

## MplTree (Function Import)
```bash
curl "http://localhost:8000/sap/opu/odata/sap/Z_UI5_SRV/MplTree?date=2025-01-01"
```

## Search with server-side paging (SmartTable)
```bash
curl "http://localhost:8000/sap/opu/odata/sap/Z_UI5_SRV/ChecklistFlatSet?$top=50&$skip=0&$inlinecount=allpages&$orderby=ChangedOn%20desc"
```

## Batch example
```bash
curl -X POST "http://localhost:8000/sap/opu/odata/sap/Z_UI5_SRV/$batch" \
  -H "Content-Type: multipart/mixed; boundary=batch_123" \
  -H "X-CSRF-Token: <token>" \
  -b "SAP_SESSIONID=<sid>" \
  --data-binary $'--batch_123\r\nContent-Type: application/http\r\nContent-Transfer-Encoding: binary\r\n\r\nGET /sap/opu/odata/sap/Z_UI5_SRV/ChecklistFlatSet?$top=1 HTTP/1.1\r\n\r\n--batch_123--\r\n'
```
