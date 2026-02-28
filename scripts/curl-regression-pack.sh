#!/usr/bin/env bash
set -euo pipefail
BASE="${BASE:-http://localhost:8000/sap/opu/odata/sap/Z_UI5_SRV}"
ROOT="${ROOT:-0123456789ABCDEF0123456789ABCDEF}"
SESS="${SESS:-sess-1}"

echo "[1] service document"
curl -sS "$BASE/"
echo

echo "[2] metadata"
curl -sS "$BASE/\$metadata" >/dev/null && echo ok

echo "[3] csrf fetch"
TOKEN=$(curl -i -sS -H 'X-CSRF-Token: Fetch' "$BASE/" | awk -F': ' 'tolower($1)=="x-csrf-token"{print $2}' | tr -d '\r')
echo "token=$TOKEN"

echo "[4] search set"
curl -sS "$BASE/ChecklistSearchSet?\$top=5&\$skip=0&\$inlinecount=allpages"
echo

echo "[5] search count"
curl -sS "$BASE/ChecklistSearchSet/\$count" && echo

echo "[6] runtime settings"
curl -sS "$BASE/RuntimeSettingsSet(Key='GLOBAL')"
echo

echo "[7] last change"
curl -sS "$BASE/LastChangeSet(RootKey='$ROOT')"
echo

echo "[8] lock control acquire"
curl -sS -H "X-CSRF-Token: $TOKEN" -H 'Content-Type: application/json' -d '{"Action":"ACQUIRE","RootKey":"'$ROOT'","SessionGuid":"'$SESS'","Uname":"DEMO"}' "$BASE/LockControl"
echo

echo "[9] lock status"
curl -sS "$BASE/LockStatusSet(RootKey='$ROOT')?SessionGuid=$SESS"
echo

echo "[10] hierarchy"
curl -sS "$BASE/GetHierarchy?DateCheck=datetime'2025-01-01T00:00:00'&Method=location_tree"
echo

echo "[11] report export"
curl -sS -H "X-CSRF-Token: $TOKEN" -H 'Content-Type: application/json' -d '{"RootKeys":["'$ROOT'"]}' "$BASE/ReportExport"
echo

echo "[12] batch read"
B="batch_123"
curl -sS -H "Content-Type: multipart/mixed; boundary=$B" -H "X-CSRF-Token: $TOKEN" --data-binary $'--'$B'\r\nContent-Type: application/http\r\nContent-Transfer-Encoding: binary\r\n\r\nGET /sap/opu/odata/sap/Z_UI5_SRV/ChecklistSearchSet?$top=1 HTTP/1.1\r\n\r\n--'$B'--\r\n' "$BASE/\$batch" >/dev/null && echo ok
