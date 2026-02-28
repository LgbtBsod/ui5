#!/usr/bin/env bash
set -euo pipefail

BASE_URL="${BASE_URL:-http://localhost:8000/sap/opu/odata/sap/Z_UI5_SRV}"
ROOT_KEY="${ROOT_KEY:-00000000000000000000000000000001}"
COOKIE_JAR="${COOKIE_JAR:-/tmp/ui5_gateway_cookie.txt}"
TOKEN=""

req() {
  local method="$1"; shift
  local url="$1"; shift
  curl -sS -X "$method" "$url" "$@"
}

echo "[1] Service document"
req GET "$BASE_URL/" | head -c 400; echo

echo "[2] Metadata"
req GET "$BASE_URL/\$metadata" | head -c 400; echo

echo "[3] CSRF fetch"
TOKEN=$(curl -sS -D - -o /dev/null -c "$COOKIE_JAR" -H 'X-CSRF-Token: Fetch' "$BASE_URL/" | awk -F': ' '/^X-CSRF-Token:/ {print $2}' | tr -d '\r')
echo "TOKEN=${TOKEN}"

echo "[4] RuntimeSettingsSet('GLOBAL')"
req GET "$BASE_URL/RuntimeSettingsSet('GLOBAL')" -b "$COOKIE_JAR" | head -c 500; echo

echo "[5] Batch read (2 GET operations)"
BATCH_BOUNDARY="batch_123"
cat > /tmp/ui5_batch_body.txt <<EOF
--${BATCH_BOUNDARY}
Content-Type: application/http
Content-Transfer-Encoding: binary

GET ${BASE_URL}/ChecklistSearchSet?\$top=1 HTTP/1.1
Accept: application/json

--${BATCH_BOUNDARY}
Content-Type: application/http
Content-Transfer-Encoding: binary

GET ${BASE_URL}/RuntimeSettingsSet('GLOBAL') HTTP/1.1
Accept: application/json

--${BATCH_BOUNDARY}--
EOF
curl -sS -X POST "$BASE_URL/\$batch" -H "Content-Type: multipart/mixed; boundary=${BATCH_BOUNDARY}" -H "X-CSRF-Token: ${TOKEN}" -b "$COOKIE_JAR" --data-binary @/tmp/ui5_batch_body.txt | head -c 600; echo

echo "[6] ChecklistSearchSet with paging + inlinecount"
req GET "$BASE_URL/ChecklistSearchSet?\$top=5&\$skip=0&\$inlinecount=allpages" -b "$COOKIE_JAR" | head -c 600; echo

echo "[7] ChecklistSearchSet/$count"
req GET "$BASE_URL/ChecklistSearchSet/\$count" -b "$COOKIE_JAR"; echo

echo "[8] LastChangeSet"
req GET "$BASE_URL/LastChangeSet(RootKey='${ROOT_KEY}')" -b "$COOKIE_JAR" | head -c 400; echo

echo "[9] LockControl ACQUIRE/HEARTBEAT/RELEASE"
for action in ACQUIRE HEARTBEAT RELEASE; do
  req POST "$BASE_URL/LockControl" -b "$COOKIE_JAR" -H "X-CSRF-Token: ${TOKEN}" -H 'Content-Type: application/json' \
    --data "{\"RootKey\":\"${ROOT_KEY}\",\"Action\":\"${action}\",\"SessionGuid\":\"TEST-SESSION\",\"Uname\":\"TEST\"}" | head -c 350; echo
done

echo "[10] AutoSave sample"
req POST "$BASE_URL/AutoSave" -b "$COOKIE_JAR" -H "X-CSRF-Token: ${TOKEN}" -H 'Content-Type: application/json' \
  --data "{\"RootKey\":\"${ROOT_KEY}\",\"ClientAggChangedOn\":\"/Date(0)/\",\"Changes\":[]}" | head -c 350; echo

echo "[11] SaveChanges sample"
req POST "$BASE_URL/SaveChanges" -b "$COOKIE_JAR" -H "X-CSRF-Token: ${TOKEN}" -H 'Content-Type: application/json' \
  --data "{\"RootKey\":\"${ROOT_KEY}\",\"ClientAggChangedOn\":\"/Date(0)/\",\"FullPayload\":{\"checks\":[],\"barriers\":[]}}" | head -c 350; echo

echo "[12] SetChecklistStatus sample"
req POST "$BASE_URL/SetChecklistStatus" -b "$COOKIE_JAR" -H "X-CSRF-Token: ${TOKEN}" -H 'Content-Type: application/json' \
  --data "{\"RootKey\":\"${ROOT_KEY}\",\"NewStatus\":\"SUBMITTED\",\"ClientAggChangedOn\":\"/Date(0)/\"}" | head -c 350; echo

echo "[13] GetHierarchy sample"
req GET "$BASE_URL/GetHierarchy?DateCheck=datetime'2026-03-01T00:00:00'&Method='location_tree'" -b "$COOKIE_JAR" | head -c 350; echo

echo "[14] ReportExport sample"
req POST "$BASE_URL/ReportExport" -b "$COOKIE_JAR" -H "X-CSRF-Token: ${TOKEN}" -H 'Content-Type: application/json' \
  --data "{\"Mode\":\"KEYS\",\"RootKeys\":[\"${ROOT_KEY}\"]}" | head -c 350; echo

echo "[15] Attachment stream PUT/GET"
ATT_KEY="${ROOT_KEY}::sample.txt"
req PUT "$BASE_URL/AttachmentSet(Key='${ATT_KEY}')/\$value" -b "$COOKIE_JAR" -H "X-CSRF-Token: ${TOKEN}" -H 'Content-Type: text/plain' --data 'hello attachment' | head -c 300; echo
req GET "$BASE_URL/AttachmentSet(Key='${ATT_KEY}')/\$value" -b "$COOKIE_JAR" | head -c 100; echo

echo "Done"
