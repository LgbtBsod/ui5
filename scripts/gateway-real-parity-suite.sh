#!/usr/bin/env bash
set -euo pipefail

if [[ $# -lt 3 ]]; then
  echo "usage: $0 <mock_service_root> <real_service_root> <root_id>"
  echo "example: $0 http://127.0.0.1:8000/sap/opu/odata/sap/Z_EHS_PRODUCTION_CONTROL_CKLT_SRV https://real-host/sap/opu/odata/sap/Z_EHS_PRODUCTION_CONTROL_CKLT_SRV 0050568A..."
  exit 2
fi

MOCK_ROOT="$1"
REAL_ROOT="$2"
ROOT_ID="$3"

python scripts/gateway-metadata-drift-gate.py \
  --mock "${MOCK_ROOT}/\$metadata" \
  --real "${REAL_ROOT}/\$metadata"

python scripts/gateway-lock-multisession-replay.py "${REAL_ROOT}" "${ROOT_ID}"
python scripts/gateway-attachment-lifecycle-parity.py "${REAL_ROOT}" "${ROOT_ID}"

echo "gateway-real-parity-suite PASS"
