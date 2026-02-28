# Runbook (Backend + UI + Smoke Pack)

## Backend (FastAPI)

```bash
cd mock_gate_way
python3 -m pip install -r requirements.txt
python3 -m uvicorn main:app --host 0.0.0.0 --port 8000
```

## UI (SAPUI5 app static host)

```bash
cd /workspace/ui5
python3 -m http.server 8080
```

Open: `http://localhost:8080`

## Smoke regression pack

```bash
cd /workspace/ui5
BASE_URL=http://localhost:8000/sap/opu/odata/sap/Z_UI5_SRV ROOT_KEY=<RAW16_KEY> ./scripts/test-pack/curl_regression.sh
```

`ROOT_KEY` берите из `ChecklistSearchSet` (`Key` поле).
