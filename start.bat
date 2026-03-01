@echo off
setlocal

echo ==============================
echo Starting Mock SAP Gateway ...
echo ==============================

start cmd /k "cd /d mock_gate_way && python -m uvicorn main:app --reload --host 0.0.0.0 --port 8000"

echo ==============================
echo Starting UI5 App Server ...
echo ==============================

start cmd /k "python -m http.server 8080"

echo ==============================
echo All servers started
echo Gateway:      http://localhost:8000
echo Service root: http://localhost:8000/sap/opu/odata/sap/Z_UI5_SRV/
echo UI5:          http://localhost:8080
echo ==============================

endlocal
