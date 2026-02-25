@echo off
echo ==============================
echo Starting Mock SAP Gateway ...
echo ==============================

start cmd /k "cd mock_gate_way && uvicorn main:app --reload --port 5000"

echo ==============================
echo Starting UI5 App Server ...
echo ==============================

start cmd /k "python -m http.server 8080"

echo ==============================
echo All servers started
echo Gateway: http://localhost:5000
echo UI5:     http://localhost:8080
echo ==============================
