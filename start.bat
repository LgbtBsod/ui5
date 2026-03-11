@echo off
setlocal EnableExtensions

pushd "%~dp0" || (
  echo [ERROR] Failed to switch to repository directory.
  exit /b 1
)

set "START_SCRIPT=scripts\start-local-env.ps1"

if not exist "%START_SCRIPT%" (
  echo [ERROR] Missing startup script: %START_SCRIPT%
  popd
  exit /b 1
)

where powershell >nul 2>&1
if errorlevel 1 (
  echo [ERROR] PowerShell is required to run %START_SCRIPT%.
  popd
  exit /b 1
)

echo ==============================
echo Starting current local UI stack ...
echo ==============================
echo.

powershell -NoProfile -ExecutionPolicy Bypass -File "%START_SCRIPT%" -BindToParentShell
if errorlevel 1 (
  echo.
  echo [ERROR] Failed to start local environment.
  echo Check logs in docs\runtime\*.log
  popd
  exit /b 1
)

echo.
echo ==============================
echo Environment is up.
echo UI:            http://127.0.0.1:8080/index.html
echo Service root:  http://127.0.0.1:8000/sap/opu/odata/sap/Z_EHS_PRODUCTION_CONTROL_CKLT_SRV/
echo Stop command:  powershell -NoProfile -ExecutionPolicy Bypass -File scripts\stop-local-env.ps1
echo Window mode:   keep this window open while servers are running
echo ==============================

echo.
echo Press Q to stop servers and close this window.

:wait_loop
choice /c QX /n /t 2 /d X >nul
if errorlevel 2 goto wait_loop

echo.
echo Stopping local environment...
powershell -NoProfile -ExecutionPolicy Bypass -File "scripts\stop-local-env.ps1"

popd
endlocal
