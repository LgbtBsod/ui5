@echo off
setlocal EnableExtensions EnableDelayedExpansion

pushd "%~dp0" || (
  echo [ERROR] Failed to switch to repository directory.
  exit /b 1
)

set "ROOT_DIR=."
set "GATEWAY_DIR=mock_gate_way"
set "REQ_FILE=%GATEWAY_DIR%\requirements.txt"
set "VENV_DIR=%GATEWAY_DIR%\.venv"
set "PYTHON_EXE="
set "PORT_GATEWAY=8000"
set "PORT_UI=8080"

echo ==============================
echo Preparing test environment ...
echo ==============================

where py >nul 2>&1
if %errorlevel%==0 (
  set "PYTHON_EXE=py -3"
) else (
  where python >nul 2>&1
  if %errorlevel%==0 (
    set "PYTHON_EXE=python"
  )
)

if "%PYTHON_EXE%"=="" (
  echo [ERROR] Python 3 was not found. Install Python and re-run start.bat
  popd
  exit /b 1
)

if not exist "%VENV_DIR%\Scripts\python.exe" (
  echo Creating virtual environment in %VENV_DIR%
  call %PYTHON_EXE% -m venv "%VENV_DIR%"
  if errorlevel 1 (
    echo [ERROR] Failed to create virtual environment.
    popd
    exit /b 1
  )
)

echo Installing backend dependencies ...
call "%VENV_DIR%\Scripts\python.exe" -m pip install --disable-pip-version-check -r "%REQ_FILE%"
if errorlevel 1 (
  echo [ERROR] Failed to install Python dependencies.
  popd
  exit /b 1
)

echo ==============================
echo Starting Mock SAP Gateway ...
echo ==============================
start "Mock Gateway" cmd /k "cd /d %GATEWAY_DIR% && .\.venv\Scripts\python.exe -m uvicorn main:app --reload --host 0.0.0.0 --port %PORT_GATEWAY%"

echo ==============================
echo Starting UI5 App Server ...
echo ==============================
start "UI5 Server" cmd /k "cd /d . && %VENV_DIR%\Scripts\python.exe scripts\dev_static_server.py %PORT_UI%"

echo ==============================
echo Environment is up.
echo Gateway:      http://localhost:%PORT_GATEWAY%
echo Service root: http://localhost:%PORT_GATEWAY%/sap/opu/odata/sap/Z_UI5_SRV/
echo UI5:          http://localhost:%PORT_UI%
echo QA check:     npm run qa
echo ==============================

popd
endlocal
