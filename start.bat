@echo off
setlocal EnableExtensions EnableDelayedExpansion

pushd "%~dp0" || (
  echo [ERROR] Failed to switch to repository directory.
  exit /b 1
)

set "COMMAND=%~1"
if "%COMMAND%"=="" set "COMMAND=start"

set "START_SCRIPT=scripts\start-local-env.ps1"
set "STOP_SCRIPT=scripts\stop-local-env.ps1"
set "RUNTIME_DIR=docs\runtime"

where powershell >nul 2>&1
if errorlevel 1 (
  echo [ERROR] PowerShell is required to manage the local stack.
  popd
  exit /b 1
)

if /I "%COMMAND%"=="start" goto do_start
if /I "%COMMAND%"=="stop" goto do_stop
if /I "%COMMAND%"=="restart" goto do_restart
if /I "%COMMAND%"=="status" goto do_status
if /I "%COMMAND%"=="clean" goto do_clean
goto usage

:do_start
if not exist "%START_SCRIPT%" (
  echo [ERROR] Missing startup script: %START_SCRIPT%
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
echo Stop command:  start.bat stop
echo Clean command: start.bat clean
echo Window mode:   keep this window open while servers are running
echo ==============================
echo.
echo Press Q to stop servers and close this window.
:wait_loop
choice /c QX /n /t 2 /d X >nul
if errorlevel 2 goto wait_loop
echo.
call "%~f0" stop
popd
endlocal
exit /b %errorlevel%

:do_stop
if not exist "%STOP_SCRIPT%" (
  echo [ERROR] Missing stop script: %STOP_SCRIPT%
  popd
  exit /b 1
)
echo ==============================
echo Stopping local UI stack ...
echo ==============================
powershell -NoProfile -ExecutionPolicy Bypass -File "%STOP_SCRIPT%"
popd
endlocal
exit /b %errorlevel%

:do_restart
call "%~f0" stop
if errorlevel 1 (
  popd
  endlocal
  exit /b 1
)
call "%~f0" start
popd
endlocal
exit /b %errorlevel%

:do_status
echo ==============================
echo Local stack status
echo ==============================
if exist "%RUNTIME_DIR%\mock_backend.pid" (
  set /p BACKEND_PID=<"%RUNTIME_DIR%\mock_backend.pid"
  echo Backend PID: !BACKEND_PID!
) else (
  echo Backend PID: not running
)
if exist "%RUNTIME_DIR%\ui_server.pid" (
  set /p UI_PID=<"%RUNTIME_DIR%\ui_server.pid"
  echo UI PID:      !UI_PID!
) else (
  echo UI PID:      not running
)
if exist "%RUNTIME_DIR%\env_guard.pid" (
  set /p GUARD_PID=<"%RUNTIME_DIR%\env_guard.pid"
  echo Guard PID:   !GUARD_PID!
) else (
  echo Guard PID:   not running
)
echo UI URL:        http://127.0.0.1:8080/index.html
echo Service root:  http://127.0.0.1:8000/sap/opu/odata/sap/Z_EHS_PRODUCTION_CONTROL_CKLT_SRV/
popd
endlocal
exit /b 0

:do_clean
echo ==============================
echo Cleaning derived local artifacts ...
echo ==============================
if exist dist rmdir /s /q dist
del /f /q "%RUNTIME_DIR%\*.log" "%RUNTIME_DIR%\*.pid" "%RUNTIME_DIR%\upload-smoke.txt" 2>nul
echo Cleaned dist and runtime logs.
popd
endlocal
exit /b 0

:usage
echo Usage: start.bat [start^|stop^|restart^|status^|clean]
popd
endlocal
exit /b 1
