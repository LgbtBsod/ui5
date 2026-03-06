param(
    [int]$BackendPort = 8000,
    [int]$UiPort = 8080
)

$ErrorActionPreference = "Stop"

$repoRoot = Split-Path -Parent $PSScriptRoot
$mockRoot = Join-Path $repoRoot "mock_gate_way"
$pythonVenv = Join-Path $mockRoot ".venv\\Scripts\\python.exe"
$pidDir = Join-Path $repoRoot "docs\\runtime"
$backendPidFile = Join-Path $pidDir "mock_backend.pid"
$uiPidFile = Join-Path $pidDir "ui_server.pid"
$backendOut = Join-Path $pidDir "mock_backend.out.log"
$backendErr = Join-Path $pidDir "mock_backend.err.log"
$uiOut = Join-Path $pidDir "ui_server.out.log"
$uiErr = Join-Path $pidDir "ui_server.err.log"

function Ensure-Dir([string]$Path) {
    if (-not (Test-Path $Path)) {
        New-Item -ItemType Directory -Path $Path | Out-Null
    }
}

function Stop-IfRunning([string]$PidFile, [int]$Port) {
    if (Test-Path $PidFile) {
        $existingPid = Get-Content $PidFile -ErrorAction SilentlyContinue | Select-Object -First 1
        if ($existingPid) {
            try {
                Stop-Process -Id ([int]$existingPid) -Force -ErrorAction Stop
            } catch {
            }
        }
        Remove-Item $PidFile -ErrorAction SilentlyContinue
    }

    Get-NetTCPConnection -LocalPort $Port -ErrorAction SilentlyContinue |
        Select-Object -ExpandProperty OwningProcess -Unique |
        Where-Object { $_ -gt 4 } |
        ForEach-Object {
            try {
                Stop-Process -Id $_ -Force -ErrorAction Stop
            } catch {
            }
        }
}

function Wait-Http([string]$Url, [int]$TimeoutSeconds, [string]$Name) {
    $deadline = (Get-Date).AddSeconds($TimeoutSeconds)
    while ((Get-Date) -lt $deadline) {
        try {
            $response = Invoke-WebRequest -Uri $Url -UseBasicParsing -TimeoutSec 3
            if ($response.StatusCode -ge 200 -and $response.StatusCode -lt 500) {
                return $response.StatusCode
            }
        } catch {
        }
        Start-Sleep -Milliseconds 400
    }
    throw "$Name did not start on $Url within $TimeoutSeconds seconds."
}

Ensure-Dir $pidDir

if (-not (Test-Path $pythonVenv)) {
    throw "Python venv not found: $pythonVenv"
}

Stop-IfRunning -PidFile $backendPidFile -Port $BackendPort
Stop-IfRunning -PidFile $uiPidFile -Port $UiPort

$backendProcess = Start-Process `
    -FilePath $pythonVenv `
    -ArgumentList @("-m", "uvicorn", "main:app", "--host", "127.0.0.1", "--port", "$BackendPort") `
    -WorkingDirectory $mockRoot `
    -RedirectStandardOutput $backendOut `
    -RedirectStandardError $backendErr `
    -WindowStyle Hidden `
    -PassThru

$backendProcess.Id | Set-Content $backendPidFile

$uiProcess = Start-Process `
    -FilePath "python" `
    -ArgumentList @("scripts/dev_static_server.py", "$UiPort") `
    -WorkingDirectory $repoRoot `
    -RedirectStandardOutput $uiOut `
    -RedirectStandardError $uiErr `
    -WindowStyle Hidden `
    -PassThru

$uiProcess.Id | Set-Content $uiPidFile

$backendStatus = Wait-Http -Url "http://127.0.0.1:$BackendPort/sap/opu/odata/sap/Z_UI5_SRV/`$metadata" -TimeoutSeconds 20 -Name "Mock backend"
$uiStatus = Wait-Http -Url "http://127.0.0.1:$UiPort/index.html" -TimeoutSeconds 20 -Name "UI server"

Write-Host "Backend: http://127.0.0.1:$BackendPort ($backendStatus)"
Write-Host "UI: http://127.0.0.1:$UiPort/index.html ($uiStatus)"
Write-Host "Logs: $pidDir"
