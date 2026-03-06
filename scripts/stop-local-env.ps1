param(
    [int]$BackendPort = 8000,
    [int]$UiPort = 8080
)

$ErrorActionPreference = "Stop"

$repoRoot = Split-Path -Parent $PSScriptRoot
$pidDir = Join-Path $repoRoot "docs\\runtime"
$backendPidFile = Join-Path $pidDir "mock_backend.pid"
$uiPidFile = Join-Path $pidDir "ui_server.pid"

function Stop-ByPidFile([string]$PidFile) {
    if (-not (Test-Path $PidFile)) {
        return
    }
    $existingPid = Get-Content $PidFile -ErrorAction SilentlyContinue | Select-Object -First 1
    if ($existingPid) {
        try {
            Stop-Process -Id ([int]$existingPid) -Force -ErrorAction Stop
        } catch {
        }
    }
    Remove-Item $PidFile -ErrorAction SilentlyContinue
}

function Stop-ByPort([int]$Port) {
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

Stop-ByPidFile $backendPidFile
Stop-ByPidFile $uiPidFile
Stop-ByPort $BackendPort
Stop-ByPort $UiPort

Write-Host "Stopped backend on port $BackendPort and UI on port $UiPort."
