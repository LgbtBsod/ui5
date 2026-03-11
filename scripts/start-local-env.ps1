param(
    [int]$BackendPort = 8000,
    [int]$UiPort = 8080,
    [string]$GatewayBaseUrl = "",
    [switch]$BindToParentShell
)

$ErrorActionPreference = "Stop"

$repoRoot = Split-Path -Parent $PSScriptRoot
$mockRoot = Join-Path $repoRoot "backend\\mock_gateway"
$pythonVenv = Join-Path $mockRoot ".venv\\Scripts\\python.exe"
$pidDir = Join-Path $repoRoot "docs\\runtime"
$backendPidFile = Join-Path $pidDir "mock_backend.pid"
$uiPidFile = Join-Path $pidDir "ui_server.pid"
$backendOut = Join-Path $pidDir "mock_backend.out.log"
$backendErr = Join-Path $pidDir "mock_backend.err.log"
$uiOut = Join-Path $pidDir "ui_server.out.log"
$uiErr = Join-Path $pidDir "ui_server.err.log"
$guardPidFile = Join-Path $pidDir "env_guard.pid"
$servicePath = "/sap/opu/odata/sap/Z_EHS_PRODUCTION_CONTROL_CKLT_SRV/"
$isRealGateway = -not [string]::IsNullOrWhiteSpace($GatewayBaseUrl)

function Ensure-Dir([string]$Path) {
    if (-not (Test-Path $Path)) {
        New-Item -ItemType Directory -Path $Path | Out-Null
    }
}

function New-PythonCandidate([string]$FilePath, [string[]]$PrefixArgs, [string]$Source) {
    return [PSCustomObject]@{
        FilePath = $FilePath
        PrefixArgs = @($PrefixArgs)
        Source = $Source
    }
}

function Resolve-PythonCandidates() {
    $candidates = @()
    $seen = @{}

    if (Test-Path $pythonVenv) {
        $key = ($pythonVenv.Trim().ToLowerInvariant() + "|")
        if (-not $seen.ContainsKey($key)) {
            $seen[$key] = $true
            $candidates += (New-PythonCandidate -FilePath $pythonVenv -PrefixArgs @() -Source "backend/mock_gateway/.venv")
        }
    }
    if (-not [string]::IsNullOrWhiteSpace($env:PYTHON_BIN)) {
        $filePath = $env:PYTHON_BIN.Trim()
        $key = ($filePath.ToLowerInvariant() + "|")
        if (-not $seen.ContainsKey($key)) {
            $seen[$key] = $true
            $candidates += (New-PythonCandidate -FilePath $filePath -PrefixArgs @() -Source "PYTHON_BIN")
        }
    }
    if (-not [string]::IsNullOrWhiteSpace($env:PYTHON)) {
        $filePath = $env:PYTHON.Trim()
        $key = ($filePath.ToLowerInvariant() + "|")
        if (-not $seen.ContainsKey($key)) {
            $seen[$key] = $true
            $candidates += (New-PythonCandidate -FilePath $filePath -PrefixArgs @() -Source "PYTHON")
        }
    }
    if (Get-Command py -ErrorAction SilentlyContinue) {
        $key = "py|-3"
        if (-not $seen.ContainsKey($key)) {
            $seen[$key] = $true
            $candidates += (New-PythonCandidate -FilePath "py" -PrefixArgs @("-3") -Source "py -3 launcher")
        }
    }
    if (Get-Command python -ErrorAction SilentlyContinue) {
        $key = "python|"
        if (-not $seen.ContainsKey($key)) {
            $seen[$key] = $true
            $candidates += (New-PythonCandidate -FilePath "python" -PrefixArgs @() -Source "python on PATH")
        }
    }

    return $candidates
}

function Test-PythonCandidate([object]$Candidate, [switch]$RequireBackendDeps) {
    $probeCode = if ($RequireBackendDeps.IsPresent) {
        "import fastapi, uvicorn; print('ok')"
    } else {
        "import sys; print(sys.version)"
    }
    try {
        $null = & $Candidate.FilePath @($Candidate.PrefixArgs + @("-c", $probeCode)) 2>$null
        return $true
    } catch {
        return $false
    }
}

function Resolve-PythonCommand([switch]$RequireBackendDeps) {
    $candidates = Resolve-PythonCandidates
    foreach ($candidate in $candidates) {
        if (Test-PythonCandidate -Candidate $candidate -RequireBackendDeps:$RequireBackendDeps.IsPresent) {
            return $candidate
        }
    }

    $candidateHints = ($candidates | ForEach-Object {
        if ($_.PrefixArgs.Count) {
            return "$($_.FilePath) $($_.PrefixArgs -join ' ')"
        }
        return $_.FilePath
    }) -join ", "

    if ($RequireBackendDeps.IsPresent) {
        throw "Python with FastAPI and uvicorn was not found. Tried: $candidateHints. Either create backend/mock_gateway/.venv, set PYTHON_BIN/PYTHON to a prepared interpreter, or use -GatewayBaseUrl to run against an external SAP Gateway."
    }
    throw "Python was not found. Tried: $candidateHints. Set PYTHON_BIN/PYTHON, install Python 3, or create backend/mock_gateway/.venv."
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

    if ($Port -gt 0) {
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

$pythonCommand = Resolve-PythonCommand -RequireBackendDeps:(!$isRealGateway)

Stop-IfRunning -PidFile $backendPidFile -Port $BackendPort
Stop-IfRunning -PidFile $uiPidFile -Port $UiPort
Stop-IfRunning -PidFile $guardPidFile -Port 0

$uiBackendBase = if ($isRealGateway) { $GatewayBaseUrl.TrimEnd("/") } else { "http://127.0.0.1:$BackendPort" }

if ($isRealGateway) {
    Remove-Item $backendPidFile -ErrorAction SilentlyContinue
} else {
    $backendProcess = Start-Process `
        -FilePath $pythonCommand.FilePath `
        -ArgumentList @($pythonCommand.PrefixArgs + @("-m", "uvicorn", "main:app", "--host", "127.0.0.1", "--port", "$BackendPort")) `
        -WorkingDirectory $mockRoot `
        -RedirectStandardOutput $backendOut `
        -RedirectStandardError $backendErr `
        -WindowStyle Hidden `
        -PassThru

    $backendProcess.Id | Set-Content $backendPidFile
}

$prevUiBackendBase = $env:UI5_BACKEND_BASE
$env:UI5_BACKEND_BASE = $uiBackendBase
try {
    $uiProcess = Start-Process `
        -FilePath $pythonCommand.FilePath `
        -ArgumentList @($pythonCommand.PrefixArgs + @("scripts/dev_static_server.py", "$UiPort")) `
        -WorkingDirectory $repoRoot `
        -RedirectStandardOutput $uiOut `
        -RedirectStandardError $uiErr `
        -WindowStyle Hidden `
        -PassThru
} finally {
    if ($null -eq $prevUiBackendBase) {
        Remove-Item Env:UI5_BACKEND_BASE -ErrorAction SilentlyContinue
    } else {
        $env:UI5_BACKEND_BASE = $prevUiBackendBase
    }
}

$uiProcess.Id | Set-Content $uiPidFile

if ($BindToParentShell.IsPresent) {
    $parentPid = (Get-CimInstance Win32_Process -Filter "ProcessId=$PID").ParentProcessId
    if ($parentPid -gt 0) {
        $stopScript = Join-Path $repoRoot "scripts\\stop-local-env.ps1"
        $escapedStopScript = $stopScript.Replace("'", "''")
        $guardCommand = @"
`$ErrorActionPreference = 'SilentlyContinue'
`$parentPid = $parentPid
while (`$true) {
    if (-not (Get-Process -Id `$parentPid -ErrorAction SilentlyContinue)) {
        break
    }
    Start-Sleep -Seconds 2
}
& '$escapedStopScript' -BackendPort $BackendPort -UiPort $UiPort | Out-Null
"@
        $guardProcess = Start-Process `
            -FilePath "powershell" `
            -ArgumentList @("-NoProfile", "-ExecutionPolicy", "Bypass", "-WindowStyle", "Hidden", "-Command", $guardCommand) `
            -WindowStyle Hidden `
            -PassThru
        $guardProcess.Id | Set-Content $guardPidFile
    }
}

$backendProbeBase = if ($isRealGateway) { $GatewayBaseUrl.TrimEnd("/") } else { "http://127.0.0.1:$BackendPort" }
$backendName = if ($isRealGateway) { "SAP Gateway" } else { "Mock backend" }
$backendStatus = Wait-Http -Url "$backendProbeBase$servicePath`$metadata" -TimeoutSeconds 20 -Name $backendName
$uiStatus = Wait-Http -Url "http://127.0.0.1:$UiPort/index.html" -TimeoutSeconds 20 -Name "UI server"

if ($isRealGateway) {
    Write-Host "Backend (SAP Gateway): $backendProbeBase ($backendStatus)"
} else {
    Write-Host "Backend: http://127.0.0.1:$BackendPort ($backendStatus)"
}
Write-Host "Python: $($pythonCommand.FilePath) $($pythonCommand.PrefixArgs -join ' ') [$($pythonCommand.Source)]"
Write-Host "UI: http://127.0.0.1:$UiPort/index.html ($uiStatus)"
Write-Host "Logs: $pidDir"
