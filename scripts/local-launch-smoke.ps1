param(
    [int]$BackendPort = 8000,
    [int]$UiPort = 8080,
    [string]$ExpectedChecklistId = "CHK-00001",
    [switch]$EnsureEnvironment,
    [switch]$StopEnvironmentAfter,
    [string]$GatewayBaseUrl = "",
    [string]$Ui5ResourcesBaseUrl = ""
)

$ErrorActionPreference = "Stop"

$repoRoot = Split-Path -Parent $PSScriptRoot
$startScript = Join-Path $PSScriptRoot "start-local-env.ps1"
$stopScript = Join-Path $PSScriptRoot "stop-local-env.ps1"
$indexUrl = "http://127.0.0.1:$UiPort/index.html"
$metadataUrl = "http://127.0.0.1:$BackendPort/sap/opu/odata/sap/Z_EHS_PRODUCTION_CONTROL_CKLT_SRV/`$metadata"
$searchUrl = "http://127.0.0.1:$UiPort/sap/opu/odata/sap/Z_EHS_PRODUCTION_CONTROL_CKLT_SRV/ChecklistSearchSet?`$top=1&`$format=json"
$startedEnvironment = $false

function Test-Http200([string]$Url) {
    try {
        $response = Invoke-WebRequest -UseBasicParsing -Uri $Url -TimeoutSec 5
        return ($response.StatusCode -eq 200)
    } catch {
        return $false
    }
}

function Ensure-EnvironmentStarted() {
    if ((Test-Http200 -Url $indexUrl) -and (Test-Http200 -Url $metadataUrl)) {
        return
    }

    & $startScript `
        -BackendPort $BackendPort `
        -UiPort $UiPort `
        -GatewayBaseUrl $GatewayBaseUrl `
        -Ui5ResourcesBaseUrl $Ui5ResourcesBaseUrl
    $script:startedEnvironment = $true
}

function Invoke-Http200([string]$Url) {
    $response = Invoke-WebRequest -UseBasicParsing -Uri $Url -TimeoutSec 10
    if ($response.StatusCode -ne 200) {
        throw "Expected HTTP 200 from $Url but received $($response.StatusCode)."
    }
    return $response
}

function Invoke-RequiredJson([string]$Url) {
    $response = Invoke-Http200 -Url $Url
    return ($response.Content | ConvertFrom-Json)
}

$verificationPasses = @(
    @{ Name = "UI index"; Url = $indexUrl },
    @{ Name = "Metadata"; Url = $metadataUrl },
    @{ Name = "UI index repeat"; Url = $indexUrl },
    @{ Name = "Metadata repeat"; Url = $metadataUrl }
)

try {
    if ($EnsureEnvironment.IsPresent) {
        Ensure-EnvironmentStarted
    }

    foreach ($probe in $verificationPasses) {
        $null = Invoke-Http200 -Url $probe.Url
    }

    $searchPayload = Invoke-RequiredJson -Url $searchUrl
    $firstResult = $searchPayload.d.results | Select-Object -First 1
    if ($null -eq $firstResult) {
        throw "ChecklistSearchSet returned no results."
    }
    if ($firstResult.Id -ne $ExpectedChecklistId) {
        throw "Unexpected first checklist id. Expected $ExpectedChecklistId but received $($firstResult.Id)."
    }

    $null = Invoke-RequiredJson -Url $searchUrl

    Write-Host "UI index: $indexUrl (200 x2)"
    Write-Host "Metadata: $metadataUrl (200 x2)"
    Write-Host "Search seed: $($firstResult.Id) / key $($firstResult.Key) (JSON x2)"
    if ($startedEnvironment) {
        Write-Host "Environment bootstrap: start-local-env.ps1 (deterministic mode, no parent-shell binding)"
    }
} finally {
    if ($StopEnvironmentAfter.IsPresent -and $startedEnvironment) {
        & $stopScript -BackendPort $BackendPort -UiPort $UiPort | Out-Null
    }
}
