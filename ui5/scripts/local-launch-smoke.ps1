param(
    [int]$BackendPort = 8000,
    [int]$UiPort = 8080,
    [string]$ExpectedChecklistId = "CHK-00001"
)

$ErrorActionPreference = "Stop"

$indexUrl = "http://127.0.0.1:$UiPort/index.html"
$metadataUrl = "http://127.0.0.1:$BackendPort/sap/opu/odata/sap/Z_EHS_PRODUCTION_CONTROL_CKLT_SRV/`$metadata"
$searchUrl = "http://127.0.0.1:$UiPort/sap/opu/odata/sap/Z_EHS_PRODUCTION_CONTROL_CKLT_SRV/ChecklistSearchSet?`$top=1&`$format=json"

function Invoke-RequiredJson([string]$Url) {
    $response = Invoke-WebRequest -UseBasicParsing -Uri $Url -TimeoutSec 10
    if ($response.StatusCode -ne 200) {
        throw "Expected HTTP 200 from $Url but received $($response.StatusCode)."
    }
    return ($response.Content | ConvertFrom-Json)
}

$indexResponse = Invoke-WebRequest -UseBasicParsing -Uri $indexUrl -TimeoutSec 10
if ($indexResponse.StatusCode -ne 200) {
    throw "UI index is not reachable: $indexUrl"
}

$metadataResponse = Invoke-WebRequest -UseBasicParsing -Uri $metadataUrl -TimeoutSec 10
if ($metadataResponse.StatusCode -ne 200) {
    throw "Mock metadata is not reachable: $metadataUrl"
}

$searchPayload = Invoke-RequiredJson -Url $searchUrl
$firstResult = $searchPayload.d.results | Select-Object -First 1
if ($null -eq $firstResult) {
    throw "ChecklistSearchSet returned no results."
}
if ($firstResult.Id -ne $ExpectedChecklistId) {
    throw "Unexpected first checklist id. Expected $ExpectedChecklistId but received $($firstResult.Id)."
}

Write-Host "UI index: $indexUrl (200)"
Write-Host "Metadata: $metadataUrl (200)"
Write-Host "Search seed: $($firstResult.Id) / key $($firstResult.Key)"
