param(
    [ValidateSet("cdn", "system")]
    [string]$Mode = "cdn"
)

$bootstrapFile = Join-Path $PSScriptRoot "..\app\ui5-bootstrap-runtime.js"
$content = Get-Content -Path $bootstrapFile -Raw

$cdnSource = 'https://ui5.sap.com/1.71.70/resources/sap-ui-core.js'
$systemSource = '/resources/sap-ui-core.js'

if ($Mode -eq "cdn") {
    $updated = $content -replace [regex]::Escape($systemSource), $cdnSource
}
else {
    $updated = $content -replace [regex]::Escape($cdnSource), $systemSource
}

if ($updated -eq $content) {
    Write-Output "UI5 bootstrap already set to '$Mode'."
    exit 0
}

Set-Content -Path $bootstrapFile -Value $updated -Encoding UTF8
Write-Output "UI5 bootstrap switched to '$Mode'."
