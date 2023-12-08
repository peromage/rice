[CmdletBinding(DefaultParameterSetName="Help")]
param ([Parameter(Mandatory=$true, ParameterSetName="AddForward")]
       [switch]$add,

       [Parameter(Mandatory=$true, ParameterSetName="DeleteForward")]
       [switch]$delete,

       [Parameter(Mandatory=$true, ParameterSetName="Status")]
       [switch]$status,

       [Parameter(ParameterSetName="Help")]
       [switch]$help,

       [Parameter(Mandatory=$true, ParameterSetName="AddForward")]
       [Parameter(Mandatory=$true, ParameterSetName="DeleteForward")]
       [string]$port,

       # Optional parameters
       [Parameter(Mandatory=$false, ParameterSetName="AddForward")]
       [Parameter(Mandatory=$false, ParameterSetName="DeleteForward")]
       [string]$ip="",

       [Parameter(Mandatory=$false, ParameterSetName="AddForward")]
       [Parameter(Mandatory=$false, ParameterSetName="DeleteForward")]
       [string]$localip="0.0.0.0",

       [Parameter(Mandatory=$false, ParameterSetName="AddForward")]
       [Parameter(Mandatory=$false, ParameterSetName="DeleteForward")]
       [string]$localport=$port)

function print_help {
    Write-Host @"
  $PSCommandPath
    -add PORT
    -delete PORT
    -status
    -help
"@
}

function getWslIp {
    (wsl hostname -I | Select-Object -First 1).Trim()
}

if (-not $add -and -not $delete -and -not $status) {
    print_help
    exit 1
}

if ($status) {
    Invoke-Expression "netsh interface portproxy show all"
    exit 0
}

$ip = if ("" -eq $ip) { "$(getWslIp)" } else { $ip }

if ($add) {
    Invoke-Expression "netsh interface portproxy add v4tov4 listenport=$localport listenaddress=$localip connectaddress=$ip connectport=$port"
    exit 0
}
if ($delete) {
    Invoke-Expression "netsh interface portproxy delete v4tov4 listenaddress=$localip listenport=$localport"
    exit 0
}
