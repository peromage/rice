param ($subcmd)

$port = "22"
$host_ip = "0.0.0.0"

function getWslIp {
    wsl hostname -I | Select-Object -First 1
}

switch ($subcmd) {
    "stop" {
        Invoke-Expression "netsh interface portproxy delete v4tov4 listenaddress=$host_ip listenport=$port"
    }

    "start" {
        Invoke-Expression "netsh interface portproxy add v4tov4 listenport=$port listenaddress=$host_ip connectaddress=$(getWslIp) connectport=$port"
    }

    default {
        Invoke-Expression "netsh interface portproxy show all"
    }
}
