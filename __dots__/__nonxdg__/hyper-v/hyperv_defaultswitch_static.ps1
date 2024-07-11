param($Subnet=233)
Remove-NetNat -Confirm:$false >$null 2>&1
New-NetNat -Name "VMNAT" -InternalIPInterfaceAddressPrefix "192.168.${Subnet}.0/24" >$null 2>&1
Remove-NetIPAddress -IPAddress "192.168.${Subnet}.1" -Confirm:$false >$null 2>&1
New-NetIPAddress -InterfaceAlias "vEthernet (Default Switch)" -IPAddress "192.168.${Subnet}.1" -PrefixLength 24 >$null 2>&1
