{ lib, ... }:

{
  config.pix.users.profiles.fang = with lib; {
    description = "Fang The Handsome";
    id = 1001;
    groups = [ "wheel" "users" "audio" "video" "cdrom" "networkmanager" ];
    enableNixManagement = true;
    password = mkDefault "P@55w0rd";
  };
}
