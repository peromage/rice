{ mkUserOptions, ... }:
{ ... }:

{
  options.rice.users.profiles.fang = mkUserOptions {
    name = "fang";
    description = "Fang The Handsome";
    id = 1001;
    groups = [ "wheel" "users" "audio" "video" "cdrom" "networkmanager" ];
    initialPassword = "P@55w0rd";
  };
}
