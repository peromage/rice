{ mkUserOptions, ... }:
{ ... }:

{
  options.rice.users.profiles.fang = mkUserOptions {
    name = "fang";
    id = 1001;
    groups = [ "wheel" "users" "audio" "video" "cdrom" "networkmanager" ];
  };
}
