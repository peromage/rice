### Host options

{ lib, ... }:

with lib; {
  imports = [
    ./config.nix
  ];

  options.rice.hosts = {
    hostName = mkOption {
      type = with types; nullOr str;
      default = null;
      description = "Host name for this machine.";
    };

    hosts = mkOption {
      type = with types; attrsOf (submodule {
        options = {
          name = mkOption {
            type = nullOr str;
            default = null;
            description = "Host name. Default to the attribute key value.";
          };
        };
      });
      description = "List of host configs.";
    };
  };
}
