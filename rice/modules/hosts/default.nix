### Host options

{ lib, ... }:

with lib; {
  options.rice.hosts = {
    hostName = {
      type = str;
      default = null;
      description = "Host name for this machine."
    };

    hosts = with types; mkOption {
      type = attrOf (submodule {
        options = {
          name = mkOption {
            type = nullOr str;
            default = null;
            description = "Host name. Default to the attribute key value."
          };
        };
      });
      description = "List of host configs."
    };
  };
}
