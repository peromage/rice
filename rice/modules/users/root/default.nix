{ ... }:
{ config, lib, ... }:

let
  cfg = config.rice.users.root;

in {
  options.rice.users.root = with lib; {
    disable = mkOption {
      type = types.bool;
      default = false;
      description = "Disable root user access.";
    };
  };

  config = {
    users.users.root = lib.optionalAttrs cfg.disable {
      ## FIXME: Remove this plain password
      hashedPassword = "**DISABLED**";
    };
  };
}
