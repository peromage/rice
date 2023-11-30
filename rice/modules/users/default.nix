### User options

{ lib, ... }:

with lib; {
  imports = [
    ./config.nix
  ];

  options.rice.users = {
    immutable = mkOption {
      type = types.bool;
      default = false;
      description = "Immutable user management";
    };

    disableRoot = mkOption {
      type = types.bool;
      default = false;
      description = "Make root user inaccessible";
    };

    users = with types; mkOption {
      type = attrsOf (submodule {
        options = {
          id = mkOption {
            type = ints.unsigned;
            default = 1000;
            description = "User UID and GID";
          };

          groups = mkOption {
            type = listOf str;
            default = [];
            description = "Groups that user belongs to";
          };

          ## TODO: Password
        };
      });
      description = "Individual user config";
    };
  };
}
