### User options

## Subdirectories are not imported by default.
## Selectively import them as needed.

{ lib, ... }:

with lib; {
  imports = [
    ./config.nix
  ];

  options.rice.users = {
    immutable = mkOption {
      type = types.bool;
      default = false;
      description = "Immutable user management.";
    };

    ## Root user
    root = {
      disable = mkOption {
        type = types.bool;
        default = false;
        description = "Disable root user access.";
      };
    };

    ## Normal users
    users = mkOption {
      type = with types; attrsOf (submodule {
        options = {
          id = mkOption {
            type = ints.unsigned;
            default = 1000;
            description = "User UID and GID.";
          };

          groups = mkOption {
            type = listOf str;
            default = [];
            description = "Groups that user belongs to.";
          };

          ## TODO: Password
        };
      });
      description = "Normal user configurations.";
    };
  };
}
