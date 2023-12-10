### User options

{ config, lib, rice, ... }:

let
  librice = rice.lib;
  cfg = config.rice.users;

  ## Handle users.disableRoot
  rootConfig = lib.optionalAttrs cfg.root.disable {
    root = {
      ## TODO: Remove this plain password
      hashedPassword = "**DISABLED**";
    };
  };

  enabledUsers = lib.filterAttrs (n: v: v.enable) cfg.users;

  ## Handle users.users
  userList = with lib; mapAttrs'
    (n: v: nameValuePair v.name {
      isNormalUser = true;
      isSystemUser = false;
      uid = v.id;
      group = n;
      extraGroups = v.groups;
    })
    enabledUsers;

  ## Handle users.users
  groupList = with lib; mapAttrs'
    (n: v: nameValuePair v.name {
      gid = v.id;
    })
    enabledUsers;

  ## Handle users.immutable
  mutableUsers = !cfg.immutable;

in with lib; {
  imports = librice.allButDefault ./.;

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
          enable = mkEnableOption "User activation";

          name = mkOption {
            type = str;
            description = "User name";
          };

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

  config = {
    users.mutableUsers = mutableUsers;
    users.users = userList // rootConfig;
    users.groups = groupList;
  };
}
