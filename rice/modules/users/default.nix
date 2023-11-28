### User options

{ config, lib, ... }:

with lib;
let
  cfg = config.rice.user;

in {
  options.rice.user = {
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
        };
      });

      description = "Individual user config";
    };
  };

  ## Option handling
  config = let
    ## Handle user.users
    userList = with builtins;
      (mapAttrs
        (n: v: {
          isNormalUser = true;
          isSystemUser = false;
          uid = v.id;
          group = n;
          extraGroups = v.groups;
        })
        cfg.users)
      ## Handle user.disableRoot
      // (if ! cfg.disableRoot then {}
          else {
            root = {
              hashedPassword = "**DISABLED**";
            };
          });


    ## Handle user.users
    groupList = with builtins;
      mapAttrs
        (n: v: {
          gid = v.id;
        })
        cfg.users;

    ## Handle user.immutable
    mutableUsers = !cfg.immutable;

  in {
    users.mutableUsers = mutableUsers;
    users.users = userList;
    users.groups = groupList;
  };
}
