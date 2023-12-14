### User options

{ config, lib, rice, ... }:

let
  librice = rice.lib;
  cfg = config.rice.users;

  /* Additional arguments to import submodules.

     CONTRACT: Each profile declared in this set must have options:

       - enable
       - name
       - id
       - groups
  */
  args = {
    mkUserOptions = { name, id, groups }: with lib; {
      enable = mkEnableOption "Activate user ${name}";

      name = mkOption {
        type = types.str;
        default = name;
        description = "User ${name}";
      };

      id = mkOption {
        type = types.ints.unsigned;
        default = id;
        description = "User ${name}'s UID and GID.";
      };

      groups = mkOption {
        type = with types; listOf str;
        default = groups;
        description = "Groups that user ${name} belongs to.";
      };

      ## TODO: Password
    };
  };

  enabledUsers = lib.filterAttrs (n: v: v.enable) cfg.profiles;

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
  imports = with librice; callListWithArgs args (allButDefault ./.);

  options.rice.users = {
    immutable = mkOption {
      type = types.bool;
      default = false;
      description = "Immutable user management.";
    };

    ## Root user
    root = {};

    ## Normal users
    profiles = {};
  };

  config = {
    users.mutableUsers = mutableUsers;
    users.users = userList;
    users.groups = groupList;
  };
}
