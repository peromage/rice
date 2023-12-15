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
    mkUserOptions = {
      name
      , id
      , groups
      , description ? ""
      , initialPassword ? null
      , hashedPassword ? null
    }: with lib; {
      enable = mkEnableOption "User ${name}";

      name = mkOption {
        type = types.str;
        default = name;
        description = "User name.";
      };

      description = mkOption {
        type = types.str;
        default = "";
        description = "User description.";
      };

      id = mkOption {
        type = types.ints.unsigned;
        default = id;
        description = "User's UID and GID.";
      };

      groups = mkOption {
        type = with types; listOf str;
        default = groups;
        description = "Groups that user belongs to.";
      };

      initialPassword = mkOption {
        type = with types; nullOr str;
        default = initialPassword;
        description = "Initial password (mandatory).";
      };

      ## This option is effective only when immutable is enabled
      hashedPassword = mkOption {
        type = with types; nullOr (either str path);
        default = hashedPassword;
        description = "Hashed password or hashed password file (Used when immutable is enabled).";
      };
    };
  };

  enabledUsers = lib.filterAttrs (n: v: v.enable) cfg.profiles;

  ## If immutable is enabled hashed password must be supplied
  getHashedPassword = user: with lib; optionalAttrs
    cfg.immutable
    (assert null != user.hashedPassword;
      (if isString user.hashedPassword then {
        hashedPassword = user.hashedPassword;
      } else {
        hashedPasswordFile = user.hashedPassword;
      }));

  ## Handle users.users
  userList = with lib; mapAttrs'
    (n: v: nameValuePair v.name ({
      description = v.description;
      isNormalUser = true;
      isSystemUser = false;
      uid = v.id;
      group = n;
      extraGroups = v.groups;
      home = "/home/${v.name}";
      homeMode = "700";
      createHome = true;
      initialPassword = assert null != v.initialPassword; v.initialPassword;
    } // (getHashedPassword v)))
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
