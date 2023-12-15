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
      enable = mkEnableOption "user";

      name = mkOption {
        type = types.str;
        default = name;
        description = "User name.";
      };

      description = mkOption {
        type = types.str;
        default = description;
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
        description = ''
          Initial password for user if immutable user option is disabled.  In
          that case this option is mandatory.
          If immutable user option is enabled, this will be ignored and hashed
          password must be supplied.
        '';
      };

      ## This option is effective only when immutable is enabled
      hashedPassword = mkOption {
        type = with types; nullOr (either str path);
        default = hashedPassword;
        description = ''
          Hashed password or hashed password file.
          If immutable user option is enabled, this is mandatory.  Otherwise
          it is ignored and use initial password instead.
        '';
      };
    };
  };

  enabledUsers = lib.filterAttrs (n: v: v.enable) cfg.profiles;

  ## If immutable is enabled hashed password must be supplied
  getPassword = user: if cfg.immutable then
    (assert null != user.hashedPassword;
      (if lib.isString user.hashedPassword then {
        hashedPassword = user.hashedPassword;
      } else {
        hashedPasswordFile = user.hashedPassword;
      })) else {
        initialPassword = assert null != user.initialPassword; user.initialPassword;
      };

  ## Handle rice.users.profiles.<name>
  userList = with lib; mapAttrs'
    (n: v: nameValuePair v.name ({
      description = v.description;
      isNormalUser = true;
      isSystemUser = false;
      uid = v.id;
      group = v.name;
      extraGroups = v.groups;
      home = "/home/${v.name}";
      homeMode = "700";
      createHome = true;
    } // (getPassword v)))
    enabledUsers;

  ## Handle rice.users.profiles.<name>
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
      description = ''
        Immutable user management.
        Note that when this is enabled the hashed password must be specified
        for each user declared within rice namespace.
      '';
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
