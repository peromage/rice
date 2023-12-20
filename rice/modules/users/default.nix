{ config, lib, rice, ... }:

let
  inherit (lib) types mkOption mkEnableOption isString mapAttrs' nameValuePair mkIf;
  inherit (rice.lib) anyEnable filterEnable callListWithArgs listDirNoDefault allAttrs;

  cfg = config.rice.users;

  options = {
    immutable = mkOption {
      type = types.bool;
      default = false;
      description = ''
        Immutable user management.
        Note that when this is enabled the `hashedPassword' must be specified
        for each user declared within rice namespace.
      '';
    };

    ## Root user
    root = {};

    ## Normal users
    profiles = {};
  };

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
    }: {
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
          Initial password for user if `immutable' user option is disabled.  In
          that case this option is mandatory.
          If `immutable' user option is enabled, this will be ignored and
          `hashedPassword' must be supplied.
        '';
      };

      ## This option is effective only when immutable is enabled
      hashedPassword = mkOption {
        type = with types; nullOr (either str path);
        default = hashedPassword;
        description = ''
          Hashed password or hashed password file.
          If `immutable' user option is enabled, this is mandatory.  Otherwise
          it is ignored and use `initialPassword' instead.
        '';
      };
    };
  };

  ## User config is only enabled if any one of the profiles is turned on
  enableUserConfig = anyEnable cfg.profiles;
  enabledUsers = filterEnable cfg.profiles;

  getPassword = user:
    if cfg.immutable then
      if isString user.hashedPassword then
        { hashedPassword = user.hashedPassword; }
      else
        { hashedPasswordFile = toString user.hashedPassword; }
    else
      { initialPassword = user.initialPassword; };

  ## Handle rice.users.profiles.<name>
  userList = mapAttrs'
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
  groupList = mapAttrs'
    (n: v: nameValuePair v.name {
      gid = v.id;
    })
    enabledUsers;

  ## Handle users.immutable
  mutableUsers = !cfg.immutable;

in {
  imports = callListWithArgs args (listDirNoDefault ./.);
  options.rice.users = options;

  config = mkIf enableUserConfig {
    assertions = [
      {
        assertion = cfg.immutable -> allAttrs (n: v: null != v.hashedPassword) enabledUsers;
        message = "Hashed password for normal users must be provided when immutable user is enabled.";
      }

      {
        assertion = !cfg.immutable -> allAttrs (n: v: null != v.initialPassword) enabledUsers;
        message = "Initial password for normal users must be provided.";
      }
    ];

    users.mutableUsers = mutableUsers;
    users.users = userList;
    users.groups = groupList;
  };
}
