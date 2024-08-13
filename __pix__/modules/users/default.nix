{ config, lib, pix, ... }:

let
  libpix = pix.lib;

  cfg = config.pix.users;

  options = {
    immutable = with lib; mkOption {
      type = types.bool;
      default = false;
      description = ''
        Immutable user management.
        Note that when this is enabled the `hashedPassword' must be specified
        for each user declared within pix namespace.
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
      enable = lib.mkEnableOption "user";

      name = with lib; mkOption {
        type = types.str;
        default = name;
        description = "User name.";
      };

      description = with lib; mkOption {
        type = types.str;
        default = description;
        description = "User description.";
      };

      id = with lib; mkOption {
        type = types.ints.unsigned;
        default = id;
        description = "User's UID and GID.";
      };

      groups = with lib; mkOption {
        type = with types; listOf str;
        default = groups;
        description = "Groups that user belongs to.";
      };

      initialPassword = with lib; mkOption {
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
      hashedPassword = with lib; mkOption {
        type = with types; nullOr (either str path);
        default = hashedPassword;
        description = ''
          Hashed password or hashed password file.
          If the provided value is a path then it will be treated as a hashed
          password file, otherwise it is a hashed password.
          If `immutable' user option is enabled, this is mandatory.  Otherwise
          it is ignored and use `initialPassword' instead.
        '';
      };
    };
  };

  ## User config is only enabled if any one of the profiles is turned on
  enableUserConfig = libpix.anyEnable cfg.profiles;
  enabledUsers = libpix.filterEnable cfg.profiles;

  getPassword = user:
    if cfg.immutable then
      if lib.isString user.hashedPassword then
        { hashedPassword = user.hashedPassword; }
      else
        { hashedPasswordFile = toString user.hashedPassword; }
    else
      { initialPassword = user.initialPassword; };

  ## Handle pix.users.profiles.<name>
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

  ## Handle pix.users.profiles.<name>
  groupList = with lib; mapAttrs'
    (n: v: nameValuePair v.name {
      gid = v.id;
    })
    enabledUsers;

  ## Handle users.immutable
  mutableUsers = !cfg.immutable;

in {
  imports = with libpix; callAll args (listDir isNotDefaultNix ./.);
  options.pix.users = options;

  config = lib.mkIf enableUserConfig {
    assertions = [
      {
        assertion = cfg.immutable -> libpix.allAttrs (n: v: null != v.hashedPassword) enabledUsers;
        message = "Hashed password for normal users must be provided when immutable user is enabled.";
      }

      {
        assertion = !cfg.immutable -> libpix.allAttrs (n: v: null != v.initialPassword) enabledUsers;
        message = "Initial password for normal users must be provided.";
      }
    ];

    users.mutableUsers = mutableUsers;
    users.users = userList;
    users.groups = groupList;
  };
}
