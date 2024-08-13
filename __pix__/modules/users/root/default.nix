{ ... }:
{ config, lib, pix, ... }:

let
  libpix = pix.lib;

  userCfg = config.pix.users;
  cfg = config.pix.users.root;

  options = {
    enable = with lib; mkOption {
      type = types.bool;
      default = true;
      description = ''
        Enable root user access.  This is on by default.
        Note that if this is disabled both `initialPassword' and `hashedPassword'
        will be ignored.
        If root access needs to be controlled by a custom password, do not
        disable this option.
      '';
    };

    initialPassword = with lib; mkOption {
      type = types.str;
      default = "P@55w0rd";
      description = ''
        Initial password for root if `immutable' user option is disabled.  In
        that case this option is mandatory.
        If `immutable' user option is enabled, this will be ignored and
        `hashedPassword' must be supplied.
      '';
    };

    hashedPassword = with lib; mkOption {
      type = with types; nullOr (either str path);
      default = null;
      description = ''
        Hashed password or hashed password file.
        If the provided value is a path then it will be treated as a hashed
        password file, otherwise it is a hashed password.
        If `immutable' user option is enabled and root user is enabled, this is
        mandatory.  Otherwise it is ignored and use `initialPassword' instead.
        Note that if `enable' option is off for root, both `initialPassword'
        and `hashedPassword' will be ignored.
      '';
    };
  };

  password =
    if userCfg.immutable then
      if lib.isString cfg.hashedPassword then
        { hashedPassword = cfg.hashedPassword; }
      else
        { hashedPasswordFile = toString cfg.hashedPassword; }
    else
      { initialPassword = cfg.initialPassword; };

  disabledPassword = {
    ## Do not disable root if a custom hashed password needs to be used
    hashedPassword = "**DISABLED**";
  };

in {
  options.pix.users.root = options;

  config = with lib; mkIf cfg.enable {
    assertions = singleton {
      assertion = userCfg.immutable -> null != cfg.hashedPassword;
      message = "Hashed password for root must be provided when immutable user is enabled.";
    };

    users.users.root = if cfg.enable then password else disabledPassword;
  };
}
