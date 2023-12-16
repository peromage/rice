{ ... }:
{ config, lib, ... }:

let
  userCfg = config.rice.users;
  cfg = config.rice.users.root;

  options = with lib; {
    enable = mkOption {
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

    initialPassword = mkOption {
      type = types.str;
      default = "P@55w0rd";
      description = ''
        Initial password for root if `immutable' user option is disabled.  In
        that case this option is mandatory.
        If `immutable' user option is enabled, this will be ignored and
        `hashedPassword' must be supplied.
      '';
    };

    hashedPassword = mkOption {
      type = with types; nullOr (either str path);
      default = null;
      description = ''
        Hashed password or hashed password file.
        If `immutable' user option is enabled, this is mandatory.  Otherwise
        it is ignored and use `initialPassword' instead.
        Note that if `enable' option is off for root, both `initialPassword'
        and `hashedPassword' will be ignored.
      '';
    };
  };

  ## If immutable is enabled hashed password must be supplied
  password = if cfg.immutable then
    (assert null != cfg.hashedPassword;
      (if lib.isString cfg.hashedPassword then {
        hashedPassword = cfg.hashedPassword;
      } else {
        hashedPasswordFile = cfg.hashedPassword;
      })) else {
        initialPassword = cfg.initialPassword;
      };

  disabledPassword = {
    ## Do not disable root if a custom hashed password needs to be used
    hashedPassword = "**DISABLED**";
  };

in {
  options.rice.users.root = options;

  config = {
    users.users.root = if cfg.enable then password else disabledPassword;
  };
}
