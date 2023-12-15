{ ... }:
{ config, lib, ... }:

let
  cfg = config.rice.users.root;
  userCfg = config.rice.users;

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
  options.rice.users.root = with lib; {
    disable = mkOption {
      type = types.bool;
      default = false;
      description = ''
        Disable root user access.
        Note that if this is enabled both `initialPassword' and `hashedPassword'
        will be ignored.
        If root access needs to be controlled by a custom password, do not enable
        this option.
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
        Note that if `disable' option is enabled for root, both `initialPassword'
        and `hashedPassword' will be ignored.
      '';
    };
  };

  config = {
    users.users.root = if cfg.disable then disabledPassword else password;
  };
}
