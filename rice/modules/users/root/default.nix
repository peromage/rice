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
      description = "Disable root user access.";
    };

    initialPassword = mkOption {
      type = types.str;
      default = "P@55w0rd";
      description = "Root user default password.";
    };

    hashedPassword = mkOption {
      type = with types; nullOr (either str path);
      default = null;
      description = "Hashed password or hashed password file (Used when immutable is enabled).";
    };
  };

  config = {
    users.users.root = if cfg.disable then disabledPassword else password;
  };
}
