{ ... }:
{ config, lib, ... }:

let
  cfg = config.rice.users.root;
  userCfg = config.rice.users;

  ## If immutable is enabled hashed password must be supplied
  hashedPassword = with lib; optionalAttrs cfg.immutable
    (assert null != cfg.hashedPassword;
      (if isString cfg.hashedPassword then {
        hashedPassword = cfg.hashedPassword;
      } else {
        hashedPasswordFile = cfg.hashedPassword;
      }));

  disabledPassword = {
    ## FIXME: Remove this plain password
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
    users.users.root = {
      initialPassword = cfg.initialPassword;
    } // (if cfg.disable then disabledPassword else hashedPassword);
  };
}
