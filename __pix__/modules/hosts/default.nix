{ config, lib, pix, ... }:

let
  libpix = pix.lib;
  cfg = config.pix.hosts;

  hostProfileOptions = {name, config, ...}: {
    options = with lib; {
      enable = mkEnableOption "host ${name}";

      config = mkOption {
        type = types.attrs;
        default = {};
        description = ''
          Host detailed configurations.
          The content of this option should be the normal toplevel NixOS config.
        '';
      };
    };
  };

in {
  imports = with libpix; listDir isNotDefaultNix ./.;

  /* Interface */
  options.pix.hosts = with lib; {
    hostName = mkOption {
      type = with types; nullOr str;
      default = null;
      description = ''
        Host name for this machine.
        This takes precedence over the name defined in each host profile.
        If this is not defined, the name of last enabled host profile will be
        used.
      '';
    };

    platform = mkOption {
      type = with types; nullOr str;
      default = null;
      description = ''
        Host platform architecture.
        For clarification, this needs to be specified explicitly.
      '';
    };

    profiles = mkOption {
      type = with types; attrsOf (submodule hostProfileOptions);
      default = {};
      description = ''
        Host profile definitions.
      '';
    };
  };

  /* Implementation */
  config = let
    enabledHosts = lib.filterAttrs (name: config: config.enable) cfg.profiles;
    anyHostEnabled = lib.length enabledHosts != 0;

    /* Use the value from `pix.hosts.hostName' if defined.
       Otherwise, use the name of last host profile.
    */
    hostName = with lib;
      if cfg.hostName != null then cfg.hostName
      else foldl (_: config: config.name) null enableHosts;

    allowedNames = ["fonts" "programs" "environment"];

  in
    with lib; mkMerge [
      (libpix.mkMergeTopLevel allowedNames (mapAttrsToList (_: config: config.config) enabledHosts))

      {
        ## Common
        nixpkgs.hostPlatform = cfg.platform;
        networking.hostName = hostName;

        ## Assertions
        assertions = [
          {
            assertion = cfg.platform != null;
            message = "Platform has be explicitly specified.";
          }

          {
            assertion = hostName != null;
            message = ''
              No hostname provided.
              Either `pix.hosts.hostName' is not set or no host profile is enabled."
            '';
          }

          ## Only allowed options can be defined
          {
            assertion = all id (flatten
              (mapAttrsToList
                (_: config: (map
                  (n: elem n allowedNames)
                  (attrNames config.config)))
                enabledHosts));
            message = "Only these config names can be defined in host profiles: ${toString allowedNames}.";
          }
        ];
      }
    ];
}
