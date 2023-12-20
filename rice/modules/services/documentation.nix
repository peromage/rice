{ config, lib, ... }:

let
  inherit (lib) mkEnableOption mkIf;

  cfg = config.rice.services.documentation;

  options = {
    enable = mkEnableOption "documentation generation";
  };

in {
  options.rice.services.documentation = options;

  config = mkIf cfg.enable {
    documentation = {
      enable = true;
      man.enable = true;
      info.enable = true;
      doc.enable = true;
      dev.enable = true;
      nixos = {
        enable = true;
        includeAllModules = true;
      };
    };
  };
}
