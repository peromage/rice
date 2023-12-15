{ config, lib, ... }:

let
  cfg = config.rice.services.documentation;

in with lib; {
  options.rice.services.documentation = {
    enable = mkEnableOption "documentation generation";
  };

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
