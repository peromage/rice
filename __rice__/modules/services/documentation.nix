{ config, lib, ... }:

let
  cfg = config.rice.services.documentation;

  options = {
    enable = lib.mkEnableOption "documentation generation";
  };

in {
  options.rice.services.documentation = options;

  config = lib.mkIf cfg.enable {
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
