{ nixpkgs, ... }:

with nixpkgs.lib; {
  mkHostPreset = name: {
    enable = mkEnableOption "Enable host preset ${name}";
    name = mkOption {
      type = types.str;
      default = name;
      description = "Default host name for ${name}";
    };
  };

  ## cfg should align with the options set by mkHostPreset
  mkHostPresetConfig = cfg: extra: mkIf cfg.enable ({
    networking.hostName = cfg.name;
  } // extra);
}
