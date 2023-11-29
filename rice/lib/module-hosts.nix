{ nixpkgs, ... }:

with nixpkgs.lib; {
  /* A shorthand to declare general host options.

     Type:
       mkHostPreset :: String -> AttrSet
  */
  mkHostPreset = name: {
    enable = mkEnableOption "Enable host preset ${name}";
    name = mkOption {
      type = types.str;
      default = name;
      description = "Default host name for ${name}";
    };
  };

  /* A shorthand to declare general host config.
     NOTE: cfg should align with the options set by mkHostPreset.

     Type:
       mkHostPresetConfig :: AttrSet -> AttrSet
  */
  mkHostPresetConfig = cfg: extra: mkIf cfg.enable ({
    networking.hostName = cfg.name;
  } // extra);
}
