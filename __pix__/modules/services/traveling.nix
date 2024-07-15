/* Settings while traveling to some restricted regions.

   For flakes the following options can be added to avoid problems during
   evaluation.

   For example:

   {
     nixConfig = {
       substituters = [
         "https://mirror.sjtu.edu.cn/nix-channels/store"
       ];
     };
   }
*/

{ config, lib, pix, ... }:

let
  libpix = pix.lib;

  cfg = config.pix.services.traveling;

  options = with lib; {
    region = mkOption {
      type = with types; nullOr (enum [ "China" ]);
      default = null;
      description = "Travel region.";
    };
  };


in {
  options.pix.services.traveling = options;

  config = libpix.mkMergeIf [
    {
      cond = "China" == cfg.region;
      as = with lib; {
        time.timeZone = mkForce "Asia/Shanghai";
        nix.settings.substituters = mkForce [
          # "https://mirrors.tuna.tsinghua.edu.cn/nix-channels/store"
          # "https://mirrors.ustc.edu.cn/nix-channels/store"
          "https://mirror.sjtu.edu.cn/nix-channels/store"
        ];
      };
    }
  ];
}
