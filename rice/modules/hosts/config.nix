### Host option handling

{ config, lib, rice, ... }:

let
  librice = rice.lib;
  cfg = config.rice.hosts;

in {
  config = let
    ## Handle hosts.hosts
    ## Must specify either hosts.hostName or a hosts.hosts.NAME
    finalHostName = librice.either
      cfg.hostName
      (lib.foldlAttrs
        (a: n: v: if null != v.name then v.name else n)
        null
        cfg.hosts);

  in {
    networking.hostName = assert ! builtins.isNull finalHostName; finalHostName;
  };
}
