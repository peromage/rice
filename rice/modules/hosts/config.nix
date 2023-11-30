### Host option handling

{ config, lib, rice, ... }:

let
  librice = rice.lib;
  cfg = config.rice.hosts;

in {
  config = let
    /* Handle host name.
       The precedence of the host name specified in options is as follow:
           1. hosts.hostName
           2. hosts.<host>.name
           3. hosts.<host>

       Any one of them must be specified.
       If `hosts.hostName' exists, the rest of the options will be ignored.
       If no global hostName exists, the last host name in the set (in
       alphabetic order) will be used.
    */
    finalHostName = librice.either
      cfg.hostName
      (lib.foldlAttrs
        (a: n: v: librice.either' v.name n a)
        null
        cfg.hosts);

  in {
    networking.hostName = (assert null != finalHostName; finalHostName);
  };
}
