{ rice, ... }:

{
  imports = rice.lib.allWithFilter
    (n: v: "regular" == v && "default.nix" != n)
    ./.;
}
