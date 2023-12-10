{ rice, ... }:

{
  imports = rice.lib.allButDefault ./.;

  ## Default host name
  rice.hosts.hosts.potpie.name = "Potpie";
}
