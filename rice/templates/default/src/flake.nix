/* Do not use this directly.
*/

{
  inputs = {
    conf.url = "github:peromage/pew/master?dir=rice";

    ## Dynamic binding
    #nixpkgs-2305.url = "github:nixos/nixpkgs/nixos-23.05";
    #conf.inputs.nixpkgs.follows = "nixpkgs-2305";
  };
  outputs = { self, conf }: {
    nixosConfigurations = {
      Foobar = conf.nixosConfigurations.Framepie.extraModule {
        rice.hosts.hostName = conf.rice.nixpkgs.lib.mkForce "Foobar";
        rice.users.immutable = true;
        rice.users.profiles.fang.hashedPassword = "secret";
      };
    };
  };
}
