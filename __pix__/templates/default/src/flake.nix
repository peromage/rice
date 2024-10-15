/* Sample flake to demonstrate how to use pix in a nondeterministic way. */
{
  inputs = {
    #nixpkgs-2305.url = "github:nixos/nixpkgs/nixos-23.05";
    #rice.inputs.nixpkgs.follows = "nixpkgs-2305
    rice.url = "github:peromage/rice/master";
  };

  outputs = { self, rice, ... }:
    let
      lib = rice.inputs.nixpkgs.lib;

    in {
      nixosConfigurations.default = with lib; rice.nixosConfigurations.Framework.extraModules {
        pix.hosts.hostName = mkForce "Foo";
        pix.users.immutable = true;
        pix.users.profiles.fang.password = "mkpassword"; ## The hashed password can be generated by `mkpassword'.
      };
    };
}
