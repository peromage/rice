/* Sample flake to demonstrate how to use pix in a nondeterministic way. */
{
  inputs = {
    #nixpkgs-2305.url = "github:nixos/nixpkgs/nixos-23.05";
    #pix.inputs.nixpkgs.follows = "nixpkgs-2305
    pix.url = "github:peromage/pix/master";
  };

  outputs = { self, pix, ... }:
    let
      lib = pix.inputs.nixpkgs.lib;

    in {
      nixosConfigurations.default = with lib; pix.nixosConfigurations.Framework.extraModules {
        pix.hosts.hostName = mkForce "Foo";
        pix.users.immutable = true;
        pix.users.profiles.fang.password = "mkpassword"; ## The hashed password can be generated by `mkpassword'.
      };
    };
}
