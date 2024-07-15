/* Sample flake.

   This is to demostrate how to use pix configuration in a nondeterministic way.
   Do not build this directly on your setup as it will fail.
*/

{
  inputs = {
    conf.url = "github:peromage/pew/master?dir=pix";

    /* Override the release version */
    #nixpkgs-2305.url = "github:nixos/nixpkgs/nixos-23.05";
    #conf.inputs.nixpkgs.follows = "nixpkgs-2305";
  };

  outputs = { self, conf }:
    let
      lib = conf.inputs.nixpkgs.lib;

    in {
    nixosConfigurations = {
      FramepieAlter = conf.nixosConfigurations.Framepie.extraModule {
        pix.hosts.hostName = conf.pix.nixpkgs.lib.mkForce "FramepieAlter";
        pix.users.immutable = true;
        pix.users.profiles.fang.hashedPassword = "secret";
      };

      Foobar = lib.nixosSystem {
        specialArgs = conf.specialArgs;
        modules = [
          conf.nixosModules.default
          /* Your modules */
        ];
      };
    };
  };
}
