{ self, nixpkgs, toplevel, rice, ... }:

let
  lib = nixpkgs.lib;

in with self; {
  ## Import a NixOS toplevel module
  buildNixOS = system: topModule: lib.nixosSystem {
    inherit system;
    specialArgs = { inherit rice system; };
    modules = [ topModule ];
  };

  ## Used to import an instance in a toplevel flake.
  importNixOS = importWithRice;

  ## Shorthand to get paths of NixOS modules relative to the toplevel.
  ## Usually used with `imports' block in a NixOS module.
  getModules = list: withPrefix (toplevel + "/modules/") list;

  ## Supported platforms
  forSupportedSystems = lib.genAttrs [
    "aarch64-linux"
    "x86_64-linux"
    "x86_64-darwin"
  ];

  ## General sudo user creation
  ## To extend the module created by this function, put the result into the
  ## `imports' list and do your customs.
  createSudoUser = name: uid: groups: {
    users.users.${name} = {
      isNormalUser = true;
      uid = uid;
      group = name;
      extraGroups = [ "wheel" ] ++ groups;
    };
    users.groups.${name} = {
      gid = uid;
    };
  };
}
