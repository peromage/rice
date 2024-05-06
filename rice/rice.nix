{ nixpkgs, flake }:

let
  rice = self: {
    inherit nixpkgs flake;
    rice = self; # Self reference
    lib = import ./lib self;

    dirs = with self.dirs;
      let withTopLevel = p: "${topLevel}/${p}";
      in {
        topLevel = builtins.path { path = ./.; }; # Explicit copy
        devshells = withTopLevel "devshells";
        dotfiles = withTopLevel "dotfiles";
        instances = withTopLevel "instances";
        modules = withTopLevel "modules";
        overlays = withTopLevel "overlays";
        packages = withTopLevel "packages";
        templates = withTopLevel "templates";
      };

    withPkgsOverlays = system: with self; import nixpkgs {
      inherit system;
      overlays = with flake.outputs.overlays; [
        unrestrictedPkgs
        ricePkgs
      ];
    };

    override = args:
      let newRice = rice (newRice // args) // args;
      in newRice;
  };

  ## Make it easier to test so that we don't have to rely on the fix function from nixpkgs
  fix = f: let x = f x; in x;

in fix rice
