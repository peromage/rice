{ nixpkgs, flake }:

let
  rice = self: {
    passthrough = {
      inherit nixpkgs flake;
      flakeInputs = flake.inputs;
      flakeOutputs = flake.outputs;
      rice = self; # Self reference
      librice = self.lib;
    };

    lib = import ./lib self.passthrough;

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

    override = args: let newRice = rice (newRice // args) // args; in newRice;
  };

  ## Make it easier to test so that we don't have to rely on the fix function from nixpkgs
  fix = f: let x = f x; in x;

in fix rice
