{
  description = "PIX - Peromage's nIX configuration";

  inputs = {
    /* Essential flakes */
    nixpkgs.url = "github:nixos/nixpkgs/nixos-unstable";

    nixos-hardware.url = "github:nixos/nixos-hardware/master";

    lanzaboote = {
      url = "github:nix-community/lanzaboote/master";
      inputs.nixpkgs.follows = "nixpkgs";
    };

    home-manager = {
      url = "github:nix-community/home-manager/master";
      inputs.nixpkgs.follows = "nixpkgs";
    };

    /* For Mac */
    nix-darwin = {
      url = "github:LnL7/nix-darwin/master";
      inputs.nixpkgs.follows = "nixpkgs";
    };

    /* Other flakes */
    nix-colors.url = "github:misterio77/nix-colors/main";

    nix-alien = {
      url = "github:thiagokokada/nix-alien/master";
      inputs.nixpkgs.follows = "nixpkgs";
    };

    /* Some useful flakes (inspected by `nix flake show URL')

       github:the-nix-way/dev-templates/main
    */
  };

  outputs = { self, nixpkgs, ... }:
    let
      /* All flakes including this one */
      specialArgs = self.inputs // { pix = self.outputs; };
      lib = nixpkgs.lib;

      paths = let
        withTop = p: ./__pix__ + "/${p}";
      in {
        topLevel = ./.;
        lib = withTop "lib";
        devshells = withTop "devshells";
        dotfiles = withTop "dotfiles";
        modules = withTop "modules";
        homes = withTop "homeConfigs";
        instances = withTop "instanceConfigs";
        overlays = withTop "overlays";
        packages = withTop "packages";
        templates = withTop "templates";
      };

      libpix = (import paths.lib specialArgs) // {
        /* Improvised functions
        */

        flakeOverlaidPkgs = system: import nixpkgs {
          inherit system;
          overlays = lib.mapAttrsToList (n: v: v) self.outputs.overlays;
        };

        flakeCall = extraArgs: libpix.call (specialArgs // extraArgs);

        /* Call all packages under the given path.

             Each package must be a function that returns a derivation by
             `pkgs.buildEnv', `nixpkgs.stdenv.mkDerivation', `pkgs.buildFHSEnv' or
             any other equivalent and can be called with `pkgs.callPackage'.

             Note: `default.nix' will be ignored.
          */
        flakeCallPackages = callPackage: extraArgs: path: lib.mapAttrs
          (n: v: callPackage v (specialArgs // extraArgs))
          (with libpix; importAllNameMapped
            baseNameNoExt
            (listDir (n: t: isNotDefaultNix n t && isImportable n t) path));
      };

    in {
      /* Pix */
      paths = paths;
      lib = libpix;
      specialArgs = specialArgs;

      /* Expose my modules */
      nixosModules = {
        default = import paths.modules;
      };

      /* Via: `nix build .#PACKAGE_NAME', `nix shell', etc.

         NOTE: This also enables:
           `home-manager { build | switch } --flake .#NAME

         Notice that there is a minor difference between `packages' and `legacyPackages'.

         From: https://github.com/NixOS/nixpkgs/blob/b2e41a5bd20d4114f27fe8d96e84db06b841d035/flake.nix#L47

         The "legacy" in `legacyPackages` doesn't imply that the packages exposed
         through this attribute are "legacy" packages. Instead, `legacyPackages`
         is used here as a substitute attribute name for `packages`. The problem
         with `packages` is that it makes operations like `nix flake show
         nixpkgs` unusably slow due to the sheer number of packages the Nix CLI
         needs to evaluate. But when the Nix CLI sees a `legacyPackages`
         attribute it displays `omitted` instead of evaluating all packages,
         which keeps `nix flake show` on Nixpkgs reasonably fast, though less
         information rich.
      */
      packages = with libpix; forSupportedSystems (system: flakeCall { inherit system; } paths.packages);

      /* Via: `nix develop .#SHELL_NAME' */
      devShells = with libpix; forSupportedSystems (system: flakeCall { inherit system; } paths.devshells);

      /* Via: `nix fmt'

         Other options beside `alejandra' include `nixpkgs-fmt'
      */
      formatter = libpix.forSupportedSystems (system: nixpkgs.legacyPackages.${system}.alejandra);

      /* Imported by other flakes */
      overlays = libpix.flakeCall {} paths.overlays;

      /* Via: `nix flake init -t /path/to/this_config#TEMPLATE_NAME' */
      templates = libpix.flakeCall {} paths.templates;

      /* Via: `nixos-rebuild { build | boot | switch | test } --flake .#HOST_NAME' */
      nixosConfigurations =
        let inc = ins: libpix.nixosTopModule specialArgs (paths.instances + "/${ins}");
        in {
          Framepie = inc "Framepie";
          Chicken65 = inc "Chicken65";
        };

      /* Via: `darwin-rebuild switch --flake .#HOST_NAME' */
      darwinConfigurations =
        let inc = ins: libpix.darwinTopModule specialArgs (paths.instances + "/${ins}");
        in {
          Applepie = inc "Applepie";
        };

      /* Via: `nix build .#homeConfigurations.SYSTEM.NAME.activationPackage'

         NOTE: The Home Manager command:
           `home-manager { build | switch } --flake .#NAME'
         is actually implemented by the `packages' output not this.
      */
      homeConfigurations = with libpix; forSupportedSystems (system:
        let inc = home: homeTopModule (flakeOverlaidPkgs system) specialArgs (paths.homes + "/${home}");
        in {
          fang = inc "fang";
        }
      );
    };
}
