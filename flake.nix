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

      path = let
        pixTop = p: ./__pix__ + "/${p}";
      in {
        ## Root directory can be accessed through `rice.outPath'
        dotfiles = ./__dots__;
        lib = pixTop "lib";
        devshells = pixTop "devshells";
        modules = pixTop "modules";
        homeModules = pixTop "homeModules";
        instances = pixTop "instances";
        overlays = pixTop "overlays";
        packages = pixTop "packages";
        templates = pixTop "templates";
      };

      libpix = (import path.lib specialArgs) // {
        /* Improvised functions
        */

        __pkgsOverlay = system: import nixpkgs {
          inherit system;
          overlays = lib.mapAttrsToList (n: v: v) self.outputs.overlays;
        };

        __call = extraArgs: libpix.call (specialArgs // extraArgs);

        /* Call all packages under the given path.

           Each package must be a function that returns a derivation by
           `pkgs.buildEnv', `nixpkgs.stdenv.mkDerivation', `pkgs.buildFHSEnv' or
           any other equivalent and can be called with `pkgs.callPackage'.

           Note: `default.nix' will be ignored.
          */
        __callPackage = callPackage: extraArgs: path: lib.mapAttrs
          (n: v: callPackage v (specialArgs // extraArgs))
          (with libpix; importAllNameMapped
            baseNameNoExt
            (listDir (n: t: isNotDefaultNix n t && isImportable n t) path));
      };

    in {
      /* Pix */
      path = path;
      lib = libpix;
      specialArgs = specialArgs;

      /* Expose my modules */
      nixosModules = {
        default = self.outputs.nixosModules.nixos;
        nixos = import (path.modules + "/nixos.nix");
        home-manager = import (path.modules + "/home-manager.nix");
      };

      /* Packages

         Related commands:
           nix build .#PACKAGE_NAME
           nix shell
           home-manager build|switch --flake .#NAME

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
      packages = with libpix; forSupportedSystems (system: __call { inherit system; } path.packages);

      /* Development Shells

         Related commands:
           nix develop .#SHELL_NAME
      */
      devShells = with libpix; forSupportedSystems (system: __call { inherit system; } path.devshells);

      /* Code Formatter

         Related commands:
           nix fmt

         Alternatively, `nixpkgs-fmt'
      */
      formatter = libpix.forSupportedSystems (system: nixpkgs.legacyPackages.${system}.alejandra);

      /* Overlays

         Imported by other flakes
      */
      overlays = libpix.__call {} path.overlays;

      /* Templates

         Related commands:
           nix flake init -t /path/to/this_config#TEMPLATE_NAME
      */
      templates = libpix.__call {} path.templates;

      /* NixOS Configurations

         Related commands:
           nixos-rebuild build|boot|switch|test --flake .#HOST_NAME
      */
      nixosConfigurations =
        let inc = ins: libpix.nixosTopModule specialArgs (path.instances + "/${ins}");
        in {
          Framework = inc "Framework-13";
          NUC = inc "NUC-Server";
        };

      /* Darwin Configurations

         Related commands:
           darwin-rebuild switch --flake .#HOST_NAME
      */
      darwinConfigurations =
        let inc = ins: libpix.darwinTopModule specialArgs (path.instances + "/${ins}");
        in {
          Macbook = inc "Macbook-13";
        };

      /* HomeManager Configurations

         Related commands:
           nix build .#homeConfigurations.SYSTEM.NAME.activationPackage

         NOTE: The Home Manager command:
           home-manager build|switch --flake .#NAME

         looks for `homeConfigurations.user' with pre-defined platform arch in
         user config.  This is not flexible.  Instead, this section is set to
         `homeConfigurations.arch.user' and mapped to
         `packages.arch.homeConfigurations.user' and the command will pick it
         from there automatically.
      */
      homeConfigurations = with libpix; forSupportedSystems (system:
        let inc = user: homeTopModule (flakeOverlaidPkgs system) specialArgs (path.homeModules + "/${user}");
        in {
          fang = inc "fang";
        }
      );
    };
}
