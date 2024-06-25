{
  description = "Nix Rice";

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
      lib = nixpkgs.lib;
      librice = rice.lib;

      /* Blend flake inputs and outputs with additional utilities.

         This is usually used by modules.
      */
      myArgs = self.inputs // self.outputs // {
        inherit pkgsWithMyOverlays callWith callPackages;
      };

      rice = (import ./rice.nix {}).override {
        flake = self;

        dirs = {
          topLevel = ./.;
          devshells = ./devshells;
          dotfiles = ./dotfiles;
          instances = ./instances;
          modules = ./modules;
          overlays = ./overlays;
          packages = ./packages;
          templates = ./templates;
        };

        lib = import ./lib myArgs;
      };

      pkgsWithMyOverlays = system: import nixpkgs {
        inherit system;
        overlays = lib.mapAttrsToList (n: v: v) self.outputs.overlays;
      };

      callWith = extraArgs: librice.callWithArgs (myArgs // extraArgs);

      /* Call all packages under the given path.

         Note: `default.nix' will be ignored.
      */
      callPackages = callPackage: extraArgs: path: lib.mapAttrs
        (n: v: callPackage v (myArgs // extraArgs))
        (librice.importAllNameMapped
          librice.baseNameNoExt
          (librice.listDir (n: t: librice.isNotDefaultNix n t && librice.isImportable n t) path));

    in {
      /* Expose rice */
      rice = rice;

      /* Expose my modules */
      nixosModules =
        let importDirs = dir: with librice; importListAsAttrs (filterDir isDirType dir);
        in {
          main = import ./modules;
          instances = importDirs ./modules/instances;
          homes = importDirs ./modules/homes;
        };

      /* Notice that there is a minor difference between `packages' and `legacyPackages'.

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

      /* Via: `nix build .#PACKAGE_NAME', `nix shell', etc.

         NOTE: This also enables:
           `home-manager { build | switch } --flake .#NAME
      */
      packages = librice.mergeSetsFirstLevel [
        (callWith {} ./packages)
        (lib.mapAttrs
          ## Fake derivation to enable `nix flake show'
          (n: v: { homeConfigurations = v // { type = "derivation"; name = "homeConfigurations"; }; })
          self.outputs.homeConfigurations)
      ];

      /* Via: `nix fmt'

         Other options beside `alejandra' include `nixpkgs-fmt'
      */
      formatter = librice.forSupportedSystems (system: nixpkgs.legacyPackages.${system}.alejandra);

      /* Via: `nix develop .#SHELL_NAME' */
      devShells = librice.forSupportedSystems (system: callWith { inherit system; } ./devshells);

      /* Imported by other flakes */
      overlays = callWith {} ./overlays;

      /* Via: `nix flake init -t /path/to/rice#TEMPLATE_NAME' */
      templates = callWith {} ./templates;

      /* Via: `nixos-rebuild { build | boot | switch | test } --flake .#HOST_NAME' */
      nixosConfigurations = with librice; {
        Framepie = nixosTopModule ./modules/instances/Framepie;
        Chicken65 = nixosTopModule ./modules/instances/Chicken65;
      };

      /* Via: `darwin-rebuild switch --flake .#HOST_NAME' */
      darwinConfigurations = with librice; {
        Applepie = darwinTopModule ./modules/instances/Applepie;
      };

      /* Via: `nix build .#homeConfigurations.SYSTEM.NAME.activationPackage'

         NOTE: The Home Manager command:
           `home-manager { build | switch } --flake .#NAME'
         is actually implemented by the `packages' output not this.
      */
      homeConfigurations = with librice; forSupportedSystems (system:
        let inc = homeTopModule (pkgsWithMyOverlays system);
        in {
          fang = inc ./modules/homes/fang;
        }
      );
    };
}
