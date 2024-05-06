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

  outputs = { self, nixpkgs, ... } @ inputs:
    let
      inherit (rice.lib)
        importListAsAttrs filterDir isDirType callWithRice mergeAttrsFirstLevel
        forSupportedSystems nixosTopModule homeTopModule darwinTopModule;
      inherit (nixpkgs.lib) mapAttrs mapAttrsToList;

      outputs = self.outputs;
      rice = import ./rice.nix { inherit nixpkgs; flake = self; };

      withPkgsAllOverlays = system: import nixpkgs { inherit system; overlays = mapAttrsToList (n: v: v) outputs.overlays; };

    in {
      /* Expose rice */
      rice = rice;

      /* Expose my modules */
      nixosModules =
        let importDirs = dir: importListAsAttrs (filterDir isDirType dir);
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
      packages = mergeAttrsFirstLevel [
        (callWithRice ./packages)
        (mapAttrs
          ## Fake derivation to enable `nix flake show'
          (n: v: { homeConfigurations = v // { type = "derivation"; name = "homeConfigurations"; }; })
          outputs.homeConfigurations)
      ];

      /* Via: `nix fmt'

         Other options beside `alejandra' include `nixpkgs-fmt'
      */
      formatter = forSupportedSystems (system: nixpkgs.legacyPackages.${system}.alejandra);

      /* Via: `nix develop .#SHELL_NAME' */
      devShells = callWithRice ./devshells;

      /* Imported by other flakes */
      overlays = callWithRice ./overlays;

      /* Via: `nix flake init -t /path/to/rice#TEMPLATE_NAME' */
      templates = callWithRice ./templates;

      /* Via: `nixos-rebuild { build | boot | switch | test } --flake .#HOST_NAME' */
      nixosConfigurations = {
        Framepie = nixosTopModule ./modules/instances/Framepie;
        Chicken65 = nixosTopModule ./modules/instances/Chicken65;
      };

      /* Via: `darwin-rebuild switch --flake .#HOST_NAME' */
      darwinConfigurations = {
        Applepie = darwinTopModule ./modules/instances/Applepie;
      };

      /* Via: `nix build .#homeConfigurations.SYSTEM.NAME.activationPackage'

         NOTE: The Home Manager command:
           `home-manager { build | switch } --flake .#NAME'
         is actually implemented by the `packages' output not this.
      */
      homeConfigurations = forSupportedSystems (system:
        let inc = homeTopModule (rice.withPkgsAllOverlays system);
        in {
          fang = inc ./modules/homes/fang;
        }
      );
    };
}
