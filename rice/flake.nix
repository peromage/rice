{
  description = "Nix Rice";

  inputs = {
    ## Essential flakes
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

    ## For Mac
    nixdarwin = {
      url = "github:LnL7/nix-darwin/master";
      inputs.nixpkgs.follows = "nixpkgs";
    };

    ## Other flakes
    nix-colors.url = "github:misterio77/nix-colors/main";

    nix-alien = {
      url = "github:thiagokokada/nix-alien/master";
      inputs.nixpkgs.follows = "nixpkgs";
    };

    ## Some useful flakes (inspected by `nix flake show <url>')
    # urls = [
    #   "github:the-nix-way/dev-templates/main"
    # ];
  };

  outputs = { self, nixpkgs, ... } @ inputs:
    let
      lib = nixpkgs.lib;
      librice = rice.lib;
      outputs = self.outputs;
      rice = {
        inherit nixpkgs inputs outputs;
        rice = rice; # Self reference
        toplevel = builtins.path { path = ./.; }; # Explicit copy
        lib = import ./lib rice;
      };

    in with librice; {
      ## Expose my lib
      lib.librice = librice;

      ## Expose my modules
      nixosModules = with lib; {
        default = import ./instances/framepie;
        hosts = importAllAsAttrs (allDirs ./modules/hosts);
        users = importAllAsAttrs (allDirs ./modules/users);
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

      ## Via: `nix build .#PACKAGE_NAME', `nix shell', etc.
      packages = callWithRice ./packages;

      ## Via: `nix fmt'
      ## Other options beside `alejandra' include `nixpkgs-fmt'
      formatter = forSupportedSystems (system: nixpkgs.legacyPackages.${system}.alejandra);

      ## Via: `nix develop .#SHELL_NAME'
      devShells = callWithRice ./devshells;

      ## Imported by other flakes
      overlays = callWithRice ./overlays;

      ## Via: `nix flake init -t /path/to/rice#TEMPLATE_NAME'
      templates = callWithRice ./templates;

      ## Via: `nixos-rebuild --flake .#HOST_NAME'
      nixosConfigurations = {
        Framepie = nixosTopModule ./instances/framepie;
      };

      ## Via: 'home-manager --flake .#name'
      homeConfigurations = {
        fang = homeTopModule [outputs.overlays.pkgsCustom] ./homes/fang/home.nix;
      };
    };
}
