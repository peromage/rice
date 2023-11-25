{
  description = "Nix Rice";

  inputs = {
    ## Essential flakes
    nixpkgs.url = "github:nixos/nixpkgs/nixos-23.05";
    nixos-hardware.url = "github:nixos/nixos-hardware/master";
    lanzaboote.url = "github:nix-community/lanzaboote?ref=v0.3.0";
    home-manager.url = "github:nix-community/home-manager/release-23.05";
    home-manager.inputs.nixpkgs.follows = "nixpkgs";

    ## Other flakes
    nix-colors.url = "github:misterio77/nix-colors/main";
    dev-templates.url = "github:the-nix-way/dev-templates/main";
    nix-alien.url = "github:thiagokokada/nix-alien/master";
  };

  outputs = { self, nixpkgs, ... } @ inputs:
    let
      importWithRice = path: lib.callPackageWith rice (import path) {};
      lib = nixpkgs.lib;
      librice = rice.lib;
      rice = {
        inherit nixpkgs inputs;
        inherit (self) outputs;
        rice = rice; # Self reference
        toplevel = builtins.path { path = ./.; }; # Explicit copy
        lib = importWithRice ./lib;
      };

    in
      {
        ## Via: 'nix build', 'nix shell', etc.
        packages = librice.forSupportedSystems
          (system: import ./packages nixpkgs.legacyPackages.${system});

        ## Via: 'nix fmt'
        ## Other options beside 'alejandra' include 'nixpkgs-fmt'
        formatter = librice.forSupportedSystems
          (system: nixpkgs.legacyPackages.${system}.alejandra);

        devShells = librice.forSupportedSystems
          (system: import ./shells (import nixpkgs {
            inherit system;
            config = {
              allowUnfree = true;
              allowBroken = true;
            };
          }));

        overlays = import ./overlays { inherit inputs; };

        templates = inputs.dev-templates.templates; # I'm lazy

        # Via: 'nixos-rebuild --flake .#host'
        nixosConfigurations = {
          framepie = librice.nixosImport ./instances/framepie;
        };
      };
}
