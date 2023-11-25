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
  };

  outputs = { self, nixpkgs, ... } @ inputs:
    let
      importWithRice = path: lib.callPackageWith rice (import path) {};
      lib = nixpkgs.lib;
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
        packages = rice.lib.forSupportedSystems
          (system: import ./packages nixpkgs.legacyPackages.${system});

        ## Via: 'nix fmt'
        ## Other options beside 'alejandra' include 'nixpkgs-fmt'
        formatter = rice.lib.forSupportedSystems
          (system: nixpkgs.legacyPackages.${system}.alejandra);

        overlays = import ./overlays { inherit inputs; };
        templates = inputs.dev-templates.templates; # I'm lazy

        # Via: 'nixos-rebuild --flake .#host'
        nixosConfigurations = {
          framepie = rice.lib.nixosImport ./instances/framepie;
        };
      };
}
