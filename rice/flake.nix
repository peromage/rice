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
    nixdarwin.url = "github:LnL7/nix-darwin/master";

    darwin-home-manager = {
      url = "github:nix-community/home-manager/master";
      inputs.nixpkgs.follows = "nixdarwin";
    };

    ## Other flakes
    nix-colors.url = "github:misterio77/nix-colors/main";

    dev-templates = {
      url = "github:the-nix-way/dev-templates/main";
      inputs.nixpkgs.follows = "nixpkgs";
    };

    nix-alien = {
      url = "github:thiagokokada/nix-alien/master";
      inputs.nixpkgs.follows = "nixpkgs";
    };
  };

  outputs = { self, nixpkgs, ... } @ inputs:
    let
      lib = nixpkgs.lib;
      librice = rice.lib;
      rice = {
        inherit nixpkgs inputs;
        inherit (self) outputs;
        rice = rice; # Self reference
        toplevel = builtins.path { path = ./.; }; # Explicit copy
        lib = import ./lib rice;
      };

    in
      {
        ## Via: `nix build .#PACKAGE_NAME', `nix shell', etc.
        packages = librice.forSupportedSystems
          (system: import ./packages nixpkgs.legacyPackages.${system});

        ## Via: `nix fmt'
        ## Other options beside `alejandra' include `nixpkgs-fmt'
        formatter = librice.forSupportedSystems
          (system: nixpkgs.legacyPackages.${system}.alejandra);

        ## Via: `nix develop .#SHELL_NAME'
        devShells = librice.forSupportedSystems
          (system: import ./devshells (import nixpkgs {
            inherit system;
            overlays = [ rice.outputs.overlays.unrestricted-packages ];
          }));

        ## Imported by other flakes
        overlays = librice.importWithRice ./overlays;

        ## Via: `nix flake init -t /path/to/rice#TEMPLATE_NAME'
        templates = inputs.dev-templates.templates; # I'm lazy

        ## Via: `nixos-rebuild --flake .#HOST_NAME'
        nixosConfigurations = {
          framepie = librice.importNixOS ./instances/framepie;
        };
      };
}
