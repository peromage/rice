{
  description = "PIX - Peromage's nIX configuration";

  inputs = {
    /* Common flakes */
    nixpkgs.url = "github:nixos/nixpkgs/nixos-unstable";
    nixos-hardware.url = "github:nixos/nixos-hardware/master";
    lanzaboote = { url = "github:nix-community/lanzaboote/master"; inputs.nixpkgs.follows = "nixpkgs"; };
    home-manager = { url = "github:nix-community/home-manager/master"; inputs.nixpkgs.follows = "nixpkgs"; };

    /* Mac specialized */
    nix-darwin = { url = "github:LnL7/nix-darwin/master"; inputs.nixpkgs.follows = "nixpkgs"; };

    /* Some useful flakes */
    # nix-colors = { url = "github:misterio77/nix-colors/main"; inputs.nixpkgs.follows = "nixpkgs"; };
    # nix-alien = { url = "github:thiagokokada/nix-alien/master"; inputs.nixpkgs.follows = "nixpkgs"; };
  };

  outputs = { self, nixpkgs, home-manager, nix-darwin, ... }:
    let
      /* Meta */
      license = lib.licenses.gpl3Plus;
      maintainer = {
        name = "Fang Deng";
        email = "fang@elfang.com";
        github = "fangtfs";
        githubId = 10389606;
      };

      lib = nixpkgs.lib;
      libpix = (import path.lib { inherit nixpkgs; });
      pix = self;

      path = {
        ## Root directory can be accessed through `rice.outPath'
        dotfiles = ./dotfiles;
        lib = "./lib";
        devshells = "./devshells";
        nixosModules = "./nixosModules";
        homeManagerModules = "./homeManagerModules";
        nixosConfigurations = "./configurations/nixos";
        darwinConfigurations = "./configurations/darwin";
        homeConfigurations = "./configurations/home";
        overlays = "./overlays";
        packages = "./packages";
        templates = "./templates";
      };

      supportedSystems = {
        amd64_pc = "x86_64-linux";
        amd64_mac = "x86_64-darwin";
        arm64_pc = "aarch64-linux";
        arm64_mac = "aarch64-darwin";
      };

      /* Improvised functions */
      imp = with self.imp; {
        forSupportedSystems = lib.genAttrs (lib.attrValues supportedSystems);

        mkPkgs = system: import nixpkgs {
          inherit system;
          overlays = with self.outputs.overlays; [ unrestrictedPkgs pixPkgs ];
        };

        mkImport = args: libpix.call (args // { inherit pix; });

        /* Note that the `system' attribute is not explicitly set (default to null)
           to allow modules to set it themselves.  This allows a hermetic configuration
           that doesn't depend on the system architecture when it is imported.
           See: https://github.com/NixOS/nixpkgs/pull/177012
        */
        mkNixOS = name: libpix.mkConfiguration lib.nixosSystem (modules: {
          specialArgs = { inherit pix; };
          modules = modules ++ [{
            nixpkgs.overlays = with self.outputs.overlays; [ unrestrictedPkgs pixPkgs ];
          }];
        }) (path.nixosConfigurations + "/${name}");

        mkDarwin = name: libpix.mkConfiguration nix-darwin.lib.darwinSystem (modules: {
          specialArgs = { inherit pix; };
          modules = modules;
        }) (path.darwinConfigurations + "/${name}");

        mkHome = name: system: let
          pkgs = mkPkgs system;
        in libpix.mkConfiguration home-manager.lib.homeManagerConfiguration (modules: {
          inherit pkgs;
          extraSpecialArgs = { inherit pix; };
          modules = modules;
        }) (path.homeConfigurations + "/${name}");
      };

    in {
      /* Pix */
      inherit license maintainer path supportedSystems imp pix;
      lib = libpix;

    } // (with imp; {

      /* Expose modules

         NOTE: Both `nixos' and `homeManager' module require an additional `pix'
         argument (I.E. this flake).  Don't forget to pass it in the `specialArgs'
         when importing them.  This is to bypass the infinite recursion problem
         where these modules are written in self-contained way.
      */
      nixosModules = {
        default = self.outputs.nixosModules.nixos;
        nixos = import path.nixosModules;
        homeManager = import path.homeManagerModules;
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
      packages = forSupportedSystems (system: mkImport { pkgs = mkPkgs system; } path.packages);

      /* Development Shells

         Related commands:
           nix develop .#SHELL_NAME
      */
      devShells = forSupportedSystems (system: mkImport { pkgs = mkPkgs system; } path.devshells);

      /* Code Formatter

         Related commands:
           nix fmt

         Alternatively, `nixpkgs-fmt'
      */
      formatter = forSupportedSystems (system: nixpkgs.legacyPackages.${system}.alejandra);

      /* Overlays

         Imported by other flakes
      */
      overlays = mkImport {} path.overlays;

      /* Templates

         Related commands:
           nix flake init -t /path/to/this_config#TEMPLATE_NAME
      */
      templates = mkImport {} path.templates;

      /* NixOS Configurations

         Related commands:
           nixos-rebuild build|boot|switch|test --flake .#HOST_NAME
      */
      nixosConfigurations = {
        Framework = mkNixOS "Framework-13";
        NUC = mkNixOS "NUC-Server";
      };

      /* Darwin Configurations

         Related commands:
           darwin-rebuild switch --flake .#HOST_NAME
      */
      darwinConfigurations = {
        Macbook = mkDarwin "Macbook-13";
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
      homeConfigurations = {
        fang_pc = mkHome "fang" supportedSystems.amd64_pc;
      };
    });
}
