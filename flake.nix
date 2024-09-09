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

  outputs = { self, nixpkgs, home-manager, nix-darwin, ... }:
    let
      lib = nixpkgs.lib;
      libpix = (import path.lib { inherit nixpkgs; });
      pixArgs = self.inputs // { pix = self; };

      license = lib.licenses.gpl3Plus;
      maintainer = {
        name = "Fang Deng";
        email = "fang@elfang.com";
        github = "fangtfs";
        githubId = 10389606;
      };

      path = let
        pixTop = p: ./__pix__ + "/${p}";
      in {
        ## Root directory can be accessed through `rice.outPath'
        dotfiles = ./__pot__;
        emacs = ./.;
        lib = pixTop "lib";
        devshells = pixTop "devshells";
        nixosModules = pixTop "modules";
        homeManagerModules = pixTop "homeModules";
        nixosConfigurations = pixTop "configurations/nixos";
        darwinConfigurations = pixTop "configurations/darwin";
        homeConfigurations = pixTop "configurations/home";
        overlays = pixTop "overlays";
        packages = pixTop "packages";
        templates = pixTop "templates";
      };

      supportedSystems = {
        amd64_pc = "x86_64-linux";
        amd64_mac = "x86_64-darwin";
        arm64_pc = "aarch64-linux";
        arm64_mac = "aarch64-darwin";
      };

      ## Improvised functions
      imp = {
        forSupportedSystems = lib.genAttrs (lib.attrValues supportedSystems);

        pkgsWithOverlay = system: import nixpkgs {
          inherit system;
          overlays = lib.mapAttrsToList (n: v: v) self.outputs.overlays;
        };

        callWithPix = args: libpix.call (pixArgs // args);

        /* Note that the `system' attribute is not explicitly set (default to null)
           to allow modules to set it themselves.  This allows a hermetic configuration
           that doesn't depend on the system architecture when it is imported.
           See: https://github.com/NixOS/nixpkgs/pull/177012
        */
        mkNixOS = name: libpix.mkConfiguration lib.nixosSystem (modules: {
          specialArgs = pixArgs;
          modules = modules;
        }) (path.nixosConfigurations + "/${name}");

        mkDarwin = name: libpix.mkConfiguration nix-darwin.lib.darwinSystem (modules: {
          specialArgs = pixArgs;
          modules = modules;
        }) (path.darwinConfigurations + "/${name}");

        mkHome = name: pkgs: libpix.mkConfiguration home-manager.lib.homeManagerConfiguration (modules: {
          inherit pkgs;
          extraSpecialArgs = pixArgs;
          modules = modules;
        }) (path.homeConfigurations + "/${name}");
      };

    in {
      /* Pix */
      inherit license maintainer path supportedSystems imp;
      lib = libpix;
    } // (with imp; {
      /* Expose my modules */
      nixosModules = {
        default = self.outputs.nixosModules.nixos;
        nixos = import path.nixosModules;
        home-manager = import path.homeManagerModules;
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
      packages = forSupportedSystems (system: callWithPix { pkgs = pkgsWithOverlay system; } path.packages);

      /* Development Shells

         Related commands:
           nix develop .#SHELL_NAME
      */
      devShells = forSupportedSystems (system: callWithPix { pkgs = pkgsWithOverlay system; } path.devshells);

      /* Code Formatter

         Related commands:
           nix fmt

         Alternatively, `nixpkgs-fmt'
      */
      formatter = forSupportedSystems (system: nixpkgs.legacyPackages.${system}.alejandra);

      /* Overlays

         Imported by other flakes
      */
      overlays = callWithPix {} path.overlays;

      /* Templates

         Related commands:
           nix flake init -t /path/to/this_config#TEMPLATE_NAME
      */
      templates = callWithPix {} path.templates;

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
        fang_pc = mkHome "fang" (pkgsWithOverlay supportedSystems.amd64_pc);
      };
    });
}
