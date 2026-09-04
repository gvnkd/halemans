{
    inputs = {
        ihp.url = "github:digitallyinduced/ihp/v1.6";
        nixpkgs.follows = "ihp/nixpkgs";
        nixpkgs-nixos.follows = "ihp/nixpkgs-nixos";
        flake-parts.follows = "ihp/flake-parts";
        devenv.follows = "ihp/devenv";
        systems.follows = "ihp/systems";
        devenv-root = {
            url = "file+file:///dev/null";
            flake = false;
        };
    };

    outputs = inputs@{ self, nixpkgs, nixpkgs-nixos, ihp, flake-parts, systems, ... }:
        flake-parts.lib.mkFlake { inherit inputs; } {

            systems = import systems;
            imports = [ ihp.flakeModules.default ];

            perSystem = { pkgs, config, lib, ... }: {
                # Smoke check (milestone 0 §6) lives in ./nix/checks.nix.
                checks = import ./nix/checks.nix { inherit pkgs lib config self; ihpLib = inputs.ihp.packages.${pkgs.system}.ihp-env-var-backwards-compat; };

                ihp = {
                    appName = "app"; # Change this to your project name
                    enable = true;
                    projectPath = ./.;
                    packages = with pkgs; [
                        # Native dependencies, e.g. imagemagick
                    ];
                    haskellPackages = p: with p; [
                        # Haskell dependencies go here
                        p.ihp
                        base
                        wai
                        text
                        aeson
                        lens
                        vector
                        wreq
                        ihp-typed-sql
                        # milestone 1: websocket fan-out + web push
                        ihp-pglistener
                        cryptonite
                        memory
                        http-client
                        http-client-tls
                        http-types
                        base64-bytestring
                        # ihp-mail           # Email support: https://ihp.digitallyinduced.com/Guide/mail.html
                        # ihp-datasync       # Real-time DataSync
                        # ihp-job-dashboard  # Job dashboard UI
                        # ihp-typed-sql      # Type-safe SQL queries
                        # ihp-pglistener     # PostgreSQL LISTEN/NOTIFY
                    ];
                    devHaskellPackages = p: with p; [
                        cabal-install
                        hlint
                        hspec
                        ihp-hspec
                        process
                    ];

                    # Hoogle documentation server (enabled by default on port 8002)
                    # withHoogle = false; # Disable to save memory

                    # Disable relation type machinery for faster compilation.
                    # Coding agents usually don't need this because they use typedSql instead.
                    # Human-written app code may prefer fetchRelated/Include; set this to true in that case.
                    relationSupport = false;

                    # Skip tests/haddock for specific packages to speed up builds
                    # dontCheckPackages = [ "my-package" ];
                    # doJailbreakPackages = [ "my-package" ];
                    # dontHaddockPackages = [ "my-package" ];

                    # Production build tuning
                    # optimizationLevel = "2"; # Default: "1", use "2" for more optimized production binaries
                    # rtsFlags = "-A96m -N"; # GHC runtime flags for compiled binaries

                    # Mount additional directories under /static/ in production builds
                    # static.extraDirs = {
                    #     # Frontend = self.packages.${system}.frontend;
                    # };
                    # static.makeBundling = true; # Set false if not using Makefile for CSS/JS bundling
                };

                # Custom configuration that will start with `devenv up`.
                # All custom nix code lives in ./nix/ (see design_docs/milestone_0.md).
                devenv.shells.default = {
                    imports = [ ./nix/devenv.nix ];
                };
            };

            # Adding the new NixOS configuration for "production"
            # See https://ihp.digitallyinduced.com/Guide/deployment.html#deploying-with-deploytonixos for more info
            # Used to deploy the IHP application
            flake.nixosConfigurations."production" = import ./Config/nix/hosts/production/host.nix { inherit inputs; };
        };

    # The following configuration speeds up build times by using the devenv, cachix and digitallyinduced binary caches
    # You can add your own cachix cache here to speed up builds. For that uncomment the following lines and replace `CHANGE-ME` with your cachix cache name
    nixConfig = {
        extra-substituters = [
            "https://devenv.cachix.org"
            "https://cachix.cachix.org"
            "https://digitallyinduced.cachix.org"
            "https://cache.digitallyinduced.com/public"
            # "https://CHANGE-ME.cachix.org"
        ];
        extra-trusted-public-keys = [
            "devenv.cachix.org-1:w1cLUi8dv3hnoSPGAuibQv+f9TZLr6cv/Hm9XgU50cw="
            "cachix.cachix.org-1:eWNHQldwUO7G2VkjpnjDbWwy4KQ/HNxht7H4SSoMckM="
            "digitallyinduced.cachix.org-1:y+wQvrnxQ+PdEsCt91rmvv39qRCYzEgGQaldK26hCKE="
            "public:kR6JCoqAIMaO4s+EdDGh+jsHEHnoLq4ZLJPMCo0hcIQ="
            # "CHANGE-ME.cachix.org-1:CHANGE-ME-PUBLIC-KEY"
        ];
    };
}
