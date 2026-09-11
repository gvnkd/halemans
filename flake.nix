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

            # Name of the cachix cache CI pushes to; read by local tooling.
            flake.cachix.push = "halemans";

            perSystem = { pkgs, config, lib, ... }: {
                # Smoke check (milestone 0 §6) lives in ./nix/checks.nix.
                checks = import ./nix/checks.nix { inherit pkgs lib config self; ihpLib = inputs.ihp.packages.${pkgs.system}.ihp-env-var-backwards-compat; };

                packages = let
                    prodServer = config.packages.optimized-prod-server;
                    # Schema bootstrap bundle baked into the image at
                    # /share/db-init: IHP framework schema, app schema,
                    # standard roles, and the schema_migrations ledger for
                    # every migration already folded into Schema.sql
                    # (fresh-deploy bootstrap; upgrades are applied by
                    # /bin/db-migrate at container start).
                    dbInit = let
                        revisions = lib.mapAttrsToList (name: _: builtins.head (lib.splitString "-" name))
                            (lib.filterAttrs (name: type: type == "regular" && lib.hasSuffix ".sql" name)
                                (builtins.readDir ./Application/Migration));
                        migrationsSql = pkgs.writeText "99-schema-migrations.sql" ''
                            CREATE TABLE IF NOT EXISTS schema_migrations (revision BIGINT NOT NULL UNIQUE);
                            INSERT INTO schema_migrations (revision) VALUES
                                ${lib.concatMapStringsSep ", " (revision: "(${revision})") revisions}
                            ON CONFLICT DO NOTHING;
                        '';
                    in pkgs.runCommand "halemans-db-init" {} ''
                        mkdir -p $out
                        cp ${config.packages.ihp-schema}/IHPSchema.sql $out/00-ihp-schema.sql
                        cp ${config.packages.schema}/Schema.sql $out/01-app-schema.sql
                        cp ${./deploy/docker/roles.sql} $out/02-roles.sql
                        cp ${migrationsSql} $out/99-schema-migrations.sql
                    '';
                    # Pending-migration bundle for /bin/db-migrate
                    # (deploy/docker/db-migrate.sh), baked verbatim.
                    dbMigrate = pkgs.runCommand "halemans-db-migrate" {} ''
                        mkdir -p $out
                        cp ${./Application/Migration}/*.sql $out/
                    '';
                in {
                    # contents land in the image /bin (RunProdServer, RunJobs,
                    # EnqueuePollers, GenPassword, psql, sh) — docker-compose
                    # overrides the command with those stable paths.
                    docker-image = pkgs.dockerTools.buildLayeredImage {
                        name = "halemans";
                        tag = "latest";
                        contents = [
                            pkgs.cacert
                            prodServer
                            config.packages.script-EnqueuePollers
                            config.packages.script-GenPassword
                            pkgs.busybox
                            pkgs.postgresql
                            dbInit
                        ];
                        extraCommands = ''
                            mkdir -p share bin
                            ln -s ${dbInit} share/db-init
                            ln -s ${dbMigrate} share/db-migrate
                            cp ${./deploy/docker/db-init.sh} bin/db-init
                            cp ${./deploy/docker/db-migrate.sh} bin/db-migrate
                            chmod +x bin/db-init bin/db-migrate
                        '';
                        config = {
                            Cmd = [ "/bin/RunProdServer" ];
                            Env = [ "PORT=8000" ];
                            ExposedPorts = { "8000/tcp" = { }; };
                        };
                    };

                    docker-image-worker = pkgs.dockerTools.buildLayeredImage {
                        name = "halemans-worker";
                        tag = "latest";
                        contents = [ pkgs.cacert ];
                        config = {
                            Cmd = [ "${config.packages.optimized-prod-server}/bin/RunJobs" ];
                        };
                    };
                };

                ihp = {
                    appName = "halemans";
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
                        aeson-pretty
                        lens
                        vector
                        wreq
                        fast-logger
                        ihp-typed-sql
                        # milestone 1: websocket fan-out + web push
                        ihp-pglistener
                        cryptonite
                        memory
                        http-client
                        http-client-tls
                        http-types
                        network-uri
                        warp
                        network
                        base64-bytestring
                        cmark
                        # static report charts (GET /reports)
                        diagrams-core
                        diagrams-lib
                        diagrams-svg
                        svg-builder
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

                    # The IHP flake module hardcodes the database name `app`;
                    # rename it to match the app.
                    env.DATABASE_URL = lib.mkForce "postgres:///halemans?host=${config.devenv.shells.default.env.PGHOST}";
                    env.PGDATABASE = lib.mkForce "halemans";
                    services.postgres.initialDatabases = lib.mkForce [{
                        name = "halemans";
                        schema = pkgs.runCommand "halemans-db-init-schema" {} (''
                            cat ${inputs.ihp}/ihp-schema-compiler/data/IHPSchema.sql >> $out
                            echo "" >> $out
                            cat ${./Application/Schema.sql} >> $out
                        '' + lib.optionalString (builtins.pathExists ./Application/Fixtures.sql) ''
                            echo "" >> $out
                            cat ${./Application/Fixtures.sql} >> $out
                        '');
                    }];
                };
            };

            # Adding the new NixOS configuration for "production"
            # See https://ihp.digitallyinduced.com/Guide/deployment.html#deploying-with-deploytonixos for more info
            # Used to deploy the IHP application
            flake.nixosConfigurations."production" = import ./Config/nix/hosts/production/host.nix { inherit inputs; };
        };

    # Binary caches: devenv/cachix/digitallyinduced upstreams + our own halemans cache
    # (CI pushes build results there; local builds substitute from it).
    nixConfig = {
        extra-substituters = [
            "https://devenv.cachix.org"
            "https://cachix.cachix.org"
            "https://digitallyinduced.cachix.org"
            "https://cache.digitallyinduced.com/public"
            "https://halemans.cachix.org"
        ];
        extra-trusted-public-keys = [
            "devenv.cachix.org-1:w1cLUi8dv3hnoSPGAuibQv+f9TZLr6cv/Hm9XgU50cw="
            "cachix.cachix.org-1:eWNHQldwUO7G2VkjpnjDbWwy4KQ/HNxht7H4SSoMckM="
            "digitallyinduced.cachix.org-1:y+wQvrnxQ+PdEsCt91rmvv39qRCYzEgGQaldK26hCKE="
            "public:kR6JCoqAIMaO4s+EdDGh+jsHEHnoLq4ZLJPMCo0hcIQ="
            "halemans.cachix.org-1:L5KmodrEz7eZDdjHKoqMhAcR3nxB9KEREbwHKNLpQKQ="
        ];
    };
}
