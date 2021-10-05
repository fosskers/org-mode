{
  inputs = {
    haskell-nix.url = "github:input-output-hk/haskell.nix";
    nixpkgs.follows = "haskell-nix/nixpkgs";
    utils.url = "github:numtide/flake-utils";
  };

  outputs = inputs@{ self, nixpkgs, haskell-nix, utils, ... }:
    let
      name = "org-mode";
      project-name = "${name}Project";
      materialization-dir-str = "nix/materializations";
      materialization-dir = ./. + ("/"+ materialization-dir-str);

      compiler-nix-name = "ghc8107"; # Not used for `stack.yaml` based projects.
      index-state = "2021-09-26T00:00:00Z";

      # This overlay adds our project to pkgs
      project-overlay = final: prev: {
        ${project-name} =
            final.haskell-nix.project' {
              src = ./.;
              inherit compiler-nix-name;
              inherit index-state;
              plan-sha256 = "0wdlgp0bz19475qgw3wa3iv1s5qcdjwydv24dnjjnr7cinaqa3cn";
              materialized = materialization-dir + "/${project-name}";
              projectFileName = "cabal.project";
            };
      };
    in
      { overlay = final: prev: {
          "${name}" = ("${project-name}-overlay" final prev)."${project-name}".flake {};
        };
      } // (utils.lib.eachSystem [ "x86_64-linux" ] (system:
        let
          pkgs = import nixpkgs {
            inherit system;
            config.allowUnfree = true;
            overlays = [
              haskell-nix.overlay
              project-overlay
            ];
          };

          project = pkgs."${project-name}";
          flake = pkgs."${project-name}".flake {};
          tools = pkgs.lib.attrsets.mapAttrs (name: attrs: attrs // {
            inherit index-state;
            materialized = materialization-dir + "/${name}";
          }) ( {
            cabal  = { version = "3.2.0.0"; plan-sha256 = "0hclf6nj3m1l8xn096rm7fbckb9kds1h8n0w5mdxvpmj7lryg8vm"; };
            ghcid  = { version = "latest"; plan-sha256 = "1jamvhf7vnh9r1rzlm1alksyjh2wrfr8apw5jj8lxgn4x4dkpwf4"; };
            hoogle = { version = "latest"; plan-sha256 = "1n645574bpy28l31z03q371sgqnipszgpdnvlz36y0il0idmvkk9"; };
            retrie = { version = "latest"; plan-sha256 = "0fyw7k08lgr6ixpwmyhlddrv8d5xk35idmhr4fhdqlbcyn847w1v"; };
          } // {
            #cabal={version="3.2.0.0";};ghcid={version="latest";};hoogle={version="latest";};retrie={version="latest";};
          });

          hls-project = pkgs.haskell-nix.cabalProject {
            src = pkgs.fetchFromGitHub {
              owner = "haskell";
              repo = "haskell-language-server";
              rev = "ghcide-v1.4.2.1";
              sha256 = "sha256-9mHCdkDdvDQAJMqHNrYT58K3yXKOdMB85A03QRCiNPA=";
              fetchSubmodules = true;
            };
            inherit compiler-nix-name;
            inherit index-state;
            plan-sha256 = "0g69m5v4czamimbk3snhfhd0rbad7a2mgr5xnlvny5iq81yk8sr6";
            materialized = materialization-dir + "/hls-project";
            modules = [{ packages.haskell-language-server.flags.all-plugins = true; }];
          };

          devShell = project.shellFor {
            name = "${name}-dev-shell";
            inherit tools;
            buildInputs = with pkgs; [
              zlib.dev
              hls-project.haskell-language-server.components.exes.haskell-language-server
              #hls-project.haskell-language-server.components.exes.haskell-language-server-wrapper
            ];

            # Prevents cabal from choosing alternate plans, so that
            # *all* dependencies are provided by Nix.
            # TODO: Set to true as soon as haskell.nix issue #231 is resolved.
            exactDeps = false;
            withHoogle = true;
          };
        in flake // rec {
          inherit devShell;
          apps = let
            inherit (pkgs) writeScriptBin;
            inherit (pkgs.lib.strings) concatStringsSep;
            inherit (pkgs.lib.attrsets) mapAttrsToList;
            lines = concatStringsSep "\n";
            cachix-repo = "stites";
            check-only-gitroot = ''
              if [[ "$(git rev-parse --show-toplevel)" != "$PWD" ]]; then
                echo "only run from top level: $(git rev-parse --show-toplevel)"
                exit 1
              fi
            '';
          in {
            update-materialization = writeScriptBin "update-materialization" (lines [
              #check-only-gitroot
              ''
              nix build '.#gcroot'
              MATERIALIZATION_DIR="$(git rev-parse --show-toplevel)/${materialization-dir-str}"
              mkdir -p $MATERIALIZATION_DIR
              for f in result/materializers/*; do
                 echo "$(basename $f) - $($f/calculateSha)";
                 echo $f/generateMaterialized $MATERIALIZATION_DIR/$(basename $f);
                 $f/generateMaterialized $MATERIALIZATION_DIR/$(basename $f);
              done
              ''
            ]);
            update-hie-yaml = writeScriptBin "update-hie-yaml" (lines [
              check-only-gitroot
              ''
              if command -v gen-hie > /dev/null 2>&1; then
                cabal v2-build all
                gen-hie > hie.yaml
              else
                echo "gen-hie not found on path. See installation instructions at"
                echo https://github.com/Avi-D-coder/implicit-hie
              fi
              ''
            ]);

            build-all = with pkgs.lib.attrsets; writeScriptBin "build-all" (lines (attrValues (mapAttrs (n: p:
              ''nix build ".#${n}"''
            ) flake.packages)));

            push-cachix = writeScriptBin "push-cachix" (lines [
              #check-only-gitroot

              # see https://docs.cachix.org/pushing#flakes
              # push flake inputs
              ''
              nix flake archive --json \
                | jq -r '.path,(.inputs|to_entries[].value.path)' \
                | cachix push ${cachix-repo}
              ''

              # push runtime closure
              (lines (mapAttrsToList (n: p: ''
                nix build '.#${n}' --json \
                 | jq -r '.[].outputs | to_entries[].value' \
                 | cachix push ${cachix-repo}
                '') packages))

              # push shell environment
              ''
              nix develop --profile dev-profile --command cachix push ${cachix-repo} dev-profile
              ''
            ]);

            pull-cachix = writeScriptBin "pull-cachix" (lines [
              check-only-gitroot
              "cachix use ${cachix-repo}" # use cachix
              "nix build" # build with cachix
              "nix develop --profile dev-profile" # build dev shell with cachix
              # this last line is important for bootstrapping, especially if you use nix-direnv
            ]);

          };

          packages = flake.packages // {
            gcroot = pkgs.linkFarmFromDrvs "${project-name}-shell-gcroot" [
              devShell
              devShell.stdenv
              project.plan-nix
              project.roots

              (let compose = f: g: x: f (g x);
                    flakePaths = compose pkgs.lib.attrValues (
                      pkgs.lib.mapAttrs
                        (name: flake: { name = name; path = flake.outPath; })
                    );
                in  pkgs.linkFarm "input-flakes" (flakePaths self.inputs)
              )

              (let getMaterializers = (name: prj:
                      pkgs.linkFarmFromDrvs "${name}" [
                        prj.plan-nix.passthru.calculateMaterializedSha
                        prj.plan-nix.passthru.generateMaterialized
                      ]);
               in pkgs.linkFarmFromDrvs "materializers" (
                    (pkgs.lib.mapAttrsToList getMaterializers (
                        { ${project-name} = project; }
                        // (pkgs.lib.mapAttrs (_: builtins.getAttr "project") (project.tools tools))
                        // { hls-project = hls-project; }
                    ))
                  )
              )
            ];
          };

          defaultPackage = packages."${name}:exe:bk";
        }
        ));
}
