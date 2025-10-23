{
  description = "Haskell app (Cabal) on NixOS";

  inputs.nixpkgs.url = "github:NixOS/nixpkgs/nixpkgs-unstable"; # pick a stable branch
  inputs.flake-parts.url = "github:hercules-ci/flake-parts";

  outputs = inputs@{ self, nixpkgs, flake-parts, ... }:
    flake-parts.lib.mkFlake { inherit inputs; } {
      systems = [ "x86_64-linux" "aarch64-linux" ];

      perSystem = { pkgs, system, ... }:
        let
          hp =  pkgs.haskell.packages.ghc912;
          pkg = hp.callCabal2nix "newspeak" ./. { };
        in
        {
          packages.default = pkg;

          devShells.default = pkgs.mkShell {
            buildInputs = [
              hp.ghc
              hp.cabal-install
              hp.haskell-language-server
              hp.tasty
              hp.haskeline
              hp.tasty-hunit
              pkgs.ghcid
              pkgs.ormolu # or fourmolu
            ];
            shellHook = ''
              echo "GHC: $(${hp.ghc}/bin/ghc --version)"
              export CABAL_DIR=$PWD/.cabal
            '';
          };

          apps.default = {
            type = "app";
            program = "${pkg}/bin/newspeak";
          };

          formatter = pkgs.nixfmt-classic;
        };
    };
}
