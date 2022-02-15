{
  nixConfig.bash-prompt = "[nix-develop-uplc2c:] ";
  description = "Untyped Plutus Core to C compiler";
  inputs = {
    haskellNix.url = "github:input-output-hk/haskell.nix";
    nixpkgs.follows = "haskellNix/nixpkgs-unstable";
    flake-utils.url = "github:numtide/flake-utils";
    plutus.url = "github:input-output-hk/plutus";
    cardano-node.url = "github:input-output-hk/cardano-node";
    plutus-apps.url = "github:input-output-hk/plutus-apps";
  };
  outputs = { self, nixpkgs, plutus, flake-utils, haskellNix, cardano-node, plutus-apps }:
    flake-utils.lib.eachSystem [ "x86_64-linux" ] (system:
      let
        overlays = [
          haskellNix.overlay
          (final: prev: {
            # This overlay adds our project to pkgs
            uplc2c =
              final.haskell-nix.project' {
                src = ./.;
                compiler-nix-name = "ghc8107";
                projectFileName = "stack.yaml";
                shell.tools = {
                  cabal = { };
                  ghcid = { };
                  hlint = { };
                  haskell-language-server = { };
                  stylish-haskell = { };
                };
                # Non-Haskell shell tools go here
                shell.buildInputs = with pkgs; [
                  nixpkgs-fmt
                ];
                shell.shellHook =
                  ''
                  manual-ci() (
                    set -e
                    ./lint.sh
                  )
                  '';
              };
          })
        ];
        pkgs = import nixpkgs { inherit system overlays; inherit (haskellNix) config; };
        flake = pkgs.uplc2c.flake { };
      in flake // {
        defaultPackage = flake.packages."uplc2c:exe:uplc2c";
      });
}
