{
  nixConfig.bash-prompt = "[nix-develop-uplc2c:] ";
  description = "A very basic flake";
  inputs =
  {
    # Nixpkgs set to specific URL for haskellNix
    nixpkgs.url = "github:NixOS/nixpkgs/baaf9459d6105c243239289e1e82e3cdd5ac4809";
    nixpkgs.follows = "haskellNix/nixpkgs-unstable";
    #HaskellNix is implemented using a set nixpkgs.follows; allowing for flake-build
    haskellNix =
    {
      inputs.nixpkgs.follows = "nixpkgs";
      url = "github:input-output-hk/haskell.nix";
    };
    flake-utils.url = "github:numtide/flake-utils";
    plutus.url = "github:input-output-hk/plutus";
    cardano-node =
    {
      inputs.nixpkgs.follows = "nixpkgs";
      url = "github:input-output-hk/cardano-node";
    };
    plutus-apps.url = "github:input-output-hk/plutus-apps";
  };
  outputs = { self, nixpkgs, plutus, flake-utils, haskellNix, cardano-node, plutus-apps }:
    flake-utils.lib.eachSystem [ "x86_64-linux" ] (system:
      let
        deferPluginErrors = true;
        overlays = [
          haskellNix.overlay
          (final: prev: {
            # This overlay adds our project to pkgs
            uplc2c =
              final.haskell-nix.project' {
                src = ./.;
                compiler-nix-name = "ghc8107";
                projectFileName = "stack.yaml";
                modules = [{
                  packages = {
                    marlowe.flags.defer-plugin-errors = deferPluginErrors;
                    plutus-use-cases.flags.defer-plugin-errors = deferPluginErrors;
                    plutus-ledger.flags.defer-plugin-errors = deferPluginErrors;
                    plutus-contract.flags.defer-plugin-errors = deferPluginErrors;
                    cardano-crypto-praos.components.library.pkgconfig =
                      pkgs.lib.mkForce [ [ (import plutus { inherit system; }).pkgs.libsodium-vrf ] ];
                    cardano-crypto-class.components.library.pkgconfig =
                      pkgs.lib.mkForce [ [ (import plutus { inherit system; }).pkgs.libsodium-vrf ] ];

                  };
                }];
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

                    ./ci/lint.sh
                    cabal test
                    nix-build
                    ./ci/examples.sh
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
