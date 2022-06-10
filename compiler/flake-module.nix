{ self, ... }:
{
  perSystem = { config, self', inputs', pkgs, system, ...  }:
    let
      # A flake-module in nix/flake-modules/haskell.nix defines haskell-nix
      # packages once, so we can reuse it here, it's more performant.
      pkgs = config.haskell-nix.pkgs;
      deferPluginErrors = true;
      haskellNixFlake =
        self.inputs.haskell-fix.lib.fixHaskellDotNix (project.flake {})
          [ ./uplc2c.cabal ];
      project = pkgs.haskell-nix.project' {
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
              pkgs.lib.mkForce [ [ (import self.inputs.plutus { inherit system; }).pkgs.libsodium-vrf ] ];
            cardano-crypto-class.components.library.pkgconfig =
              pkgs.lib.mkForce [ [ (import self.inputs.plutus { inherit system; }).pkgs.libsodium-vrf ] ];

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
        shell.shellHook = ''
          manual-ci() (
            set -e

            ./ci/lint.sh
            cabal test
            nix-build
            ./ci/examples.sh
          )
        '';
      };
      in
      {
        packages = haskellNixFlake.packages;
        checks = haskellNixFlake.checks;
        devShells.uplc2c = haskellNixFlake.devShell;
      };
}
