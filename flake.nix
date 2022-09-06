{
  inputs = {
    haskell-fix = {
      url = "github:matthewcroughan/haskell.fix";
    };
    haskell-nix = {
      url = "github:input-output-hk/haskell.nix/91ec10c0321b75a79a4b6af69fdb30fa748ec0f7";
      inputs.nixpkgs.follows = "nixpkgs";
    };
    nixpkgs.url = "github:nixos/nixpkgs/22a500a3f87bbce73bd8d777ef920b43a636f018";
    cardano-node.url = "github:input-output-hk/cardano-node/37d86a65b4ffd7a500bd6fc7246793b1363ff60a";
    #   used for libsodium-vrf
    plutus.url = "github:input-output-hk/plutus/0836d25ecc469d8a3494bf6db10fc8404da522ec";
    plutus-apps.url = "github:input-output-hk/plutus-apps/ac3f39e1c4384ce44fdb8c321985ac447e85e411";
    tinyram.url = "github:Orbis-Tertius/tinyram/f3a2ae87597b0d37934cc6a6da25fe905ff5ee59";
    flake-parts = {
      url = "github:hercules-ci/flake-modules-core";
    };
  };

  outputs = { self, flake-parts, ... }:
    flake-parts.lib.mkFlake { inherit self; } {
      systems = [ "x86_64-linux" ];
      imports = [
        ./compiler/flake-module.nix
        ./architecture/flake-module.nix
        ./nix/flake-modules/haskell.nix/flake-module.nix
      ];
    };
}
