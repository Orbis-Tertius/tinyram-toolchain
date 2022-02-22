{
  description = "UPLC2C System Architecture";

  inputs = {
    nixpkgs.url = "github:nixos/nixpkgs/nixos-21.11";
    flake-compat = {
      url = "github:edolstra/flake-compat";
      flake = false;
    };
    flake-compat-ci.url = "github:hercules-ci/flake-compat-ci";
    danalib.url = "github:ArdanaLabs/danalib";
  };

  outputs = { self, nixpkgs, flake-compat, flake-compat-ci, danalib }:
  let pkgs = import nixpkgs { system = "x86_64-linux"; };
  in
  {
    packages.x86_64-linux.uplc2c-architecture =
      danalib.internal.x86_64-linux.mkDoc
      "uplc2c-architecture"
      ./src
      "uplc2c-architecture"
      "UPLC2C Compiler and Runtime System Architecture";

    defaultPackage.x86_64-linux = self.packages.x86_64-linux.uplc2c-architecture;

    ciNix = flake-compat-ci.lib.recurseIntoFlakeWith {
      flake = self;
      systems = [ "x86_64-linux" ];
    };
  };
}
