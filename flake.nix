{
  description = "uplc2c";
  inputs = {
    nixpkgs.follows = "haskellNix/nixpkgs-unstable";
    flake-utils.url = "github:numtide/flake-utils";
    architecture.url = "path:./architecture";
    compiler.url = "path:./compiler";
  };
  outputs = { self, nixpkgs, flake-utils, architecture, compiler }:
    flake-utils.lib.eachSystem [ "x64_64-linux" ] (system:
      let
        overlays = [
          (final: prev: {
            uplc2c = final.stdenv.mkDerivation {
              name = "uplc2c";
              src = ./.;
              buildInputs = [ architecture compiler ];
              buildPhase = ''
                echo done
              '';
            };
          })
        ];
        pkgs = import nixpkgs { inherit system overlays; };
        flake = pkgs.uplc2c.flake { };
      in flake // {
        defaultPackage = flake.packages.uplc2c;
      });
}
