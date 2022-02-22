{
  description = "UPLC2C System Architecture";

  inputs = {
    nixpkgs.url = "github:nixos/nixpkgs/nixos-21.11";
    flake-compat = {
      url = "github:edolstra/flake-compat";
      flake = false;
    };
    flake-compat-ci.url = "github:hercules-ci/flake-compat-ci";
  };

  outputs = { self, nixpkgs, flake-compat, flake-compat-ci }:
  let pkgs = import nixpkgs { system = "x86_64-linux"; };
  in
  {
    packages.x86_64-linux.uplc2c-architecture =
     with pkgs;
     let deps = [ (texlive.combine { inherit (texlive) scheme-basic amsmath graphics hyperref; }) ];
     in
       stdenv.mkDerivation {
         name = "uplc2c-architecture";
         src = ./src;
         buildInputs = deps;
         buildPhase = ''
           mkdir -p $out
           HOME=./. pdflatex uplc2c-architecture.tex
           HOME=./. pdflatex uplc2c-architecture.tex
           cp uplc2c-architecture.pdf "$out/UPLC2C Compiler and Runtime System Architecture.pdf"
         '';
         installPhase = ''
           echo done
         '';
       };

    defaultPackage.x86_64-linux = self.packages.x86_64-linux.uplc2c-architecture;

    ciNix = flake-compat-ci.lib.recurseIntoFlakeWith {
      flake = self;
      systems = [ "x86_64-linux" ];
    };
  };
}
