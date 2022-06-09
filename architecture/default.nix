{ stdenv, texlive }:
stdenv.mkDerivation {
  name = "uplc2c-architecture";
  src = ./src;
  buildInputs = [ (texlive.combine { inherit (texlive) scheme-basic amsmath graphics hyperref; }) ];
  buildPhase = ''
    mkdir -p $out
    HOME=./. pdflatex uplc2c-architecture.tex
    HOME=./. pdflatex uplc2c-architecture.tex
    cp uplc2c-architecture.pdf "$out/UPLC2C Architecture.pdf"
  '';
  installPhase = ''
    echo done
  '';
}
