let
  pkgs = import <nixpkgs> { };
in
pkgs.stdenv.mkDerivation {
  name = "cofree.coffee";
  buildInputs = [ pkgs.pandoc ];
  src = ./src;
  installPhase = ''
    mkdir "$out"
    for f in *.md;
    do
      pandoc "$f" -s -f markdown -t html -o "$f.html";
      cp "$f.html" $out;
   done
  '';
}
