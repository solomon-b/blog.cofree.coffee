let
  pkgs = import <nixpkgs> { };
in
pkgs.fetchFromGitHub {
  owner = "ssbothwell";
  repo = "cofree.coffee";
  rev = "8ec9426a79e12b26591082aecc9bd28d432c83bd";
  sha256 = "0pmg9q7vnmppxx9fz24rd9a64f3xsq7923703ha9qxsz3nmrk890";
}
