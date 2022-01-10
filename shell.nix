{ pkgs ? import <nixpkgs> { } }:
let
  easy-ps = import
    (pkgs.fetchFromGitHub {
      owner = "justinwoo";
      repo = "easy-purescript-nix";
      rev = "5716cd791c999b3246b4fe173276b42c50afdd8d";
      sha256 = "1r9lx4xhr42znmwb2x2pzah920klbjbjcivp2f0pnka7djvd2adq";
    }) {
    inherit pkgs;
  };
in
pkgs.mkShell {
  buildInputs = [
    easy-ps.purs
    pkgs.nodePackages.bower
    pkgs.nodejs
    easy-ps.pulp
    easy-ps.psc-package
    easy-ps.purescript-language-server
    easy-ps.psa
    easy-ps.spago
  ];
}
