let

  nixpkgs = import <nixpkgs> { overlays = [ ]; };


  haskellDeps = ps: with ps; [
    hmatrix
    Naperian
    numbers
  ];

in

  nixpkgs.stdenv.mkDerivation {
  name = "env";
  buildInputs = [
    (nixpkgs.haskellPackages.ghcWithPackages haskellDeps)
  ];
}
