{ nur ? (import ~/personal/nur-packages {})
, nixpkgs ? import <nixpkgs> { overlays = [ nur.overlays.advent-of-code-2019 ]; }
}:
with nixpkgs;
mkShell {
  buildInputs = [
    cabal-install
    haskellPackages.hasktags
    haskellPackages.hlint
  ];
  inputsFrom = [
    (haskellPackages.callCabal2nix "advent-of-code-2019" ./. {}).env
  ];
  name = "advent-of-code-2019";
}
