with (import <nixpkgs> {});
mkShell {
  buildInputs = [
    ghc
  ];
}