with (import <nixpkgs> {});
mkShell {
  buildInputs = [
    haskell.compiler.ghc921
    deno
  ];
}