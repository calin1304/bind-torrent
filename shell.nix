let pkgs = import <nixpkgs> {};
in
  pkgs.mkShell {
    buildInputs = with pkgs;
    [ dhall
      haskell.packages.ghc865.hlint
      haskell.packages.ghc865.stylish-haskell
    ];
  }

