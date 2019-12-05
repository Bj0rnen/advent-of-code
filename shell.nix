with (import <nixpkgs> {}).pkgs;
let
  ghcPackages = haskell.packages.ghc865;
  ghc = ghcPackages.ghcWithPackages
          (pkgs: with pkgs; [ megaparsec dlist mtl ]);
in
stdenv.mkDerivation {
  name = "my-haskell-env-0";
  buildInputs = [ ghc ghcPackages.hlint ];
  shellHook = "eval $(grep export ${ghc}/bin/ghc)";
}
