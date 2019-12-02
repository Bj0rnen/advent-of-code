with (import <nixpkgs> {}).pkgs;
let
  ghcPackages = haskell.packages.ghc865;
  ghc = ghcPackages.ghcWithPackages
          (pkgs: with pkgs; [ megaparsec ]);
in
stdenv.mkDerivation {
  name = "my-haskell-env-0";
  buildInputs = [ ghc ];
  shellHook = "eval $(grep export ${ghc}/bin/ghc)";
}
