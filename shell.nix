with (import <nixpkgs> {}).pkgs;
let
  dependent-format = import ../dependent-format/release.nix;
  ghcPackages = haskell.packages.ghc901;
  ghc = ghcPackages.ghcWithPackages
          (pkgs: with pkgs; [ megaparsec dlist mtl reflection async pipes pipes-concurrency dependent-format ]);
in
stdenv.mkDerivation {
  name = "my-haskell-env-0";
  buildInputs = [ ghc ghcPackages.haskell-language-server ];
  shellHook = "eval $(grep export ${ghc}/bin/ghc)";
}
