self: super:
let
  haskellOverlay = import ./haskell-overlay.nix;
in {
  ghc902Packages = super.haskell.packages.ghc902.extend haskellOverlay;
}
