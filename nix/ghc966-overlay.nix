self: super:
let
  haskellOverlay = import ./haskell-overlay.nix;
in {
  ghc966Packages = super.haskell.packages.ghc966.extend haskellOverlay;
}
