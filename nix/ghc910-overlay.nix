self: super:
let
  haskellOverlay = import ./haskell-overlay.nix;
in {
  ghc910Packages = super.haskell.packages.ghc910.extend haskellOverlay;
}
