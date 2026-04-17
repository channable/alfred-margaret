self: super:
let
  haskellOverlay = import ./haskell-overlay.nix;
in {
  ghc914Packages = super.haskell.packages.ghc914.extend haskellOverlay;
}
