ghcVersion: self: super:
let
  haskellOverlay = import ./haskell-overlay.nix;
in {
  channableHaskellPackages = super.haskell.packages.${ghcVersion}.extend haskellOverlay;
}
