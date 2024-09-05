{ overlays ? [] }:
let
  sources = import ./sources.nix;
in
  import sources.nixpkgs {
    overlays = [(import ./ghc966-overlay.nix)] ++ overlays;
  }
