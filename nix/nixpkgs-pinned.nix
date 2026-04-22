{ overlays ? [] }:
let
  sources = import ./sources.nix;
in
  import sources.nixpkgs {
    overlays = [(import ./ghc914-overlay.nix)] ++ overlays;
  }
