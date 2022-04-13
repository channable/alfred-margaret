{ overlays ? [] }:
let
  sources = import ./sources.nix;
in
  import sources.nixpkgs {
    overlays = [(import ./ghc902-overlay.nix)] ++ overlays;
  }
