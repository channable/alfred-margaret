{ overlays ? [], ghcVersion ? "ghc914" }:
let
  sources = import ./sources.nix;
in
  import sources.nixpkgs {
    overlays = [(import ./ghc-overlay.nix ghcVersion)] ++ overlays;
  }
