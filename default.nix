{
  pkgs ? import ./nix/nixpkgs-pinned.nix {}
}:
let
  paths = [
    # Nix tooling
    pkgs.niv

    # Haskell tooling
    pkgs.stack

    # Other
    pkgs.llvm_9
  ];
in
  pkgs.buildEnv {
    name = "unicode-trail";
    paths = paths;
  }
