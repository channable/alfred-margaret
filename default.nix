{
  pkgs ? import ./nix/nixpkgs-pinned.nix {}
}:
let
  paths = [
    # Nix tooling
    pkgs.niv

    # Haskell tooling
    pkgs.stack
    pkgs.haskellPackages.haskell-language-server
    pkgs.haskellPackages.implicit-hie
    pkgs.haskellPackages.stylish-haskell

    # Other
    pkgs.llvm_9
  ];
in
  pkgs.buildEnv {
    name = "unicode-trail";
    paths = paths;
  }
