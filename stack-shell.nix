{}:
let
  pkgs = import ./nix/nixpkgs-pinned.nix {};
  haskellDependencies = import ./nix/haskell-dependencies.nix;
in
  pkgs.haskell.lib.buildStackProject {
    name = "alfred-margaret";
    ghc = pkgs.haskellPackages.ghcWithHoogle haskellDependencies;
    buildInputs = with pkgs; [
      llvm_9
      zlib
    ];
  }
