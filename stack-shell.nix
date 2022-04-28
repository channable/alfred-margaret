{}:
let
  pkgs = import ./nix/nixpkgs-pinned.nix {};
  haskellDependencies = import ./nix/haskell-dependencies.nix;
in
  pkgs.haskell.lib.buildStackProject {
    name = "alfred-margaret";
    ghc = pkgs.ghc902Packages.ghcWithPackages haskellDependencies;
    buildInputs = with pkgs; [
      llvm_9
      zlib
    ];
  }
