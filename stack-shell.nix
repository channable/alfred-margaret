{}:
let
  pkgs = import ./nix/nixpkgs-pinned.nix {};
  haskellDependencies = import ./nix/haskell-dependencies.nix;

  libacbench = pkgs.rustPlatform.buildRustPackage rec {
    name = "libacbench";
    src = ./benchmark/rust-ffi/libacbench;
    buildType = "release";
    cargoLock = {
      lockFile = ./benchmark/rust-ffi/libacbench/Cargo.lock;
    };
  };
in
  pkgs.haskell.lib.buildStackProject {
    name = "alfred-margaret";
    ghc = pkgs.ghc966Packages.ghcWithPackages haskellDependencies;
    buildInputs = with pkgs; [
      llvm_9
      zlib
      libacbench
    ];
  }
