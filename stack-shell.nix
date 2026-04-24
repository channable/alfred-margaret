{ ghcVersion ? "ghc914" }:
let
  pkgs = import ./nix/nixpkgs-pinned.nix { inherit ghcVersion; };
  haskellDependencies = import ./nix/haskell-dependencies.nix;

  glibcLocalesMinimal = import ./nix/locale.nix { inherit pkgs; };

  libacbench = pkgs.rustPlatform.buildRustPackage rec {
    name = "libacbench";
    src = ./benchmark/rust-ffi/libacbench;
    buildType = "release";
    cargoLock = {
      lockFile = ./benchmark/rust-ffi/libacbench/Cargo.lock;
    };
  };
in
  pkgs.mkShell {
    name = "alfred-margaret";
    buildInputs = [
      (pkgs.channableHaskellPackages.ghcWithPackages haskellDependencies)
      pkgs.cabal-install
      pkgs.llvm_19
      pkgs.llvmPackages_19.clang
      pkgs.zlib
      glibcLocalesMinimal
      libacbench
    ];

    LOCALE_ARCHIVE = "${glibcLocalesMinimal}/lib/locale/locale-archive";
    LANG = "C.UTF-8";
  }
