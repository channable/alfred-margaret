{}:
let
  pkgs = import ./nix/nixpkgs-pinned.nix {};
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
      (pkgs.ghc914Packages.ghcWithPackages haskellDependencies)
      pkgs.llvm_19
      pkgs.llvmPackages_19.clang
      pkgs.zlib
      glibcLocalesMinimal
      libacbench
    ];

    LOCALE_ARCHIVE = "${glibcLocalesMinimal}/lib/locale/locale-archive";
    LANG = "C.UTF-8";
  }
