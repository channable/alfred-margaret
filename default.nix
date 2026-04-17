{
  pkgs ? import ./nix/nixpkgs-pinned.nix {},
  # Use haskell-languager-server?
  hsTools ? false,
  # Use tools for the benchmarks in other languages (Cargo, Bazel, etc.)?
  benchTools ? false,
  # Which environment to build. Use "shell" for direnv/nix-shell, "build-env"
  # for the legacy buildEnv derivation.
  environment ? "build-env"
}:
let
  haskellDependencies = import ./nix/haskell-dependencies.nix;

  glibcLocalesMinimal = import ./nix/locale.nix { inherit pkgs; };

  paths = with pkgs; (
    [
      # Nix tooling
      niv
      nix-tree

      # Haskell tooling
      stack

      # Haskell dependencies
      (ghc914Packages.ghcWithPackages haskellDependencies)

      # Other
      llvm_19
      llvmPackages_19.clang
    ] ++
    # We don't use the overlay here because the tooling doesn't need it.
    # The advantage of doing so is that these packages are already available in a global cache.
    lib.optionals hsTools (with haskell.packages.ghc914; [
      haskell-language-server
      implicit-hie
    ]) ++
    lib.optionals benchTools [
      (python3.withPackages (pyPkgs: [
        pyPkgs.clize
        pyPkgs.numpy
      ]))

      # For rust implementation
      cargo

      # For java implementation (uses outdated bazel stuff)
      jdk8
      bazel_1

      # For Haskell implementation
      gmp
    ]
  );

  environments = {
    build-env = pkgs.buildEnv {
      name = "alfred-margaret-env";
      paths = paths;
    };

    shell = pkgs.mkShell {
      buildInputs = paths ++ [ glibcLocalesMinimal ];

      LOCALE_ARCHIVE = "${glibcLocalesMinimal}/lib/locale/locale-archive";
      LANG = "C.UTF-8";
    };
  };
in
  environments."${environment}"
