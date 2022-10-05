{
  pkgs ? import ./nix/nixpkgs-pinned.nix {},
  # Use haskell-languager-server?
  hsTools ? false,
  # Use tools for the benchmarks in other languages (Cargo, Bazel, etc.)?
  benchTools ? false
}:
let
  haskellDependencies = import ./nix/haskell-dependencies.nix;

  paths = with pkgs; (
    [
      # Nix tooling
      cachix
      niv
      nix-tree

      # Haskell tooling
      stack

      # Haskell dependencies
      (ghc902Packages.ghcWithPackages haskellDependencies)

      # Other
      llvm_9
    ] ++
    # We don't use the overlay here because the tooling doesn't need it.
    # The advantage of doing so is that these packages are already available in a global cache.
    lib.optionals hsTools (with haskell.packages.ghc902; [
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
in
  pkgs.buildEnv {
    name = "alfred-margaret-env";
    paths = paths;
  }
