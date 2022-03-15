let
    pinnedPkgs = import (builtins.fetchTarball {
        name = "ac-bench-nixpkgs";
        url = "https://github.com/NixOS/nixpkgs/archive/1882c6b7368fd284ad01b0a5b5601ef136321292.tar.gz";
        sha256 = "0zg7ak2mcmwzi2kg29g4v9fvbvs0viykjsg2pwaphm1fi13s7s0i";
    }) {};
in
{ pkgs ? pinnedPkgs }:
pkgs.mkShell {
    buildInputs = [
        # For benchmark code
        (pkgs.python3.withPackages (pyPkgs: [
            pyPkgs.clize
            pyPkgs.numpy
        ]))

        # For rust implementation
        pkgs.cargo

        # For java implementation (uses outdated bazel stuff)
        pkgs.jdk8
        pkgs.bazel_1

        # For Haskell implementation
        pkgs.stack
        pkgs.gmp
        # GHC 8.10.7 produces faster code with LLVM 9 than with LLVM 12
        pkgs.llvmPackages_9.llvm
    ];
}
