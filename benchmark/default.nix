{ pkgs ? import <nixpkgs> {} }:
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
        pkgs.llvm
    ];
}