#!/usr/bin/env bash
# Build the alfred-margaret Nix environment and push it (together with all
# of its build-time and runtime requisites) to the `channable-public`
# Cachix cache.
#
# Requires `CACHIX_SIGNING_KEY` to be set in the environment (CI exposes it
# as a secret).
#
# Usage:
#   ./.semaphore/push-to-cachix.sh
set -euo pipefail

# 1. Build both environments (no GC root) and capture their output paths.
#    When `GHC_VERSION` is set (e.g. by the Cabal matrix block in CI), also
#    build the `shell` environment for that specific GHC version so that
#    matrix builds populate the cache too.
outputs=$(
  nix build -f default.nix --no-link --print-out-paths --argstr environment build-env
  nix build -f default.nix --no-link --print-out-paths --argstr environment shell
  if [[ -n "${GHC_VERSION:-}" ]]; then
    nix build -f default.nix --no-link --print-out-paths --argstr environment build-env --argstr ghcVersion "$GHC_VERSION"
    nix build -f default.nix --no-link --print-out-paths --argstr environment shell --argstr ghcVersion "$GHC_VERSION"
  fi
)

# 2. Push all paths to cachix.
echo "$outputs" | cachix push channable-public
