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
outputs=$(
  nix build -f default.nix --no-link --print-out-paths --argstr environment build-env
  nix build -f default.nix --no-link --print-out-paths --argstr environment shell
)

# 2. Push all paths to cachix.
echo "$outputs" | cachix push channable-public
