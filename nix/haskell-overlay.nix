self: super:
let
  hlib = (import ./nixpkgs-pinned.nix {}).haskell.lib;
in {
  binary-orphans = hlib.doJailbreak super.binary-orphans;
  microstache = hlib.doJailbreak super.microstache;
}
