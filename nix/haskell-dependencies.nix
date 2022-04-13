haskellPackages:
  with haskellPackages; [
    # Direct dependencies
    aeson
    hashable
    primitive
    text
    unordered-containers
    hspec
    hspec-expectations

    # Transitive dependencies
    attoparsec
    OneTuple
    quickcheck-instances
    semialign
    text-short
  ]
