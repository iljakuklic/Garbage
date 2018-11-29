with import <nixpkgs> {};
derivation {
  name = "test";
  buildInputs = [ ghc ];
  builder = ./.;
  system = "foo";
}
