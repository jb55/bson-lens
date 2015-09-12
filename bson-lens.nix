{ mkDerivation, base, profunctors, bson, lens, stdenv }:
mkDerivation {
  pname = "bson-lens";
  version = "0.1.0";
  src = ./.;
  libraryHaskellDepends = [ base profunctors bson lens ];
  homepage = "https://github.com/jb55/bson-lens";
  description = "BSON lenses";
  license = stdenv.lib.licenses.mit;
}
