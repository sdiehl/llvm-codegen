# This file was auto-generated by cabal2nix. Please do NOT edit manually!

{ cabal, errors, haskeline, libffi, llvmGeneral, llvmGeneralPure
, mtl, parsec, prettyShow, tasty, tastyGolden, tastyHunit
, transformers, vector
}:

cabal.mkDerivation (self: {
  pname = "llvm-codegen";
  version = "0.1.0.0";
  sha256 = "c4d6a6ba91b3bd4219d3175df7626aa3fb928709ef86fdbc6623b926c13aa653";
  isLibrary = true;
  isExecutable = true;
  buildDepends = [
    errors haskeline libffi llvmGeneral llvmGeneralPure mtl parsec
    prettyShow transformers vector
  ];
  testDepends = [
    errors libffi llvmGeneral llvmGeneralPure mtl tasty tastyGolden
    tastyHunit transformers vector
  ];
  meta = {
    license = self.stdenv.lib.licenses.mit;
    platforms = self.ghc.meta.platforms;
  };
})