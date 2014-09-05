let
  pkgs = import <nixpkgs> {};
  zlib = pkgs.zlib;
  ncurses = pkgs.ncurses;
  haskellPackages = pkgs.haskellPackages;
  cabal = haskellPackages.cabal;
  cabalInstall = haskellPackages.cabalInstall;
in cabal.mkDerivation (self: {
  pname = "llvm-codegen";
  version = "0.1.0.0";
  src = ./.;
  isLibrary = true;
  isExecutable = true;
  buildDepends = [
    zlib
    ncurses
    haskellPackages.errors
    haskellPackages.haskeline
    haskellPackages.libffi
    haskellPackages.llvmGeneral
    haskellPackages.llvmGeneralPure
    haskellPackages.mtl
    haskellPackages.parsec
    haskellPackages.prettyShow
    haskellPackages.transformers
    haskellPackages.vector
  ];
  buildTools = [ "cabalInstall_1_18_1_3" ];
  testDepends = [
     haskellPackages.errors
     haskellPackages.libffi
     haskellPackages.llvmGeneral
     haskellPackages.llvmGeneralPure
     haskellPackages.mtl
     haskellPackages.tasty
     haskellPackages.tastyGolden
     haskellPackages.tastyHunit
     haskellPackages.transformers
     haskellPackages.vector
  ];
  meta = {
    license = self.stdenv.lib.licenses.mit;
    platforms = self.ghc.meta.platforms;
  };
})
