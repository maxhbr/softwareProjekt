{ nixpkgs ? import <nixpkgs> {}, compiler ? "default" }:

let

  inherit (nixpkgs) pkgs;

  f = { mkDerivation, array, base, binary, bytestring, deepseq
      , directory, hspec, integer-gmp, monad-parallel, parallel, process
      , random, stdenv, template-haskell
      }:
      mkDerivation {
        pname = "galFld";
        version = "0.1.0.0";
        src = ./.;
        isLibrary = true;
        isExecutable = true;
        libraryHaskellDepends = [
          array base binary bytestring deepseq integer-gmp parallel process
          template-haskell
        ];
        executableHaskellDepends = [
          array base binary bytestring deepseq directory parallel process
          template-haskell
        ];
        testHaskellDepends = [ base hspec monad-parallel random ];
        description = "An implementation of finite fields and arbitrary extensions together with some algorithms";
        license = stdenv.lib.licenses.bsd3;
      };

  haskellPackages = if compiler == "default"
                      then pkgs.haskellPackages
                      else pkgs.haskell.packages.${compiler};

  drv = haskellPackages.callPackage f {};

in

  if pkgs.lib.inNixShell then drv.env else drv
