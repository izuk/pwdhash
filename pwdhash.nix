{ mkDerivation, base, Crypto, dataenc, haskeline, lib, process
, QuickCheck, test-framework, test-framework-quickcheck2
, transformers
}:
mkDerivation {
  pname = "pwdhash";
  version = "0.2.2.0";
  src = ./.;
  isLibrary = true;
  isExecutable = true;
  libraryHaskellDepends = [ base Crypto dataenc transformers ];
  executableHaskellDepends = [ base haskeline ];
  testHaskellDepends = [
    base process QuickCheck test-framework test-framework-quickcheck2
  ];
  description = "Implementation of the pwdhash algorithm";
  license = lib.licenses.bsd3;
}
