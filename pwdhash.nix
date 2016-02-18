{ mkDerivation, base, Crypto, dataenc, haskeline, process
, QuickCheck, stdenv, transformers
}:
mkDerivation {
  pname = "pwdhash";
  version = "0.2.1.0";
  src = ./.;
  isLibrary = true;
  isExecutable = true;
  libraryHaskellDepends = [ base Crypto dataenc transformers ];
  executableHaskellDepends = [ base haskeline ];
  testHaskellDepends = [ base process QuickCheck ];
  description = "Implementation of the pwdhash algorithm";
  license = stdenv.lib.licenses.bsd3;
}
