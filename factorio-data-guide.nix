{ mkDerivation, base, stdenv, unordered-containers, data-default }:
mkDerivation {
  pname = "factorio-data-guide";
  version = "0.1.0.0";
  src = ./.;
  isLibrary = true;
  isExecutable = true;
  libraryHaskellDepends = [ base unordered-containers data-default ];
  executableHaskellDepends = [ base ];
  doHaddock = false;
  homepage = "https://github.com/johnsonwj/factorio-data-guide";
  description = "Data library and calculators for Factorio";
  license = stdenv.lib.licenses.mit;
}
