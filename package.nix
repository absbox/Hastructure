{ mkDerivation, aeson, aeson-pretty, attoparsec, attoparsec-aeson
, base, base-compat, bytestring, containers, Decimal, deepseq
, dlist, exceptions, generic-lens, hashable, http-types, ieee754
, lens, lib, math-functions, MissingH, monad-loops, mtl
, numeric-limits, openapi3, parallel, regex-base
, regex-pcre-builtin, regex-tdfa, scientific, servant
, servant-openapi3, servant-server, split, string-conversions
, swagger2, tabular, tasty, tasty-golden, tasty-hspec, tasty-hunit
, template-haskell, text, time, vector, wai, wai-cors, warp, yaml
}:
mkDerivation {
  pname = "Hastructure";
  version = "0.45.0";
  src = ./.;
  isLibrary = true;
  isExecutable = true;
  libraryHaskellDepends = [
    aeson aeson-pretty attoparsec attoparsec-aeson base base-compat
    bytestring containers Decimal deepseq dlist exceptions generic-lens
    hashable http-types ieee754 lens math-functions MissingH
    monad-loops mtl numeric-limits openapi3 parallel regex-base
    regex-pcre-builtin regex-tdfa scientific servant servant-openapi3
    servant-server split string-conversions swagger2 tabular
    template-haskell text time vector wai wai-cors warp yaml
  ];
  executableHaskellDepends = [
    aeson aeson-pretty attoparsec attoparsec-aeson base base-compat
    bytestring containers Decimal deepseq dlist exceptions generic-lens
    hashable http-types ieee754 lens math-functions MissingH
    monad-loops mtl numeric-limits openapi3 parallel regex-base
    regex-pcre-builtin regex-tdfa scientific servant servant-openapi3
    servant-server split string-conversions swagger2 tabular tasty
    tasty-golden tasty-hspec tasty-hunit template-haskell text time
    vector wai wai-cors warp yaml
  ];
  testHaskellDepends = [
    aeson aeson-pretty attoparsec attoparsec-aeson base base-compat
    bytestring containers Decimal deepseq dlist exceptions generic-lens
    hashable http-types ieee754 lens math-functions MissingH
    monad-loops mtl numeric-limits openapi3 parallel regex-base
    regex-pcre-builtin regex-tdfa scientific servant servant-openapi3
    servant-server split string-conversions swagger2 tabular tasty
    tasty-golden tasty-hspec tasty-hunit template-haskell text time
    vector wai wai-cors warp yaml
  ];
  homepage = "https://github.com/yellowbean/Hastructure#readme";
  description = "Cashflow modeling library for structured finance";
  license = lib.licenses.bsd3;
  mainProgram = "Hastructure-exe";
}
