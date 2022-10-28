{ mkDerivation, base, bytestring, conduit, containers, hspec
, hspec-discover, lib, path, path-io, template-haskell, text
}:
mkDerivation {
  pname = "template-haskell-reload";
  version = "0.0.0.0";
  src = ./.;
  libraryHaskellDepends = [
    base bytestring conduit containers path path-io template-haskell
    text
  ];
  testHaskellDepends = [ base containers hspec path text ];
  testToolDepends = [ hspec-discover ];
  homepage = "https://github.com/NorfairKing/template-haskell-reload#readme";
  license = lib.licenses.mit;
}
