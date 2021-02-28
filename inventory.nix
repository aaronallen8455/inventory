{ mkDerivation, appendmap, base, bytestring, containers, directory
, filepath, ghc, ghc-paths, hpack, lib, mtl, tasty, tasty-hunit
}:
mkDerivation {
  pname = "inventory";
  version = "0.1.0.1";
  src = ./.;
  isLibrary = true;
  isExecutable = true;
  libraryHaskellDepends = [
    appendmap base bytestring containers directory filepath ghc
    ghc-paths mtl
  ];
  libraryToolDepends = [ hpack ];
  executableHaskellDepends = [
    appendmap base bytestring containers directory filepath ghc
    ghc-paths mtl
  ];
  testHaskellDepends = [
    appendmap base bytestring containers directory filepath ghc
    ghc-paths mtl tasty tasty-hunit
  ];
  prePatch = "hpack";
  homepage = "https://github.com/aaronallen8455/inventory#readme";
  description = "Project statistics and definition analysis";
  license = lib.licenses.bsd3;
}
