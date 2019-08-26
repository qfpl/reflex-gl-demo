{ mkDerivation, base, bytestring, containers, dependent-map
, dependent-sum, dependent-sum-template, directory, doctest
, fsnotify, gl, GLFW-b, glow, lens, linear, mtl, path, QuickCheck
, ref-tf, reflex, reflex-basic-host, StateVar, stdenv, these, time
, witherable
}:
mkDerivation {
  pname = "reflex-gl-demo";
  version = "0.1.0.0";
  src = ./.;
  isLibrary = true;
  isExecutable = true;
  libraryHaskellDepends = [
    base bytestring containers dependent-map dependent-sum
    dependent-sum-template directory fsnotify gl GLFW-b glow lens
    linear mtl path ref-tf reflex reflex-basic-host StateVar these time
    witherable
  ];
  executableHaskellDepends = [ base ];
  testHaskellDepends = [ base doctest QuickCheck ];
  description = "Wire Reflex and OpenGL together";
  license = stdenv.lib.licenses.bsd3;
}
