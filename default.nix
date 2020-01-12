{ mkDerivation, base, foldl, stdenv, turtle }:
mkDerivation {
  pname = "katt";
  version = "1.0.0";
  src = ./.;
  isLibrary = false;
  isExecutable = true;
  executableHaskellDepends = [ base foldl turtle ];
  license = "unknown";
  hydraPlatforms = stdenv.lib.platforms.none;
}
