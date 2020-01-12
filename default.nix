{ mkDerivation, aeson, base, bytestring, mime-mail, mime-mail-ses
, stdenv, string-conversions, text, turtle
}:
mkDerivation {
  pname = "katt";
  version = "1.0.0";
  src = ./.;
  isLibrary = false;
  isExecutable = true;
  executableHaskellDepends = [
    aeson base bytestring mime-mail mime-mail-ses string-conversions
    text turtle
  ];
  license = "unknown";
  hydraPlatforms = stdenv.lib.platforms.none;
}
