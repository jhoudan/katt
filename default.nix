{ mkDerivation, aeson, base, bytestring, http-conduit, mime-mail
, mime-mail-ses, stdenv, string-conversions, text
}:
mkDerivation {
  pname = "katt";
  version = "1.0.0";
  src = ./.;
  isLibrary = false;
  isExecutable = true;
  executableHaskellDepends = [
    aeson base bytestring http-conduit mime-mail mime-mail-ses
    string-conversions text
  ];
  license = "unknown";
  hydraPlatforms = stdenv.lib.platforms.none;
}
