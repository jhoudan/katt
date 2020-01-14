{ mkDerivation, aeson, base, bytestring, mime-mail, mime-mail-ses
, modern-uri, req, stdenv, string-conversions, text
}:
mkDerivation {
  pname = "katt";
  version = "1.0.0";
  src = ./.;
  isLibrary = false;
  isExecutable = true;
  executableHaskellDepends = [
    aeson base bytestring mime-mail mime-mail-ses modern-uri req
    string-conversions text
  ];
  license = "unknown";
  hydraPlatforms = stdenv.lib.platforms.none;
}
