{stdenv, fetchurl, python3, perl, glibc, zlib }:

stdenv.mkDerivation rec {
  pname = "meme";
  version = "5.1.1";

  src = fetchurl {
    url = "http://meme-suite.org/meme-software/5.1.1/meme-5.1.1.tar.gz";
    sha256 = "38d73d256d431ad4eb7da2c817ce56ff2b4e26c39387ff0d6ada088938b38eb5";
  };

  nativeBuildInputs = [ perl python3 zlib ];

}
