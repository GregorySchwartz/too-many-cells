{stdenv, fetchurl, perl, unzip, wget, which, zlib, zip}:

stdenv.mkDerivation rec {
  pname = "homer";
  version = "4.11";

  src = fetchurl {
    url = "http://homer.ucsd.edu/homer/configureHomer.pl";
    sha256 = "N+NvsQOHqQBQJRaD01/laGV5QbhjsgiNyw0J2bG1lHc=";
  };

  dontUnpack = true;
  dontFixup = true;

  # sourceRoot = ".";

  buildInputs = [ unzip wget zlib which zip ];
  nativeBuildInputs = [ perl wget ];

  # Requires internet, so no sandboxing (set to relaxed).
  __noChroot = true;

  installPhase = ''
                  mkdir -p $out
                  cp $src $out
                  perl $out/$(basename $src) -install
                  perl $out/$(basename $src) -install human
                  perl $out/$(basename $src) -install mouse
                  perl $out/$(basename $src) -install hg38
                  perl $out/$(basename $src) -install mm10
                 '';
  postInstall = ''
    wrapProgram "$out/bin/findMotifs.pl" --prefix 'PERL5LIB' ':' "$out/bin/"
    wrapProgram "$out/bin/findMotifsGenome.pl" --prefix 'PERL5LIB' ':' "$out/bin/"
  '';

}
