# default.nix
{ compilerVersion ? "ghc865", pkgsLink ? (builtins.fetchTarball https://github.com/NixOS/nixpkgs/archive/1e90c46c2d98f9391df79954a74d14f263cad729.tar.gz)}:
let
  # Packages
  config = { allowBroken = true;
             allowUnfree = true;
             packageOverrides = super: let self = super.pkgs; in {
                Renv = super.rWrapper.override {
                  packages = with self.rPackages; [
                    devtools
                    reshape2
                    locfit
                    limma
                    jsonlite
                    ggplot2
                    cowplot
                    dplyr
                    edgeR
                  ];
                };
              };
           };
  pkgs = import pkgsLink { inherit config; };

  # Ensure working fonts with a fontconfig
  fontsConf = pkgs.makeFontsConf {
    fontDirectories = [];
  };

  # Additional dependencies
  kent = pkgs.callPackage (pkgs.fetchurl { url = "https://raw.githubusercontent.com/NixOS/nixpkgs/5482754fde6fb7ae4797951227201a7e9a14a07a/pkgs/applications/science/biology/kent/default.nix"; sha256 = "28267b39ddb19eb260bd43902f52f7870d0ab0a5b0b87cad5dbec00598eed204"; } ) {};
  macs2 = pkgs.callPackage ./deps/macs2.nix {};
  meme = pkgs.callPackage ./deps/meme.nix {};

  # Haskell compilier
  compiler = pkgs.haskell.packages."${compilerVersion}";

  # TooManyCells package
  pkg = compiler.developPackage {
    root = ./.;
    source-overrides = {
      inline-r = "0.10.2";
      terminal-progress-bar = "0.4.1";
      turtle = builtins.fetchTarball https://github.com/Gabriel439/Haskell-Turtle-Library/archive/ef0f4b15f82ac03708132726515c6bb59a0d2c07.tar.gz;
      elbow = builtins.fetchTarball https://github.com/GregorySchwartz/elbow/archive/03fff043c88b8c3de83b08f1638963f46e604c90.tar.gz;
      birch-beer = builtins.fetchTarball https://github.com/GregorySchwartz/birch-beer/archive/f4838c361485e91ff381448395179c5429b7d9d2.tar.gz;
      sparse-linear-algebra = builtins.fetchTarball https://github.com/ocramz/sparse-linear-algebra/archive/dbad792f6c6a04e4de23806b676cb3e76d36a65b.tar.gz;
      spectral-clustering = builtins.fetchTarball https://github.com/GregorySchwartz/spectral-clustering/archive/8d735caeb26266beda299d7886b5586dc7d7e7b1.tar.gz;
      differential = builtins.fetchTarball https://github.com/GregorySchwartz/differential/archive/ed557a6ade210da60a9286bfb39d12cbb80a74c7.tar.gz;
    };
    overrides = self: super: (with pkgs.haskell.lib; with pkgs.haskellPackages; {
      BiobaseNewick = doJailbreak super.BiobaseNewick;
      birch-beer = dontHaddock super.birch-beer;
      clustering = dontBenchmark (dontCheck (addExtraLibraries super.clustering [pkgs.Renv]));
      diagrams-graphviz = doJailbreak super.diagrams-graphviz;
      differential = addExtraLibraries super.differential [pkgs.Renv];
      fgl = doJailbreak super.fgl;
      hmatrix-svdlibc = dontCheck (doJailbreak super.hmatrix-svdlibc);
      pipes-text = doJailbreak super.pipes-text;
      streaming-cassava = doJailbreak super.streaming-cassava;
      typed-spreadsheet = doJailbreak super.typed-spreadsheet;
    });
  };
  buildInputs = [ pkgs.zlib
                  pkgs.zlib.dev
                  pkgs.zlib.out
                  pkgs.blas
                  pkgs.bedtools
                  pkgs.liblapack
                  pkgs.gfortran.cc.lib
                  pkgs.cairo
                  pkgs.stdenv
                  pkgs.gmp
                  pkgs.gsl
                  pkgs.gtk2
                  pkgs.pango
                  pkgs.graphviz
                  pkgs.pkg-config
                  pkgs.fontconfig
                  pkgs.Renv
                  pkgs.makeWrapper
                  pkgs.glibcLocales
                  pkgs.ghcid
                  pkgs.cabal-install
                  (pkgs.python3.withPackages (p: with p; [ numpy macs2 ]))
                  kent
                  meme
               ];
in (pkg.overrideAttrs(attrs: {
  buildInputs = attrs.buildInputs ++ buildInputs;
  nativeBuildInputs = attrs.nativeBuildInputs ++ buildInputs;
  postInstall = ''
            mkdir -p $out/paths
            cat "${pkgs.Renv}/bin/R" \
              | grep export \
              | sed "s/export R_LIBS_SITE=//" \
              | sed "s/'\$.*//" \
              > $out/paths/r_path.txt

            wrapProgram $out/bin/too-many-cells \
              --prefix 'PATH' ':' "${pkgs.bedtools}/bin/" \
              --prefix 'PATH' ':' "${pkgs.Renv}/bin/" \
              --prefix 'PATH' ':' "${pkgs.graphviz}/bin/" \
              --prefix-contents 'R_LIBS_SITE' ':' "$out/paths/r_path.txt" \
              --set 'R_LIBS_USER' "${pkgs.R}/lib/R/library" \
              --set 'LANG' 'en_US.UTF-8' \
              --set 'LOCALE_ARCHIVE' "${pkgs.glibcLocales}/lib/locale/locale-archive" \
              --set 'FONTCONFIG_FILE' "${fontsConf}"
            '';
}))
