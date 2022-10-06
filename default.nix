# default.nix
{ compilerVersion ? "ghc8104", pkgsLink ? (builtins.fetchTarball https://github.com/NixOS/nixpkgs/archive/7124e24a47d3c3f3329e80ca7a143ee9f37a6817.tar.gz)}:
let
  # Packages
  config = { allowBroken = true;
             allowUnfree = true;
             packageOverrides = super: let self = super.pkgs; in {
                Renv = super.rWrapper.override {
                  packages = with self.rPackages; [
                    cowplot
                    devtools
                    dplyr
                    edgeR
                    ggplot2
                    jsonlite
                    limma
                    locfit
                    reshape2
                    spatstat
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
  annospat = pkgs.callPackage ./deps/annospat.nix {};
  homer = pkgs.callPackage ./deps/homer.nix {};

  # Haskell compiler
  compiler = pkgs.haskell.packages."${compilerVersion}";

  # TooManyCells package
  pkg = compiler.developPackage {
    name = "too-many-cells";
    root = builtins.filterSource
            (path: type:
              baseNameOf path != ".git"
              && baseNameOf path != "workshop"
              && baseNameOf path != "too-many-peaks_doc"
              && baseNameOf path != "dist-newstyle"
              && baseNameOf path != "img"
              && baseNameOf path != "stack-work-profile"
              && baseNameOf path != ".stack-work"
              && baseNameOf path != "dist")
            ./.;  # Filter out directories. Is recursive, but excluding a directory excludes all paths below it too.
    source-overrides = {
      birch-beer = builtins.fetchTarball https://github.com/GregorySchwartz/birch-beer/archive/5f8123a9a0aced7b997630b55f640d26296ed2f2.tar.gz;
      hvega-theme = builtins.fetchTarball https://github.com/GregorySchwartz/hvega-theme/archive/66071f477e7a76d9cfbf0339dc4dc2d56c4c1748.tar.gz;
      ploterific = builtins.fetchTarball https://github.com/GregorySchwartz/ploterific/archive/ae02c2afad10d7ec9d05044021697d3e9036095f.tar.gz;
      sparse-linear-algebra = builtins.fetchTarball https://github.com/ocramz/sparse-linear-algebra/archive/dbad792f6c6a04e4de23806b676cb3e76d36a65b.tar.gz;
      terminal-progress-bar = "0.4.1";
    };
    overrides = self: super: (with pkgs.haskell.lib; with pkgs.haskellPackages; {
      BiobaseNewick = doJailbreak super.BiobaseNewick;
      birch-beer = dontHaddock super.birch-beer;
      clustering = dontBenchmark (dontCheck (addExtraLibraries super.clustering [pkgs.Renv]));
      diagrams-cairo = doJailbreak super.diagrams-cairo;
      diagrams-graphviz = doJailbreak super.diagrams-graphviz;
      diagrams-gtk = doJailbreak super.diagrams-gtk;
      differential = addExtraLibraries super.differential [pkgs.Renv];
      fgl = doJailbreak super.fgl;
      hmatrix-svdlibc = dontCheck (doJailbreak super.hmatrix-svdlibc);
      palette = doJailbreak super.palette;
      pipes-text = doJailbreak super.pipes-text;
      streaming-cassava = doJailbreak super.streaming-cassava;
      streaming-utils = doJailbreak super.streaming-utils;
      streaming-with = doJailbreak super.streaming-with;
      typed-spreadsheet = doJailbreak super.typed-spreadsheet;
    });
  };
  buildInputs = [ annospat
                  homer
                  pkgs.MACS2
                  pkgs.Renv
                  pkgs.bedtools
                  pkgs.blas
                  pkgs.cabal-install
                  pkgs.cairo
                  pkgs.coreutils
                  pkgs.fontconfig
                  pkgs.gfortran.cc.lib
                  pkgs.ghcid
                  pkgs.glibcLocales
                  pkgs.gmp
                  pkgs.graphviz
                  pkgs.gsl
                  pkgs.gtk2
                  pkgs.gzip
                  pkgs.kent
                  pkgs.liblapack
                  pkgs.makeWrapper
                  pkgs.meme-suite
                  pkgs.pango
                  pkgs.pkg-config
                  pkgs.stdenv
                  pkgs.zlib
                  pkgs.zlib.dev
                  pkgs.zlib.out
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
              --prefix 'PATH' ':' "${pkgs.Renv}/bin/" \
              --prefix 'PATH' ':' "${pkgs.gzip}/bin/" \
              --prefix 'PATH' ':' "${pkgs.coreutils}/bin/" \
              --prefix 'PATH' ':' "${pkgs.graphviz}/bin/" \
              --prefix 'PATH' ':' "${pkgs.bedtools}/bin/" \
              --prefix 'PATH' ':' "${pkgs.kent}/bin/" \
              --prefix 'PATH' ':' "${pkgs.MACS2}/bin/" \
              --prefix 'PATH' ':' "${pkgs.meme-suite}/bin/" \
              --prefix 'PATH' ':' "${annospat}/bin/" \
              --prefix 'PATH' ':' "${homer}/bin/" \
              --prefix-contents 'R_LIBS_SITE' ':' "$out/paths/r_path.txt" \
              --set 'R_LIBS_USER' "${pkgs.R}/lib/R/library" \
              --set 'LANG' 'en_US.UTF-8' \
              --set 'LOCALE_ARCHIVE' "${pkgs.glibcLocales}/lib/locale/locale-archive" \
              --set 'FONTCONFIG_FILE' "${fontsConf}"
            '';
}))
