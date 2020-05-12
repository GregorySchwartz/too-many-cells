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
  compiler = pkgs.haskell.packages."${compilerVersion}";

  # TooManyCells package
  pkg = compiler.developPackage {
    root = ./.;
    source-overrides = {
      inline-r = "0.10.2";
      terminal-progress-bar = "0.2";
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
               ];
in (pkgs.haskell.lib.dontHaddock (pkg.overrideAttrs(attrs: {
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
              --prefix-contents 'R_LIBS_SITE' ':' "$out/paths/r_path.txt" \
              --set 'R_LIBS_USER' "${pkgs.R}/lib/R/library" \
              --set 'LANG' 'en_US.UTF-8' \
              --set 'LOCALE_ARCHIVE' "${pkgs.glibcLocales}/lib/locale/locale-archive" \
              --set 'FONTCONFIG_FILE' "${fontsConf}"
            '';
})))
