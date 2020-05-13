{ pkgsLink ? (builtins.fetchTarball https://github.com/NixOS/nixpkgs/archive/1e90c46c2d98f9391df79954a74d14f263cad729.tar.gz)}:
let
  pkgs = import pkgsLink {};
  pkg = pkgs.callPackage ./default.nix {};
  debian = pkgs.dockerTools.pullImage {
            imageName = "debian";
            imageDigest = "sha256:666ffd4fbcdff07edcbd65f0ec1dc86ed294f5e3be93eb26280575f77427df46";
            sha256 = "1wmzsj63i0jmsxp5gsmmlz7cx500xfv2lwcifgkir5gldyc3pnll";
  };
in
  pkgs.dockerTools.buildImage {
    name = "gregoryschwartz/too-many-cells";
    tag = "latest";

    fromImage = debian;
    contents = pkg;

    created = "now";  # Breaks reproducibility but enables sane build time point.

    config = {
      Entrypoint = [ "too-many-cells" ];
    };
  }
