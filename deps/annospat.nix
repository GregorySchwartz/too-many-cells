{ lib, python3, fetchurl }:

python3.pkgs.buildPythonPackage rec {
  pname = "AnnoSpat";
  version = "0.0.99";

  src = fetchurl { url = "https://test-files.pythonhosted.org/packages/e2/be/fbc0d074ec1bc551a0ff29cf1af2fd0f1a26e98eaa2d4e1846bbf43cc875/AnnoSpat-0.0.99.tar.gz"; sha256 = "04dzkwwk9h45905jy3j3rp5jkg10h2p0c8mdpj3s143hb1d7qhkr"; };
  # src = python3.pkgs.fetchPypi {
  #   inherit pname version;
  #   sha256 = "1v0kmziqnaxlrr0b2v0z1yjx5lrzd9n6xajydc1ll5izsnkpfyij";
  # };

  propagatedBuildInputs =
    with python3.pkgs;
      [
        matplotlib
        numpy
        pandas
        scikitlearn
        scipy
        typer
      ];

  # To prevent ERROR: diffpeak_cmd (unittest.loader._FailedTest) for obsolete
  # function (ImportError: Failed to import test module: diffpeak_cmd)
  doCheck = false;
  pythonImportsCheck = [ "AnnoSpat" ];

  meta = with lib; {
    description = "A tool for annotating and inferring patterns in single cells from spatial proteomics";
    license = licenses.gpl3;
    maintainers = with maintainers; [ gschwartz ];
    platforms = platforms.linux;
  };
}
