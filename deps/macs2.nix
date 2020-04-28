{ pkgs, lib, buildPythonPackage ? pkgs.python3.pkgs.buildPythonPackage, fetchurl, numpy ? pkgs.python3.pkgs.numpy}:

buildPythonPackage rec {
    pname = "macs2";
    version = "2.2.6";
    src = fetchurl { url = "https://files.pythonhosted.org/packages/21/0f/972b44c84d85e37d816beae88aa5ddad606bd757630d77dc2f558900a6ce/MACS2-2.2.6.tar.gz"; sha256 = "ca9e43f8cf15312ace9afa0a9bf67e8072227376235b7b0f18dd2dcac17dea42";};
    propagatedBuildInputs = [ numpy ];
    doCheck = false;
}
