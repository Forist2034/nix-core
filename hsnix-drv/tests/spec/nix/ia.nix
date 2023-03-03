let
  mkIA = (import ./mkTestDrv.nix).mkIA;
  src = import ./src.nix;
  outputs = import ./multiOut.nix;
in rec {
  simple = mkIA "simple-ia" {
    args = ["1234567" "7654321"];
    E1 = "environment 1";
    E2 = "environment 2";
    PassFile1 = "12345";
    PassFile2 = "54321";
    passAsFile = [ "PassFile1" "PassFile2" ];
  };
  multiOutput = mkIA "multi-out-ia" {
    inherit outputs;
  };
  dependOnSrc = mkIA "dep-src-ia" {
    depSrc = [ { name = "source"; source = src.text; } ];
    inherit outputs;
  };
  dependOnIA = mkIA "dep-ia-ia" {
    depDrv = [
      { prefix = "inputIA1"; drv = simple.drv; }
      { prefix = "inputIA2"; drv = dependOnSrc.drv; }
    ];
    inherit outputs;
  };
}