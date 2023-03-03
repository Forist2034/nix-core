let
  mkCA = (import ./mkTestDrv.nix).mkCA;
  srcInput = import ./src.nix;
  outputs = import ./multiOut.nix;
in rec {
  flatHash = mkCA "simple-ca" {
    args = [ "1234567" "7654321" ];
    E1 = "env1";
    E2 = "env2";
    PF1 = "file1";
    PF2 = "file2";
    passAsFile = [ "PF1" "PF2" ];
    outputHashMode = "flat";
  };
  recHash = mkCA "simple-ca-rec" {
    args = [ "1234567" "7654321" ];
    E1 = "environment1";
    E2 = "environment2";
    FILE = ''
      1223
      3211
    '';
    passAsFile = [ "FILE" ];
    outputHashMode = "recursive";
  };
  multiOutput = mkCA "multi-out-ca" {
    inherit outputs;
    outputHashMode = "recursive";
  };
  dependOnSrc = mkCA "dep-src-ca" {
    depSrc = [ { name = "source"; source = srcInput.text; } ];
    inherit outputs;
    outputHashMode = "recursive";
  };
  dependOnCA = mkCA "dep-ca-ca" {
    depDrv = [
      { prefix = "inputCA1"; drv = flatHash.drv; }
      { prefix = "inputCA2"; drv = dependOnSrc.drv; }
    ];
    inherit outputs;
    outputHashMode = "recursive";
  };
}