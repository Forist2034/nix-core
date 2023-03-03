let srcInput = import ./src.nix;
    mkFOD = (import ./mkTestDrv.nix).mkFOD;
in rec {
  hashFlat = mkFOD "simple-fod1" {
    args = [ "12345" "67890" ];
    ENV1 = "v1";
    ENV2 = "v2";
    FILE1 = "content1";
    FILE2 = "content2";
    passAsFile = ["FILE1" "FILE2"] ;
    outputHashMode = "flat";
  };
  hashRec = mkFOD "simple-fod-rec" {
    args = [ "123" "456" ];
    E1 = "v1";
    E2 = "v2";
    F1 = "c1";
    F2 = "c2";
    passAsFile = [ "F1" "F2" ];
    outputHashMode = "recursive";
  };
  dependOnSrc = mkFOD "dep-src-fod" {
    depSrc = [
      { name = "source"; source = srcInput.text; }
    ];
    outputHashMode = "recursive";
  };
  dependOnFOD = mkFOD "dep-fod-fod" {
    depDrv = [
      { prefix = "inputFOD1"; drv = hashRec.drv; }
      { prefix = "inputFOD2"; drv = dependOnSrc.drv; }
    ];
    outputHashMode = "recursive";
  };
}