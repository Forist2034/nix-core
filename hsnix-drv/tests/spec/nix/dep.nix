with (import ./mkTestDrv.nix);
let
  outputs = import ./multiOut.nix;
  simpleIA = mkIA "simple-ia" {
    inherit outputs;
  };
  simpleFOD = mkFOD "simple-fod" {
    outputHashMode = "recursive";
  };
  simpleCA = mkCA "simple-ca" {
    inherit outputs;
    outputHashMode = "recursive";
  };
in rec {
  ia = rec {
    depFOD = mkIA "dep-fod-ia" {
      depDrv = [
        { prefix = "FOD"; drv = simpleFOD.drv; }
      ];
      inherit outputs;
    };
    depCA = mkIA "dep-ca-ia" {
      depDrv = [
        { prefix = "CA"; drv = simpleCA.drv; }
      ];
      inherit outputs;
    };
    depDeferred = mkIA "dep-deferred-ia-ia" {
      depDrv = [
        { prefix = "simpleIA"; drv = simpleIA.drv; }
        { prefix = "deferredIA"; drv = depCA.drv; }
      ];
      inherit outputs;
    };
  };
  ca = {
    depFOD = mkCA "dep-fod-ca" {
      depDrv = [
        { prefix = "FOD"; drv = simpleFOD.drv; }
      ];
      inherit outputs;
      outputHashMode = "recursive";
    };
    depRegularIA = mkCA "dep-regular-ia-ca" {
      depDrv = [
        { prefix = "regularIA"; drv = simpleIA.drv; }
      ];
      inherit outputs;
      outputHashMode = "recursive";
    };
    depIA = mkCA "dep-ia-ca" {
      depDrv = [
        { prefix = "regularIA"; drv = simpleIA.drv; }
        { prefix = "deferredIA"; drv = ia.depDeferred.drv; }
      ];
      inherit outputs;
      outputHashMode = "recursive";
    };
  };
}