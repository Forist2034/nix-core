let
  inherit (import ./mkTestDrv.nix) collectRes;
  res = collectRes {
    fixedOutput = collectRes (import ./fod.nix);
    inputAddressed = collectRes (import ./ia.nix);
    contentAddressed = collectRes (import ./ca.nix);
    depend =
      collectRes
        ( builtins.mapAttrs
            (name: value: collectRes value)
            (import ./dep.nix)
        );
  };
in {
  extractCmd = res.extractCmd "";
  result = res.result;
}