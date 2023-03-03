let
  mkDrv= name: {builder ? "builder", depDrv ? [], depSrc ? [], ...}@attrs:
    let
      depDrvArgs = builtins.map ({prefix, drv}: drv.outPath) depDrv;
      depDrvEnvs =
        builtins.listToAttrs
          ( builtins.concatMap
              ( {prefix, drv}:
                  builtins.map
                    (o: {
                      name = "${prefix}${o}";
                      value = drv.${o}.outPath;
                    })
                    drv.outputs
              )
              depDrv
          );
      srcEnvs =
        builtins.listToAttrs
          ( builtins.map
              ({name, source}: {inherit name; value = source;})
              depSrc
          );
      baseAttr = {
        inherit name builder;
        system = "x86_64-linux";
        outputs = [ "out" ];
        preferLocalBuild = false;
        allowSubstitutes = true;
        passAsFile = [];
      } // builtins.removeAttrs attrs ["depDrv" "depSrc"]; 
      drv = builtins.derivation (baseAttr // depDrvEnvs // srcEnvs // {
        args = depDrvArgs ++ (baseAttr.args or []);
        passAsFile =
          builtins.map ({name, source}: name) depSrc
            ++ baseAttr.passAsFile;
      });
    in {
      inherit drv;
      extractCmd = path: ''
        echo "writing dev info ${name}"
        jq "${path}" "''$1" > "${name}.json"
        cp -fv "${drv.drvPath}" "${name}.drv"
       '';
      result = {
        inherit name;
        drv-path = drv.drvPath;
        refs =
          builtins.map ({name, source}: source) depSrc
            ++ builtins.map ({prefix, drv}: drv.drvPath) depDrv;
        default-output = drv.outputName;
        default-store-path = drv.outPath;
        outputs =
          builtins.listToAttrs (
            builtins.map
              (x: {name = x; value = drv.${x}.outPath;})
              drv.outputs);
      };
    };
in {
  mkIA = mkDrv;

  mkFOD = 
    let simpleHash = "8bb0cf6eb9b17d0f7d22b456f121257dc1254e1f01665370476383ea776df414"; # sha256 "1234567"
    in name: 
       { outputHashMode,
         outputHashAlgo ? "sha256",
         outputHash ? simpleHash,
         ...}@attrs:
         mkDrv
           name
           ( {inherit outputHashAlgo outputHash;} // attrs);
  
  mkCA =
    name:
    { outputHashMode,
      outputHashAlgo ? "sha256",
      ... }@attrs:
    mkDrv
      name
      ({ inherit outputHashAlgo; __contentAddressed = true; } // attrs);
  
  collectRes = attrs: {
    extractCmd = name:
      builtins.concatStringsSep
        "\n"
        ( builtins.attrValues
            ( builtins.mapAttrs
                ( label: value: value.extractCmd "${name}.${label}")
                attrs
            )
        );
    result = builtins.mapAttrs (label: value: value.result) attrs;
  };
}