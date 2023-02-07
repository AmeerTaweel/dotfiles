{pkgs, ...}: let
  mkShellScript = {
    name,
    path,
    buildInputs,
    ...
  }: let
    scriptRaw = builtins.readFile path;
    scriptUnpatched = pkgs.writeScriptBin name scriptRaw;
    scriptPatched = scriptUnpatched.overrideAttrs (old: {
      buildCommand = "${old.buildCommand}\n patchShebangs $out";
    });
  in
    pkgs.symlinkJoin {
      inherit name;
      paths = [scriptPatched] ++ buildInputs;
      buildInputs = [pkgs.makeWrapper];
      postBuild = "wrapProgram $out/bin/${name} --prefix PATH : $out/bin";
    };
in {
  power-menu = mkShellScript {
    name = "power-menu";
    path = ./power-menu.sh;
    buildInputs = [pkgs.rofi];
  };
}
