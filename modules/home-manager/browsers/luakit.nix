{
  lib,
  params,
  pkgs,
  ...
}: let
  ifDefault = lib.mkIf (params.browser == "luakit");
in {
  home.packages = [pkgs.luakit];

  xdg.mime.enable = ifDefault true;
  xdg.mimeApps.enable = ifDefault true;
  xdg.mimeApps.defaultApplications =
    ifDefault (import ./default-browser.nix "luakit");
}
