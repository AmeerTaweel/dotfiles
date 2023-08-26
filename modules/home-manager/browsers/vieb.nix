{
  lib,
  params,
  pkgs,
  ...
}: let
  ifDefault = lib.mkIf (params.browser == "vieb");
in {
  home.packages = [pkgs.vieb];

  xdg.mime.enable = ifDefault true;
  xdg.mimeApps.enable = ifDefault true;
  xdg.mimeApps.defaultApplications =
    ifDefault (import ./default-browser.nix "vieb");
}
