{
  lib,
  params,
  pkgs,
  ...
}: let
  ifDefault = lib.mkIf (params.browser == "nyxt");
in {
  home.packages = [pkgs.nyxt];

  xdg.mime.enable = ifDefault true;
  xdg.mimeApps.enable = ifDefault true;
  xdg.mimeApps.defaultApplications =
    ifDefault (import ./default-browser.nix "nyxt");
}
