{
  lib,
  params,
  pkgs,
  ...
}: let
  ifDefault = lib.mkIf (params.browser == "opera");
in {
  # `opera` is unfree
  imports = [../../nixpkgs-unfree.nix];

  home.packages = [pkgs.opera];

  xdg.mime.enable = ifDefault true;
  xdg.mimeApps.enable = ifDefault true;
  xdg.mimeApps.defaultApplications =
    ifDefault (import ./default-browser.nix "opera");
}
