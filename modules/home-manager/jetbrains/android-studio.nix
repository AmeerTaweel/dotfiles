{pkgs, ...}: {
  imports = [
    ../nixpkgs-unfree.nix # `android-studio` is unfreee
    ./ideavim
  ];

  home.packages = [ pkgs.android-studio ];
}
