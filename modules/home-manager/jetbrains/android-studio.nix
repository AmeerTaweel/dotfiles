{pkgs, ...}: {
  imports = [
    ../nixpkgs-unfree.nix # `android-studio` is unfree
    ./ideavim
  ];

  home.packages = [pkgs.android-studio];
}
