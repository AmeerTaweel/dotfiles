{pkgs, ...}: {
  imports = [
    ../../nixpkgs-unfree.nix # `android-studio` is unfree
    ./ideavim
  ];

  # `pkgs.jetbrains.plugins.addPlugins` doesn't work with Android Studio
  home.packages = [pkgs.android-studio];
}
