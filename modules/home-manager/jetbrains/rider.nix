{pkgs, ...}: {
  imports = [
    ../nixpkgs-unfree.nix # `rider` is unfreee
    ./ideavim
  ];

  home.packages = [pkgs.jetbrains.rider];
}
