{pkgs, ...}: {
  imports = [
    ../nixpkgs-unfree.nix # `rider` is unfree
    ./ideavim
  ];

  home.packages = [pkgs.jetbrains.rider];
}
