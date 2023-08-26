{pkgs, ...}: {
  imports = [
    ../nixpkgs-unfree.nix # `idea-ultimate` is unfree
    ./ideavim
  ];

  home.packages = [pkgs.jetbrains.idea-ultimate];
}
