{pkgs, ...}: {
  imports = [
    ../nixpkgs-unfree.nix # `idea-ultimate` is unfreee
    ./ideavim
  ];

  home.packages = [pkgs.jetbrains.idea-ultimate];
}
