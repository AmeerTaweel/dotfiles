{pkgs, ...}: {
  # `slack` is unfree
  imports = [../nixpkgs-unfree.nix];

  home.packages = [pkgs.slack];
}
