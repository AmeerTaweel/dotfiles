{pkgs, ...}: {
  # `slack` is unfreee
  imports = [./nixpkgs-unfree.nix];

  home.packages = [pkgs.slack];
}
