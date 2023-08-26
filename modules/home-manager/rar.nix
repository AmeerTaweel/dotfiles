{pkgs, ...}: {
  # `rar` is unfreee
  imports = [./nixpkgs-unfree.nix];

  # Provides `rar` and `unrar` commands
  home.packages = [pkgs.rar];
}
