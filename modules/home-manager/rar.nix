{pkgs, ...}: {
  # `rar` is unfree
  imports = [../nixpkgs-unfree.nix];

  # Provides `rar` and `unrar` commands
  home.packages = [pkgs.rar];
}
