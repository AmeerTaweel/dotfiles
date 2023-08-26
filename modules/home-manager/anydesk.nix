{pkgs, ...}: {
  # `anydesk` is unfree
  imports = [./nixpkgs-unfree.nix];

  home.packages = [pkgs.anydesk];
}
