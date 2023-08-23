{pkgs, ...}: {
  # `slack` is unfreee
  imports = [
    ./nixpkgs-unfree.nix
  ];

  home.packages = with pkgs; [ slack ];
}
