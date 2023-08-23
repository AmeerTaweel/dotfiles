{pkgs, ...}: {
  # `anydesk` is unfreee
  imports = [
    ./nixpkgs-unfree.nix
  ];

  home.packages = with pkgs; [ anydesk ];
}
