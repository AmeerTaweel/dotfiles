{pkgs, ...}: {
  # `rar` is unfreee
  imports = [
    ./nixpkgs-unfree.nix
  ];

  home.packages = with pkgs; [
    # Provides `rar` and `unrar` commands
    rar
  ];
}
