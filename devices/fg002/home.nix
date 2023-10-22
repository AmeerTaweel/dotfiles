{...}: {
  imports = [
    ./home-modules/browsers/brave.nix
    ./home-modules/core.nix
    ./home-modules/custom-pkgs-overlay.nix # TODO: Move to separate module
    ./home-modules/documents/zathura.nix
    ./home-modules/fonts.nix
    ./home-modules/git.nix
    ./home-modules/nixvim
    ./home-modules/nnn.nix
    ./home-modules/rar.nix
    ./home-modules/shell.nix
    ./home-modules/tmux
    ./home-modules/vim
    ./home-modules/xdg.nix
  ];
}
