{pkgs, ...}: {
  imports = [
    ./home-modules/anydesk.nix
    ./home-modules/browsers/brave.nix
    ./home-modules/core.nix
    ./home-modules/custom-pkgs-overlay.nix # TODO: Move to separate module
    ./home-modules/documents/zathura.nix
    ./home-modules/emacs.nix
    ./home-modules/fonts.nix
    ./home-modules/git.nix
    ./home-modules/kitty.nix
    ./home-modules/nixvim
    ./home-modules/nnn.nix
    ./home-modules/rar.nix
    ./home-modules/shell.nix
    ./home-modules/slack.nix
    ./home-modules/social/discord.nix
    ./home-modules/social/facebook-messenger.nix
    ./home-modules/social/signal.nix
    ./home-modules/social/telegram.nix
    ./home-modules/social/whatsapp.nix
    ./home-modules/video-players/mpv.nix
    ./home-modules/vim
    ./home-modules/xdg.nix
  ];

  home.packages = with pkgs; [
    gimp
    qbittorrent
  ];
}
