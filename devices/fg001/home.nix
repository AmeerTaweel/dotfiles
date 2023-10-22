{pkgs, ...}: {
  imports = [
    ./modules/home-manager/anydesk.nix
    ./modules/home-manager/browsers/brave.nix
    ./modules/home-manager/core.nix
    ./modules/home-manager/documents/zathura.nix
    ./modules/home-manager/emacs
    ./modules/home-manager/fonts.nix
    ./modules/home-manager/git.nix
    ./modules/home-manager/kitty.nix
    ./modules/home-manager/nixvim
    ./modules/home-manager/nnn.nix
    ./modules/home-manager/rar.nix
    ./modules/home-manager/shell.nix
    ./modules/home-manager/slack.nix
    ./modules/home-manager/social/discord.nix
    ./modules/home-manager/social/facebook-messenger.nix
    ./modules/home-manager/social/signal.nix
    ./modules/home-manager/social/telegram.nix
    ./modules/home-manager/social/whatsapp.nix
    ./modules/home-manager/tmux
    ./modules/home-manager/video-players/mpv.nix
    ./modules/home-manager/vim
    ./modules/home-manager/xdg.nix
  ];

  home.packages = with pkgs; [
    gimp
    qbittorrent
  ];
}
