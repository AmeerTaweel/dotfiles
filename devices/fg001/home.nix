{
  config,
  params,
  pkgs,
  ...
}: {
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

  programs.fish.shellAbbrs = let
    flake-dir = config.home.sessionVariables.FLAKEDIR;
    nixos-flake = "${flake-dir}#${params.hostname}";
    home-flake = "${flake-dir}#${params.username}@${params.hostname}";
    nix-summary = "${pkgs.nixos-change-summary}/bin/nixos-change-summary";
    hm-summary = "${pkgs.home-manager-change-summary}/bin/home-manager-change-summary";
  in {
    nx-boot = "sudo nixos-rebuild boot --flake ${nixos-flake} && ${nix-summary}";
    nx-build = "nixos-rebuild build --flake ${nixos-flake}";
    nx-switch = "sudo nixos-rebuild switch --flake ${nixos-flake} && ${nix-summary}";
    nx-summary = nix-summary;
    hm-build = "home-manager build --flake ${home-flake}";
    hm-switch = "home-manager switch --flake ${home-flake} && ${hm-summary}";
    hm-summary = hm-summary;
  };

  home.packages = with pkgs; [
  #   # Media
  #   yt-dlp
  #   kdenlive
  #   image-roll

  #   # Other
  #   gimp
  #   devour
  #   sshfs

  #   pamixer

  #   qbittorrent

  #   eva # Calculator

  # katawa-shoujo
  # upscayl
  ];

  # TODO: Check these options
  # - programs.browserpass.enable
  # - programs.gpg.enable
  # - programs.borgmatic.enable
}
