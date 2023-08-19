# This is your home-manager configuration file
# Use this to configure your home environment (it replaces ~/.config/nixpkgs/home.nix)
{
  config,
  inputs,
  pkgs,
  params,
  ...
}: {
  imports = [
    # Nix-Colors HM Module
    inputs.nix-colors.homeManagerModules.default

    ./home-modules/rar.nix
    ./home-modules/shell.nix
    ./home-modules/xdg.nix
  ];

  home = {
    inherit (params) username;
    homeDirectory = "/home/${params.username}";
  };

  home.sessionVariables = {
    # TERMINAL = "kitty";
    EDITOR = "${pkgs.neovim}/bin/nvim";
    MANPAGER = "${pkgs.neovim}/bin/nvim +Man!";
    FLAKEDIR = "${config.home.homeDirectory}/dotfiles/devices/${params.hostname}";
    HISTFILE = "${config.xdg.stateHome}/bash/history";
  };

  colorScheme = inputs.nix-colors.colorSchemes.ayu-dark;

  programs.fish.shellAbbrs = let
    flake-dir = config.home.sessionVariables.FLAKEDIR;
    nixos-flake = "${flake-dir}#${params.hostname}";
    home-flake = "${flake-dir}#${params.username}@${params.hostname}";
  in {
    nx-boot = "sudo nixos-rebuild boot --flake ${nixos-flake}";
    nx-build = "nixos-rebuild build --flake ${nixos-flake}";
    nx-switch = "sudo nixos-rebuild switch --flake ${nixos-flake}";
    hm-build = "home-manager build --flake ${home-flake}";
    hm-switch = "home-manager switch --flake ${home-flake}";
    # ts = "sudo systemctl restart systemd-timesyncd.service";
  };

  # TODO: START FIX

  # xdg.mime.enable = true;
  # xdg.mimeApps.enable = true;
  # xdg.mimeApps.defaultApplications = {
  #   "text/html" = "google-chrome.desktop";
  #   "x-scheme-handler/http" = "google-chrome.desktop";
  #   "x-scheme-handler/https" = "google-chrome.desktop";
  #   "x-scheme-handler/about" = "google-chrome.desktop";
  #   "x-scheme-handler/unknown" = "google-chrome.desktop";
  #   "video/mp4" = "mpv.desktop";
  #   "application/pdf" = "zathura.desktop";
  # };

  # Add stuff for your user as you see fit:
  home.packages = with pkgs; [ vim neovim brave ];
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
  # ];

  # Enable home-manager and git
  programs.home-manager.enable = true;
  programs.git.userName = params.name;
  programs.git.userEmail = params.email;

  programs.git = {
    enable = true;
    extraConfig = {
      init = {
        defaultBranch = "master";
      };
    };
  };

  # Use Bluetooth headset buttons to control media player
  # services.mpris-proxy.enable = true;
  # services.blueman-applet.enable = true;
  # services.network-manager-applet.enable = true;
  # Automount removable media
  # services.udiskie.enable = true;

  # TODO: Check these options
  # - programs.browserpass.enable
  # - programs.gpg.enable
  # - programs.borgmatic.enable

  # programs.thefuck.enable = true;

  # TODO: END FIX

  # Nicely reload system units when changing configs
  systemd.user.startServices = "sd-switch";

  # https://nixos.wiki/wiki/FAQ/When_do_I_update_stateVersion
  home.stateVersion = params.state-version;
}
