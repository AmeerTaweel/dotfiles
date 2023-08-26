{
  config,
  inputs,
  params,
  pkgs,
  ...
}: {
  imports = [
    inputs.nix-colors.homeManagerModules.default

    ./home-modules/browsers/brave.nix
    ./home-modules/custom-pkgs-overlay.nix # TODO: Move to separate module
    ./home-modules/git.nix
    ./home-modules/nixvim
    ./home-modules/rar.nix
    ./home-modules/shell.nix
    ./home-modules/thefuck.nix
    ./home-modules/vim.nix
    ./home-modules/xdg.nix
  ];

  programs.home-manager.enable = true;

  home = {
    inherit (params) username;
    homeDirectory = "/home/${params.username}";
  };

  home.sessionVariables = {
    # TERMINAL = "kitty";
    FLAKEDIR = "${config.home.homeDirectory}/dotfiles/devices/${params.hostname}";
  };

  colorScheme = inputs.nix-colors.colorSchemes.ayu-dark;

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

  fonts.fontconfig.enable = true;
  # Add stuff for your user as you see fit:
  home.packages = with pkgs; [
    (nerdfonts.override {
      fonts = ["FiraCode"];
    })
    nixos-change-summary
    home-manager-change-summary
  ];
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
