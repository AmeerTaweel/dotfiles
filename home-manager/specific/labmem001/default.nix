# This is your home-manager configuration file
# Use this to configure your home environment (it replaces ~/.config/nixpkgs/home.nix)
{
  inputs,
  outputs,
  lib,
  config,
  pkgs,
  ...
}: {
  # You can import other home-manager modules here
  imports = [
    # If you want to use modules your own flake exports (from modules/home-manager):
    # outputs.homeManagerModules.example
    outputs.homeManagerModules.autostart
    outputs.homeManagerModules.thefuck

    # Or modules exported from other flakes (such as nix-colors):
    inputs.nix-colors.homeManagerModules.default

    # You can also split up your configuration and import pieces of it here:
    ../../generic/copyq.nix
    ../../generic/dunst.nix
    ../../generic/git.nix
    ../../generic/nixpkgs.nix
    ../../generic/qutebrowser.nix
    ../../generic/tmux.nix
    ../../generic/zathura.nix
    ../../generic/vim.nix
    ../../generic/nvim.nix
    ../../generic/keyboard.nix
    ../../generic/kitty.nix
    ../../generic/alacritty.nix
    ../../generic/discord.nix
    ../../generic/telegram.nix
    ../../generic/top.nix
    ../../generic/fonts.nix
    ../../generic/gammastep.nix
    ../../generic/direnv.nix
    ../../generic/video-players.nix
    ../../generic/file-managers.nix
    ../../generic/emacs.nix
    ../../generic/music.nix
    ../../generic/rofi.nix
    ../../generic/sxhkd.nix
    ../../generic/shell.nix

    # ./vscode.nix
    # ./xdg.nix
  ];

  # You can add overlays here
  nixpkgs.overlays = [
    # Add overlays your own flake exports (from overlays and pkgs dir):
    outputs.overlays.modifications
    outputs.overlays.additions

    # You can also add overlays exported from other flakes:
    # neovim-nightly-overlay.overlays.default
    inputs.za-zombie.overlays.default

    # Or define it inline, for example:
    # (final: prev: {
    #   hi = final.hello.overrideAttrs (oldAttrs: {
    #     patches = [ ./change-hello-to-hi.patch ];
    #   });
    # })
  ];

  home = {
    username = "labmem001";
    homeDirectory = "/home/labmem001";
  };

  home.sessionVariables = {
    TERMINAL = "kitty";
    EDITOR = "${pkgs.vim}/bin/vim";
    MANPAGER = "vim -M +MANPAGER -";
    # OR neovim
    # EDITOR = "${pkgs.neovim}/bin/nvim";
    # MANPAGER = "nvim +Man!";
  };

  colorScheme = inputs.nix-colors.colorSchemes.ayu-dark;

  home.keyboard.layout = "us,ar,tr";

  # TODO: START FIX

  # Add stuff for your user as you see fit:
  # programs.neovim.enable = true;
  # home.packages = with pkgs; [ steam ];
  home.packages = with pkgs; [
    # Browsers
    google-chrome
    firefox
    opera
    vieb
    luakit
    tor-browser-bundle-bin
    brave

    # Communication
    element-desktop
    slack
    zoom-us

    # Help
    tealdeer # TLDR
    cht-sh

    # Media
    okular
    koreader
    yt-dlp
    kdenlive
    image-roll

    # IDE's
    # android-studio
    jetbrains.idea-ultimate
    # jetbrains.rider

    # Chess
    stockfish
    gnuchess
    chessx

    # Other
    stow
    ffmpeg
    imagemagick
    bat
    fd
    ripgrep
    rar
    zip
    unzip
    gimp
    octaveFull # Gnu Octave
    libreoffice
    postman
    devour
    sshfs
    unityhub
    # TODO: Decide on archiver
    xarchiver
    alsa-utils
    playerctl

    file
    tree
    curl
    # TODO: Fix
    # shell-scripts = import ../../../shell-scripts {inherit pkgs;};
    # shell-scripts.power-menu

    # entr
    # hplip-gui

    xorg.xmessage
    xkb-switch
    anydesk

    sqlite
    emacsPackages.emacsql-sqlite # for org-roam
    gcc

    pamixer

    virt-manager
  ];

  home.file.ideaVimRC = {
    source = ../../../config/idea-vim/ideavimrc;
    target = ".ideavimrc";
  };
  xdg.configFile.ideaVimConfig = {
    source = ../../../config/idea-vim/config;
    target = "idea-vim";
    recursive = true;
  };

  programs.obs-studio.enable = true;

  # Enable home-manager and git
  programs.home-manager.enable = true;
  programs.git.userName = "Ameer Taweel";
  programs.git.userEmail = "ameertaweel2002@gmail.com";

  # Use Bluetooth headset buttons to control media player
  services.mpris-proxy.enable = true;
  services.blueman-applet.enable = true;
  services.network-manager-applet.enable = true;
  # Automount removable media
  services.udiskie.enable = true;

  gtk.enable = true;
  gtk.iconTheme.package = pkgs.gnome.adwaita-icon-theme;
  gtk.iconTheme.name = "Adwaita";
  gtk.theme.package = pkgs.gnome.gnome-themes-extra;
  gtk.theme.name = "Adwaita";

  gtk.gtk2.extraConfig = ''
    gtk-application-prefer-dark-theme = "true"
  '';
  gtk.gtk3.extraConfig = {
    gtk-application-prefer-dark-theme = true;
  };
  gtk.gtk4.extraConfig = {
    gtk-application-prefer-dark-theme = true;
  };

  services.picom.enable = true;

  xdg.configFile.qtileConfig = {
    source = ../../../config/qtile;
    target = "qtile";
    recursive = true;
  };

  xdg.configFile.awesomewmConfig = {
    source = ../../../config/awesomewm;
    target = "awesome";
    recursive = true;
  };

  # xdg.configFile.awesomewmTheme = {
  #   text = "return require('ui.themes.${user.theme}.theme')";
  #   target = "awesome/ui/theme.lua";
  # };

  # TODO: Check these options
  # - programs.autojump.enable
  #   https://github.com/wting/autojump
  # - programs.browserpass.enable
  # - programs.firefox.enable
  # - programs.chromium.enable
  # - programs.gpg.enable
  # - programs.borgmatic.enable
  # - services.emacs.enable

  programs.thefuck.enable = true;

  # TODO: END FIX

  # Nicely reload system units when changing configs
  systemd.user.startServices = "sd-switch";

  # https://nixos.wiki/wiki/FAQ/When_do_I_update_stateVersion
  home.stateVersion = "21.11";
}
