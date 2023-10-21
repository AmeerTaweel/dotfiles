# This is your home-manager configuration file
# Use this to configure your home environment (it replaces ~/.config/nixpkgs/home.nix)
{ pkgs, ... }: {
  home.keyboard.layout = "us,ar,tr";

  programs.fish.shellAbbrs = {
    sl = "xrandr --setprovideroutputsource NVIDIA-G0 modesetting && xrandr --output eDP-1 --auto --output HDMI-1-0 --auto --left-of  eDP-1";
    ss = "xrandr --setprovideroutputsource NVIDIA-G0 modesetting && xrandr --output eDP-1 --auto --output HDMI-1-0 --auto --same-as  eDP-1";
    sr = "xrandr --setprovideroutputsource NVIDIA-G0 modesetting && xrandr --output eDP-1 --auto --output HDMI-1-0 --auto --right-of eDP-1";
    ts = "sudo systemctl restart systemd-timesyncd.service";
  };

  # Enable PlayerCTL
  services.playerctld.enable = true;

  # Add stuff for your user as you see fit:
  # programs.neovim.enable = true;
  # home.packages = with pkgs; [ steam ];
  home.packages = with pkgs; [
    # Communication
    element-desktop
    slack
    zoom-us

    # Help
    tealdeer # TLDR
    cht-sh

    # Media
    yt-dlp
    kdenlive
    image-roll

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
    postman
    devour
    sshfs
    unityhub
    # TODO: Decide on archiver
    xarchiver
    alsa-utils

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

    qbittorrent

    eva # Calculator

    katawa-shoujo

    # upscayl
  ];

  programs.obs-studio.enable = true;

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
}
