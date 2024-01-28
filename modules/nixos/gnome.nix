{pkgs, ...}: {
  # Enable the X11 windowing system
  services.xserver.enable = true;

  # Enable the GNOME Desktop Environment
  services.xserver.displayManager.gdm.enable = true;
  services.xserver.desktopManager.gnome.enable = true;

  # Exclude default GNOME packages
  environment.gnome.excludePackages =
    (with pkgs; [
      gnome-console
      gnome-photos
      gnome-tour
      gedit # text editor
    ])
    ++ (with pkgs.gnome; [
      epiphany # web browser
      evince # document viewer
      geary # email reader
      gnome-calendar
      gnome-characters
      gnome-contacts
      gnome-maps
      gnome-music
      gnome-terminal
      gnome-weather
      totem # video player
      yelp # help viewer
    ]);

  # Install GNOME extensions
  environment.systemPackages =
    (with pkgs.gnome; [
      gnome-tweaks
    ])
    ++ (with pkgs.gnomeExtensions; [
      hibernate-status-button # Hibernate in Power Options
      appindicator # System Tray
      pano # Clipboard Manager
      just-perfection
    ]);

  # Ensure gnome-settings-daemon udev rules are enabled
  services.udev.packages = [pkgs.gnome.gnome-settings-daemon];
}
