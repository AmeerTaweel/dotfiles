{pkgs, ...}: {
  imports = [
    ./hardware.nix
    ./nvidia.nix
    ./steam.nix

    ./modules/nixos/core.nix
    ./modules/nixos/kumech.nix
    ./modules/nixos/nix.nix
    ./modules/nixos/nix-index.nix
  ];

  # Bootloader
  boot.loader.systemd-boot.enable = true;
  boot.loader.efi.canTouchEfiVariables = true;

  # Enable the X11 windowing system
  services.xserver.enable = true;
  # Enable the GNOME Desktop Environment
  services.xserver.displayManager.gdm.enable = true;
  services.xserver.desktopManager.gnome.enable = true;
  # Install the systray Gnome shell extension
  environment.systemPackages = with pkgs.gnomeExtensions; [
    appindicator # System Tray
    pano # Clipboard Manager
  ];
  # Ensure gnome-settings-daemon udev rules are enabled
  services.udev.packages = [pkgs.gnome.gnome-settings-daemon];

  # System-wide packages
  # environment.systemPackages = [ ];
}
