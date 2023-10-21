{pkgs, ...}: {
  imports = [
    ./hardware.nix
    ./nvidia.nix
    ./steam.nix

    ./nixos-modules/core.nix
    ./nixos-modules/kumech.nix
    ./nixos-modules/nix.nix
    ./nixos-modules/nix-index.nix
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
  environment.systemPackages = [ pkgs.gnomeExtensions.appindicator ];
  # Ensure gnome-settings-daemon udev rules are enabled
  services.udev.packages = [ pkgs.gnome.gnome-settings-daemon ];

  # System-wide packages
  # environment.systemPackages = [ ];
}
