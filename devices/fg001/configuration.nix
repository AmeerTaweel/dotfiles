{...}: {
  imports = [
    ./hardware.nix
    ./nvidia.nix
    ./steam.nix

    ./nixos-modules/bluetooth.nix
    ./nixos-modules/core.nix
    ./nixos-modules/kumech.nix
    ./nixos-modules/networking.nix
    ./nixos-modules/nix.nix
    ./nixos-modules/nix-index.nix
    ./nixos-modules/pipewire.nix
    ./nixos-modules/physlock.nix
  ];

  # Bootloader
  boot.loader.systemd-boot.enable = true;
  boot.loader.efi.canTouchEfiVariables = true;

  # Enable the X11 windowing system
  services.xserver.enable = true;
  # Enable the GNOME Desktop Environment
  services.xserver.displayManager.gdm.enable = true;
  services.xserver.desktopManager.gnome.enable = true;

  # System-wide packages
  # environment.systemPackages = [ ];
}
