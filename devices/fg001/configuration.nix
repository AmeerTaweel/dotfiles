{...}: {
  imports = [
    ./gnome.nix
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

  # Disable the X11 windowing system
  services.xserver.enable = false;
  # Disable the GNOME Desktop Environment
  services.xserver.displayManager.gdm.enable = false;
  services.xserver.desktopManager.gnome.enable = false;

  # Enable Qtile window-manager
  services.xserver.windowManager.qtile = {
    enable = true;
    backend = "wayland";
  };
  programs.fish.shellAbbrs = {
    startw = "qtile start -b wayland";
  };

  # System-wide packages
  # environment.systemPackages = [ ];
}
