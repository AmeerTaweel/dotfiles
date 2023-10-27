{...}: {
  imports = [
    ./gnome.nix
    ./hardware.nix
    ./headless.nix
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

  # System-wide packages
  # environment.systemPackages = [ ];
}
