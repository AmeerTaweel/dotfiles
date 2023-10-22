{...}: {
  imports = [
    ./hardware.nix

    ./modules/nixos/bluetooth.nix
    ./modules/nixos/core.nix
    ./modules/nixos/kumech.nix
    ./modules/nixos/networking.nix
    ./modules/nixos/nix.nix
    ./modules/nixos/nix-index.nix
    ./modules/nixos/physlock.nix
    ./modules/nixos/pipewire.nix
  ];

  # Bootloader.
  boot.loader.grub.enable = true;
  boot.loader.grub.device = "/dev/sda";
  # boot.loader.grub.useOSProber = true;

  # Setup keyfile
  boot.initrd.secrets = {
    "/crypto_keyfile.bin" = null;
  };

  # Enable grub cryptodisk
  boot.loader.grub.enableCryptodisk = true;

  boot.initrd.luks.devices."luks-b719494e-dae7-437a-a8fb-7e952da98495".keyFile = "/crypto_keyfile.bin";
  # Enable swap on luks
  boot.initrd.luks.devices."luks-2d4dbad5-681d-4599-9286-60cb7cd6a7fb".device = "/dev/disk/by-uuid/2d4dbad5-681d-4599-9286-60cb7cd6a7fb";
  boot.initrd.luks.devices."luks-2d4dbad5-681d-4599-9286-60cb7cd6a7fb".keyFile = "/crypto_keyfile.bin";

  # Enable Sway window-manager
  programs.sway.enable = true;

  # System-wide packages
  # environment.systemPackages = [ ];
}
