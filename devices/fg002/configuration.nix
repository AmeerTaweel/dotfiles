{
  config,
  inputs,
  params,
  pkgs,
  ...
}: {
  imports = [
    ./hardware.nix

    ./nixos-modules/auto-time-zone.nix
    ./nixos-modules/bluetooth.nix
    ./nixos-modules/networking.nix
    ./nixos-modules/nix.nix
    ./nixos-modules/nix-index.nix
    ./nixos-modules/physlock.nix
    ./nixos-modules/pipewire.nix
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

  networking.hostName = params.hostname;
  # networking.wireless.enable = true; # Enables wireless support via wpa_supplicant.

  # Set your time zone.
  time.timeZone = "Asia/Jerusalem";

  # Select internationalisation properties.
  i18n.defaultLocale = "en_US.UTF-8";

  i18n.extraLocaleSettings = {
    LC_ADDRESS = "en_US.UTF-8";
    LC_IDENTIFICATION = "en_US.UTF-8";
    LC_MEASUREMENT = "en_US.UTF-8";
    LC_MONETARY = "en_US.UTF-8";
    LC_NAME = "en_US.UTF-8";
    LC_NUMERIC = "en_US.UTF-8";
    LC_PAPER = "en_US.UTF-8";
    LC_TELEPHONE = "en_US.UTF-8";
    LC_TIME = "en_US.UTF-8";
  };

  # Configure keymap in X11
  services.xserver = {
    layout = "us";
    xkbVariant = "";
  };

  # Define a user account. Don't forget to set a password with ‘passwd’.
  users.users.${params.username} = {
    isNormalUser = true;
    description = params.name;
    extraGroups = ["networkmanager" "wheel"];
    packages = with pkgs; [];
    shell = pkgs.fish;
  };

  programs.fish.enable = true;

  # Allow unfree packages
  # nixpkgs.config.allowUnfree = true;

  # List packages installed in system profile.
  # To search, run: $ nix search wget
  environment.systemPackages = with pkgs; [];

  # Some programs need SUID wrappers, can be configured further or are started in user sessions. programs.mtr.enable = true; programs.gnupg.agent = {
  #   enable = true; enableSSHSupport = true;
  # };

  # List services that you want to enable:

  # Enable the OpenSSH daemon. services.openssh.enable = true;

  # TODO: Clean
  programs.sway.enable = true;

  # This value determines the NixOS release from which the default settings for stateful data, like file locations and database versions on your system were
  # taken. It‘s perfectly fine and recommended to leave this value at the release version of the first install of this system. Before changing this value read the
  # documentation for this option (e.g. man configuration.nix or on https://nixos.org/nixos/options.html).
  system.stateVersion = params.state-version; # Did you read the comment?
}
