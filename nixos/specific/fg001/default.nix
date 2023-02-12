# This is your system's configuration file.
# Use this to configure your system environment (it replaces /etc/nixos/configuration.nix)
{
  inputs,
  outputs,
  lib,
  config,
  pkgs,
  ...
}: {
  # You can import other NixOS modules here
  imports = [
    # If you want to use modules your own flake exports (from modules/nixos):
    # outputs.nixosModules.example

    # Or modules from other flakes (such as nixos-hardware):
    # inputs.hardware.nixosModules.common-cpu-amd
    # inputs.hardware.nixosModules.common-ssd

    # You can also split up your configuration and import pieces of it here:
    # Generic Config
    ../../generic/nix.nix
    ../../generic/nixpkgs.nix
    ../../generic/bluetooth.nix
    ../../generic/networking.nix
    ../../generic/pipewire.nix
    ../../generic/screenlock.nix
    ../../generic/virtualization.nix
    # Specific Config
    ./bootloader.nix
    ./hardware.nix
    ./nvidia.nix
    ./xserver.nix
  ];

  # You can add overlays here
  nixpkgs.overlays = [
    # Add overlays your own flake exports (from overlays and pkgs dir):
    outputs.overlays.modifications
    outputs.overlays.additions

    # You can also add overlays exported from other flakes:
    # neovim-nightly-overlay.overlays.default

    # Or define it inline, for example:
    # (final: prev: {
    #   hi = final.hello.overrideAttrs (oldAttrs: {
    #     patches = [ ./change-hello-to-hi.patch ];
    #   });
    # })
  ];

  # Host Name
  networking.hostName = "fg001";

  # Fix clock issue with Windows dual boot
  time.hardwareClockInLocalTime = true;

  # Services
  services = {
    # Keeps the system timezone up-to-date based on the current location
    automatic-timezoned = {
      enable = true;
    };

    # Automatic CPU speed and power optimizer for Linux
    auto-cpufreq = {
      enable = true;
    };

    # Temperature management daemon
    thermald = {
      enable = true;
    };

    # Auto Nice Daemon
    ananicy = {
      enable = true;
    };

    # Provide Power Management Support
    # TODO: Fine-tune percentage levels
    upower = {
      enable = true;
      usePercentageForPolicy = true;
      percentageLow = 20;
      percentageCritical = 10;
      percentageAction = 5;
      criticalPowerAction = "Hibernate";
    };

    # TODO: Check the following services:
    # - gvfs
  };

  # TODO: Check these options
  # - programs.adb.enable
  # - programs.gamemode.enable
  #   https://github.com/FeralInteractive/GameMode
  # - programs.steam.enable
  # - programs.system-config-printer.enable

  users.users = {
    labmem001 = {
      createHome = true;
      description = "Ameer Taweel";
      extraGroups = [
        # Enable "sudo" for the user
        "wheel"
        "networkmanager"
        "video"
        "libvirtd"
        "docker"
      ];
      # You can set an initial password for your user.
      # If you do, you can skip setting a root password by passing `--no-root-passwd` to nixos-install.
      # Be sure to change it (using passwd) after rebooting!
      initialPassword = "";
      isNormalUser = true;
      shell = pkgs.fish;
    };
  };

  # https://nixos.wiki/wiki/FAQ/When_do_I_update_stateVersion
  system.stateVersion = "21.11";
}
