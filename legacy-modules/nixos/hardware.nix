{
  pkgs,
  config,
  lib,
  modulesPath,
  ...
}: {
  imports = ["${modulesPath}/installer/scan/not-detected.nix"];

  boot.initrd.availableKernelModules = ["xhci_pci" "ahci" "usb_storage" "sd_mod" "rtsx_pci_sdmmc"];
  boot.initrd.kernelModules = [];
  boot.kernelModules = ["kvm-intel"];
  boot.extraModulePackages = [];

  networking.useDHCP = false;
  # Per-interface useDHCP
  networking.interfaces.eno1.useDHCP = true;
  networking.interfaces.wlo1.useDHCP = true;

  fileSystems."/" = {
    device = "/dev/disk/by-label/NIXOS_MAIN";
    fsType = "ext4";
  };

  fileSystems."/boot" = {
    device = "/dev/disk/by-label/BOOT";
    fsType = "vfat";
  };

  fileSystems."/extra" = {
    device = "/dev/disk/by-label/NIXOS_EXTRA";
    fsType = "ext4";
  };

  fileSystems."/shared" = {
    device = "/dev/disk/by-label/SHARED";
    fsType = "ntfs";
  };

  swapDevices = [
    {
      device = "/dev/disk/by-label/NIXOS_SWAP";
    }
  ];

  powerManagement.cpuFreqGovernor = lib.mkDefault "powersave";
  hardware.cpu.intel.updateMicrocode = lib.mkDefault config.hardware.enableRedistributableFirmware;
}
