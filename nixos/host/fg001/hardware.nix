{ config, lib, pkgs, modulesPath, ... }:

{
	imports = [
		"${modulesPath}/installer/scan/not-detected.nix"
	];

	boot.initrd.availableKernelModules = [ "xhci_pci" "ahci" "usb_storage" "sd_mod" "rtsx_pci_sdmmc" ];
	boot.initrd.kernelModules = [ ];
	boot.kernelModules = [ "kvm-intel" ];
	boot.extraModulePackages = [ ];

	fileSystems."/" = {
		device = "/dev/disk/by-uuid/3b3aa263-7eee-4dad-aa31-ecf9267d449a";
		fsType = "ext4";
	};

	fileSystems."/boot" = {
		device = "/dev/disk/by-uuid/3A10-2CD1";
		fsType = "vfat";
	};

	swapDevices = [
		{ device = "/dev/disk/by-uuid/15af03f1-3a04-46b0-9559-e8f2f6da084e"; }
	];

	powerManagement.cpuFreqGovernor = lib.mkDefault "powersave";
	hardware.cpu.intel.updateMicrocode = lib.mkDefault config.hardware.enableRedistributableFirmware;
}
