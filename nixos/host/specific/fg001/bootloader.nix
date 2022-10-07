{ ... }:

{
	boot.loader.efi.canTouchEfiVariables = true;
	boot.loader.efi.efiSysMountPoint = "/boot";
	boot.loader.grub.enable = true;
	boot.loader.grub.version = 2;
	boot.loader.grub.device = "nodev";
	boot.loader.grub.useOSProber = true;
	boot.loader.grub.efiSupport = true;
}
