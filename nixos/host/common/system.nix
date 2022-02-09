{ host, pkgs, ... }:

{
	# Use the systemd-boot EFI boot loader.
	# boot.loader.systemd-boot.enable = true;
	# boot.loader.efi.canTouchEfiVariables = true;
	# boot.loader.grub.enable = true;
	boot.loader.efi.canTouchEfiVariables = true;
	boot.loader.grub.enable = true;
	boot.loader.grub.version = 2;
	boot.loader.grub.device = "nodev";
	boot.loader.grub.useOSProber = true;
	boot.loader.grub.efiSupport = true;
	boot.loader.efi.efiSysMountPoint = "/boot";
	time.hardwareClockInLocalTime = true;

	time.timeZone = host.timezone;

	nixpkgs.config.allowUnfree = true;

	# Enable flakes systemwide
	nix.package = pkgs.nixUnstable;
	nix.extraOptions = ''
		experimental-features = nix-command flakes
	'';

	# Auto optimise store to save disk space
	nix.settings.auto-optimise-store = true;

	# Auto garbage-collection to save disk space
	nix.gc = {
		automatic = true;
		dates = "weekly";
		options = "--delete-older-than 30d";
	};

	system.autoUpgrade.enable = true;
	system.autoUpgrade.allowReboot = false;

	# Essential system packages
	environment.systemPackages = with pkgs; [
		file
		tree
		curl
		wget
		wget2
	];

	# System fonts
	fonts.fonts = with pkgs; [
		(nerdfonts.override { fonts = [ "Hack" ]; })
	];

	services.upower.enable = true;
}
