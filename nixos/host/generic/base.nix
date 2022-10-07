{ host, pkgs, ... }:

{
	# Enable flakes
	nix.package = pkgs.nixUnstable;
	nix.extraOptions = ''
		experimental-features = nix-command flakes
	'';

	time.timeZone = host.timezone;
	# Fix clock issue with Windows dual boot
	time.hardwareClockInLocalTime = true;

	nixpkgs.config.allowUnfree = true;

	# Auto optimise store to save disk space
	nix.settings.auto-optimise-store = true;
	# Auto garbage-collection to save disk space
	nix.gc = {
		automatic = true;
		dates = "weekly";
		options = "--delete-older-than 30d";
	};

	# Disable auto-upgrade
	system.autoUpgrade.enable = false;
	system.autoUpgrade.allowReboot = false;

	services.upower.enable = true;
}
