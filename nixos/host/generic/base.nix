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

	# DBus service for applications to query and manipulate storage devices.
	services.udisks2.enable = true;

	# This value determines the NixOS release from which the default
	# settings for stateful data, like file locations and database versions
	# on your system were taken. It‘s perfectly fine and recommended to leave
	# this value at the release version of the first install of this system.
	# Before changing this value read the documentation for this option
	# (e.g. man configuration.nix or on https://nixos.org/nixos/options.html)
	system.stateVersion = host.stateVersion;
}
