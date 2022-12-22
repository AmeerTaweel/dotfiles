{ ... }:

{
	imports = [
		# Generic Modules
		../../generic/base.nix
		../../generic/networking.nix
		../../generic/bluetooth.nix
		../../generic/sound/pipewire.nix
		../../generic/virtualization.nix
		../../generic/physlock.nix
		# Host-Specific Modules
		./bootloader.nix
		./fonts.nix
		./hardware.nix
		./nvidia.nix
		./xserver.nix
	];
}
