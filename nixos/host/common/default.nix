{ ... }:

{
	imports = [
		./system.nix
		./networking.nix
		./bluetooth.nix
		./audio.nix
	];
	services.xserver.enable = true;
	services.xserver.displayManager.gdm.enable = true;
	services.xserver.desktopManager.gnome.enable = false;
	# Enable touchpad support (enabled default in most desktopManager).
	services.xserver.libinput.enable = true;
	hardware.nvidia.modesetting.enable = true;
}
