{ host, pkgs, ... }:

{
	hardware.bluetooth.enable = true;
	hardware.bluetooth.package = pkgs.bluezFull;

	services.blueman.enable = true;
}
