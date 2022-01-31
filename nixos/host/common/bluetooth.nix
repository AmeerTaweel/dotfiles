{ host, pkgs, ... }:

let
	bluetoothPackage = pkgs.bluezFull;
in {
	hardware.bluetooth.enable = true;
	hardware.bluetooth.package = bluetoothPackage;

	# Support high-quality Bluetooth sound
	hardware.bluetooth.hsphfpd.enable = true;

	services.blueman.enable = true;

	systemd.user.services.mpris-proxy = {
		description = "Control media player using Bluetooth headset buttons.";
		after = [ "network.target" "sound.target" ];
		script = "${bluetoothPackage}/bin/mpris-proxy";
		wantedBy = [ "default.target" ];
	};
}
