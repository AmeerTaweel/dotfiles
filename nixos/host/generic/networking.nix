{ host, ... }:

{
	networking.hostName = host.hostName;

	# Enables wireless support via wpa_supplicant.
	# networking.wireless.enable = true;

	# The global useDHCP flag is deprecated, so it's explicitly set to false.
	networking.useDHCP = false;
	# Per-interface useDHCP will be mandatory in the future
	# This config replicates the default behaviour.
	networking.interfaces = with builtins; listToAttrs (map (nic: {
		name = nic; value = { useDHCP = true; };
	}) host.nics);

	networking.networkmanager.enable = true;

	# GUI system-tray applet
	# Using the home-manager option instead
	# programs.nm-applet.enable = true;

	# Configure network proxy if necessary
	# networking.proxy.default = "http://user:password@proxy:port/";
	# networking.proxy.noProxy = "127.0.0.1,localhost,internal.domain";
	
	networking.firewall.enable = true;
	# Open ports in the firewall.
	# networking.firewall.allowedTCPPorts = [ ... ];
	# networking.firewall.allowedUDPPorts = [ ... ];
}
