{ pkgs, ... }:

{
	virtualisation.libvirtd.enable = true;
	programs.dconf.enable = true;
	environment.systemPackages = [ pkgs.virt-manager ];

	# USB Redirection
	virtualisation.spiceUSBRedirection.enable = true;

	# Docker
	virtualisation.docker.enable = true;
}
