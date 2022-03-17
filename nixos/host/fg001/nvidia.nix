{ host, pkgs, config, ... }:

let
	nvidia-offload = pkgs.writeShellScriptBin "nvidia-offload" ''
		export __NV_PRIME_RENDER_OFFLOAD=1
		export __NV_PRIME_RENDER_OFFLOAD_PROVIDER=NVIDIA-G0
		export __GLX_VENDOR_LIBRARY_NAME=nvidia
		export __VK_LAYER_NV_optimus=NVIDIA_only
		exec -a "$0" "$@"
	'';


	intelBusId = "PCI:0:2:0";
	nvidiaBusId = "PCI:1:0:0";
in {
	environment.systemPackages = with pkgs; [
		nvidia-offload # Wrapper around programs
	];

	services.xserver.videoDrivers = [ "nvidia" ];
	hardware.nvidia.prime = {
		offload.enable = true;
		# sync.enable = true;

		# Bus ID of the Intel GPU
		# Find it using lspci, either under 3D or VGA
		inherit intelBusId;

		# Bus ID of the Nvidia GPU
		# Find it using lspci, either under 3D or VGA
		inherit nvidiaBusId;
	};

	# Useful for when NixOS has issues finding the primary display
	hardware.nvidia.modesetting.enable = true;

	# OpenGL and accelerated video playback
	nixpkgs.config.packageOverrides = pkgs: {
		vaapiIntel = pkgs.vaapiIntel.override { enableHybridCodec = true; };
	};
	hardware.opengl = {
		enable = true;
		driSupport = true;
		extraPackages = with pkgs; [
			intel-media-driver
			vaapiIntel
			# vaapiVdpau
			libvdpau-va-gl
		];
		driSupport32Bit = true;
		extraPackages32 = with pkgs.pkgsi686Linux; [
			intel-media-driver
			vaapiIntel
			# vaapiVdpau
			libvdpau-va-gl
			libva
		];
	};
}
