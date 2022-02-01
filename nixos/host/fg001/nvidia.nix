{ host, pkgs, config, lib, ... }:

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

	nvidia_x11 = config.boot.kernelPackages.nvidia_x11;
	nvidia_gl = nvidia_x11.out;
	nvidia_gl_32 = nvidia_x11.lib32;
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

	# Automatically detect external monitors and change profile
	# Profiles configured per user
	services.autorandr.enable = true;

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
			vaapiVdpau
			libvdpau-va-gl
			nvidia_gl
		];
		driSupport32Bit = true;
		extraPackages32 = with pkgs.pkgsi686Linux; [
			intel-media-driver
			vaapiIntel
			vaapiVdpau
			libvdpau-va-gl
			libva
			nvidia_gl_32
		];
	};

	# Failed tries to enable external monitors

	# Blacklist the open source driver
	# boot.blacklistedKernelModules = [ "nouveau" ];

	# boot.blacklistedKernelModules = [
	# 	"nouveau"
	# 	"rivafb"
	# 	"nvidiafb"
	# 	"rivatv"
	# 	"nv"
	# 	"uvcvideo"
	# ];

	# boot.extraModprobeConfig = ''
	# 	options bbswitch load_state=-1 unload_state=1 nvidia-drm
	# '';
	# boot.extraModulePackages = [ config.boot.kernelPackages.nvidia_x11 ];

	# Fix screen tearing
	# services.xserver.screenSection = ''
	# 	Option "metamodes" "nvidia-auto-select +0+0 {ForceFullCompositionPipeline=On}"
	# 	Option "AllowIndirectGLXProtocol" "off"
	# 	Option "TripleBuffer" "on"
	# '';

	# Configure XDG compliance
	# environment.variables = {
	# 	__GL_SHADER_DISK_CACHE_PATH = "$XDG_CACHE_HOME/nv";
	# 	CUDA_CACHE_PATH = "$XDG_CACHE_HOME/nv";
	# };

	# Configure external displays
	# specialisation = {
	# 	external-display.configuration = {
	# 		system.nixos.tags = [ "external-display" ];
	# 		hardware.nvidia.prime.offload.enable = lib.mkForce false;
	# 		hardware.nvidia.powerManagement.enable = lib.mkForce false;
	# 	};
	# };

	# Fix graphical corruption on suspend
	# hardware.nvidia.powerManagement.enable = true;
}
