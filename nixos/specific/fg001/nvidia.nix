{pkgs, ...}: let
  intelBusId = "PCI:0:2:0";
  nvidiaBusId = "PCI:1:0:0";
in {
  services.xserver.videoDrivers = ["nvidia"];
  hardware.nvidia.prime = {
    offload.enable = true;

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
    vaapiIntel = pkgs.vaapiIntel.override {enableHybridCodec = true;};
  };
  hardware.opengl = {
    enable = true;
    driSupport = true;
    extraPackages = with pkgs; [
      intel-media-driver
      vaapiIntel
      vaapiVdpau
      libvdpau-va-gl
    ];
    driSupport32Bit = true;
    extraPackages32 = with pkgs.pkgsi686Linux; [
      intel-media-driver
      vaapiIntel
      vaapiVdpau
      libvdpau-va-gl
      libva
    ];
  };

  environment.systemPackages = with pkgs; [
    nvidia-offload
  ];
}
