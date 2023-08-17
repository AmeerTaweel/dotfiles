{pkgs, inputs, ...}: {
  sound.enable = true;
  hardware.pulseaudio.enable = false;
  security.rtkit.enable = true;
  services.pipewire = {
    package = inputs.nixpkgs-pipewire.legacyPackages.x86_64-linux.pipewire;
    enable = true;
    audio.enable = true;
    pulse.enable = true;
    jack.enable = true;
    alsa = {
      enable = true;
      support32Bit = true;
    };
  };
}
