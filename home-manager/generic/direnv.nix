{...}: {
  programs.direnv = {
    enable = true;
    enableBashIntegration = true;
    # Enabled by default
    # enableFishIntegration = true;
    enableZshIntegration = true;
    nix-direnv = {
      enable = true;
    };
  };
}
