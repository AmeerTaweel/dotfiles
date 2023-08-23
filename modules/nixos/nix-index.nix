{inputs, ...}: {
  imports = [
    inputs.nix-index-database.nixosModules.nix-index
  ];

  # Enable `nix-index` and its shell integrations
  programs.nix-index = {
    enable = true;
    enableBashIntegration = true;
    enableFishIntegration = true;
    enableZshIntegration = true;
  };

  # Disable `command-not-found` as it serves the same functionality in a
  # different way
  programs.command-not-found.enable = false;
}
