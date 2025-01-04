{
  config,
  inputs,
  lib,
  ...
}: {
  nix = {
    # This will add each flake input as a registry
    # To make nix3 commands consistent with your flake
    registry = lib.mapAttrs (_: value: {flake = value;}) inputs;

    # This will additionally add your inputs to the system's legacy channels
    # Making legacy nix commands consistent as well, awesome!
    nixPath = lib.mapAttrsToList (key: value: "${key}=${value.to.path}") config.nix.registry;

    settings = {
      # Enable experimental features (flakes, new `nix` commands, and content-addressed nix)
      experimental-features = "nix-command flakes ca-derivations auto-allocate-uids";

      # Deduplicate and optimize nix store
      auto-optimise-store = true;

      # Automatically pick UIDs for builds, rather than creating nixbld* user accounts
      auto-allocate-uids = true;
    };

    # Auto garbage-collection to save disk space
    gc = {
      automatic = true;
      dates = "weekly";
      options = "--delete-older-than 30d";
    };
  };
}
