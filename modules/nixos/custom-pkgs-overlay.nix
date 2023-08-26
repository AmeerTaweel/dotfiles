{...}: {
  nixpkgs.overlays = [
    (final: prev: import ../../pkgs {pkgs = final;})
  ];
}
