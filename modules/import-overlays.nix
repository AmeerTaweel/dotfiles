{...}: let
  overlays = import ../overlays;
in {
  nixpkgs.overlays = [
    overlays.modifications
    overlays.additions
  ];
}
