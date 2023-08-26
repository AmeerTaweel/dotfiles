{
  config,
  lib,
  pkgs,
  ...
}: let
  bashAndZshInitScript = ''
    eval $(${pkgs.thefuck}/bin/thefuck --alias fuck)
  '';
  fishInitScript = ''
    ${pkgs.thefuck}/bin/thefuck --alias fuck | source
  '';
  inherit (lib) mkIf;
  prg = {inherit (config.programs) bash fish zsh;};
in {
  home.packages = [pkgs.thefuck];

  programs.bash.initExtra = mkIf prg.bash.enable bashAndZshInitScript;
  programs.fish.interactiveShellInit = mkIf prg.fish.enable fishInitScript;
  programs.zsh.initExtra = mkIf prg.zsh.enable bashAndZshInitScript;
}
