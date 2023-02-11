{
  config,
  lib,
  pkgs,
  ...
}: let
  inherit (lib) mkOption mkIf mkEnableOption mdDoc types;

  prg = config.programs;
  cfg = prg.thefuck;

  bashAndZshInitScript = ''
    eval $(${pkgs.thefuck}/bin/thefuck --alias ${cfg.alias})
  '';
  fishInitScript = ''
    ${pkgs.thefuck}/bin/thefuck --alias ${cfg.alias} | source
  '';
in {
  options = {
    programs.thefuck = {
      enable = mkEnableOption (mdDoc "thefuck");

      alias = mkOption {
        default = "fuck";
        type = types.str;
        description = mdDoc ''
          `thefuck` needs an alias to be configured.
          The default value is `fuck`, but you can use anything else as well.
        '';
      };
    };
  };

  config = mkIf cfg.enable {
    home.packages = [pkgs.thefuck];

    programs.bash.initExtra = bashAndZshInitScript;
    programs.zsh.initExtra = mkIf prg.zsh.enable bashAndZshInitScript;
    programs.fish.interactiveShellInit = mkIf prg.fish.enable fishInitScript;
  };
}
