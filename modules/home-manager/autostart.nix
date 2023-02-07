{
  config,
  lib,
  pkgs,
  ...
}: let
  cfg = config.autostart;
  inherit (lib) mkOption mdDoc types;
in {
  options = {
    autostart = mkOption {
      default = {};

      description = mdDoc ''
        Run programs / commands on boot.
      '';

      type = types.attrsOf (types.submodule ({...}: {
        options = {
          description = mkOption {
            description = mdDoc ''
              Description for the autostart service.
            '';
            type = types.str;
          };

          execPre = mkOption {
            default = "";
            description = mdDoc ''
              Command to execute before the main command.
              Can be used for delays.
            '';
            type = types.str;
          };

          exec = mkOption {
            description = mdDoc ''
              Command to execute on boot.
            '';
            type = types.str;
          };
        };
      }));
    };
  };

  config = {
    systemd.user.services = lib.attrsets.mapAttrs' (name: value:
      lib.attrsets.nameValuePair
      "autostart-${name}"
      {
        Install = {
          WantedBy = ["graphical-session.target"];
        };

        Service = {
          ExecStartPre = value.execPre;
          ExecStart = value.exec;
        };

        Unit = {
          After = ["graphical-session-pre.target" "tray.target"];
          Description = value.description;
          PartOf = ["graphical-session.target"];
          Requires = ["tray.target"];
        };
      })
    cfg;
  };
}
