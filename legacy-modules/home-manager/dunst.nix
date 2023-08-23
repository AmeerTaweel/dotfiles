{
  config,
  pkgs,
  ...
}: {
  services.dunst = {
    enable = true;
    settings = {
      global = {
        # TODO: Fix magic numbers.
        # TODO: Fix font.
        font = "Hack Nerd Font";
        min_icon_size = 128;
        max_icon_size = 128;
        width = 400;
        gap_size = 5;
        transparency = 15;
        frame_color = "#${config.colorScheme.colors.base05}";
        separator_color = "#${config.colorScheme.colors.base05}";
      };

      urgency_low = {
        background = "#${config.colorScheme.colors.base0D}";
        foreground = "#${config.colorScheme.colors.base02}";
      };

      urgency_normal = {
        background = "#${config.colorScheme.colors.base00}";
        foreground = "#${config.colorScheme.colors.base05}";
      };

      urgency_critical = {
        background = "#${config.colorScheme.colors.base08}";
        foreground = "#${config.colorScheme.colors.base02}";
      };
    };
  };
}
