{config, ...}: let
  nvimTheme = {
    ayu-dark = {
      name = "ayu";
      config = ''
        vim.go.background = "dark"
        require("ayu").colorscheme()
        require("lualine").setup({
          options = { theme = "ayu_dark" }
        })
      '';
    };
    ayu-mirage = {
      name = "ayu";
      config = ''
        vim.go.background = "dark"
        require("ayu").setup({ mirage = true })
        require("ayu").colorscheme()
        require("lualine").setup({
          options = { theme = "ayu_mirage" }
        })
      '';
    };
    ayu-light = {
      name = "ayu";
      config = ''
        vim.go.background = "light"
        require("ayu").colorscheme()
        require("lualine").setup({
          options = { theme = "ayu_light" }
        })
      '';
    };
  };
in {
  programs.nixvim = {
    colorschemes.${nvimTheme.${config.colorScheme.slug}.name}.enable = true;
    extraConfigLua = ''
      ${nvimTheme.${config.colorScheme.slug}.config}
    '';
  };
}
