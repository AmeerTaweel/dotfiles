{
  lib,
  params,
  pkgs,
  ...
}: {
  home.sessionVariables = lib.mkIf (builtins.elem params.editor ["nvim" "neovim"]) {
    EDITOR = "nvim";
    MANPAGER = "nvim +Man!";
  };

  programs.neovim = {
    enable = true;
    plugins = with pkgs.vimPlugins; [
      # Theme:
      # https://github.com/Shatur/neovim-ayu
      neovim-ayu
      # Status Line
      # https://github.com/nvim-lualine/lualine.nvim
      lualine-nvim
    ];
  };
}
