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
      # Tree-Sitter Parsers
      # https://github.com/nvim-treesitter/nvim-treesitter
      (nvim-treesitter.withPlugins (p: [
        p.lua p.nix p.c p.cpp p.java p.rust
	p.html p.css p.scss p.javascript p.typescript p.json p.svelte p.vue
	p.markdown
	p.python
	p.sql
      ]))
      # Default LSP Configurations
      # https://github.com/neovim/nvim-lspconfig
      nvim-lspconfig
      # Configures Lua Language Server for editing Neovim configs
      # https://github.com/folke/lazydev.nvim
      lazydev-nvim

      # Fuzzy Finder
      # https://github.com/nvim-telescope/telescope.nvim
      telescope-nvim
      # Telescope dependency
      # https://github.com/nvim-lua/plenary.nvim/
      plenary-nvim
      # https://github.com/nvim-telescope/telescope-fzf-native.nvim
      telescope-fzf-native-nvim

      # File Explorer
      # https://github.com/stevearc/oil.nvim
      oil-nvim
      # Icon Provider
      # https://github.com/echasnovski/mini.icons
      mini-icons

      # Auto-Complete
      # https://github.com/Saghen/blink.cmp
      blink-cmp

      # Unix shell commands in Vim
      vim-eunuch

      # Heuristically set buffer options
      vim-sleuth
    ];
    extraPackages =  with pkgs; [
      # Wayland Clipboard Provider
      wl-clipboard

      # For Telescope
      fzf
      ripgrep

      # Language Servers
      lua-language-server
      nil
      ccls
      vscode-langservers-extracted
      superhtml
      typescript-language-server
      svelte-language-server
      basedpyright
    ];
  };
}
