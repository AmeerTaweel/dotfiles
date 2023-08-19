{
  config,
  pkgs,
  ...
}: let
  nvimConfigurationPath = ../../config/nvim;

  nvimThemeConfiguration = {
    ayu-dark = ''
      vim.go.background = "dark"
      require("ayu").colorscheme()
      require("lualine").setup {
      	options = { theme  = "ayu_dark" }
      }
    '';
    ayu-mirage = ''
      vim.go.background = "dark"
      require("ayu").setup({ mirage = true })
      require("ayu").colorscheme()
      require("lualine").setup {
      	options = { theme  = "ayu_mirage" }
      }
    '';
    ayu-light = ''
      vim.go.background = "light"
      require("ayu").colorscheme()
      require("lualine").setup {
      	options = { theme  = "ayu_light" }
      }
    '';
  };
in {
  programs.neovim = {
    enable = true;
    plugins = with pkgs.vimPlugins; [
      plenary-nvim # Required by many packages

      # Themes
      neovim-ayu
      nord-nvim
      dracula-vim
      gruvbox-nvim
      onehalf

      # Version Control
      neogit
      gitsigns-nvim

      # TreeSitter
      # (nvim-treesitter.withPlugins (_: pkgs.tree-sitter.allGrammars))

      nvim-ts-rainbow
      nvim-ts-context-commentstring
      nvim-ts-autotag
      nvim-autopairs
      indent-blankline-nvim
      playground

      # LSP
      nvim-lspconfig
      nvim-cmp
      cmp-nvim-lsp
      cmp-path
      cmp-cmdline
      cmp-buffer
      cmp-nvim-lua
      cmp-nvim-ultisnips
      fidget-nvim

      # Navigation
      vim-kitty-navigator

      # Telescope
      telescope-nvim
      telescope-fzf-native-nvim
      telescope-ultisnips-nvim
      telescope-asynctasks-nvim

      # Other
      comment-nvim
      vim-eunuch
      vim-surround
      vim-repeat
      vim-highlightedyank
      which-key-nvim
      vim-matchup
      lualine-nvim
      nvim-web-devicons
      tabular
      ultisnips
      lightspeed-nvim
      winshift-nvim
      asyncrun-vim
      asynctasks-vim
      todo-nvim
      taboo-vim
      # NOTE: Cheatsheet for targets-vim in the link below
      # https://github.com/wellle/targets.vim/blob/master/cheatsheet.md
      targets-vim
      vim-sleuth
      vimtex
    ];
    extraPackages = with pkgs; [
      # Clipboard Support
      xclip

      # Telescope
      ripgrep
      fd

      # Language Servers
      rnix-lsp
      pyright
      yaml-language-server
      ccls
      rust-analyzer
      nodePackages.bash-language-server
      nodePackages.vim-language-server
      nodePackages.vscode-langservers-extracted
      nodePackages.typescript-language-server
      nodePackages.diagnostic-languageserver
      lua-language-server
      texlab

      # Linters
      shellcheck
      vim-vint
      nodePackages.markdownlint-cli2
    ];
    extraConfig = ''
      luafile ~/.config/nvim/lua/settings.lua
      luafile ~/.config/nvim/lua/plugins/init.lua
      luafile ~/.config/nvim/lua/keybindings.lua
      luafile ~/.config/nvim/lua/theme.lua
      luafile ~/.config/nvim/lua/autocmd.lua

      if exists("g:neovide")
      	luafile ~/.config/nvim/lua/neovide.lua
      endif
    '';
  };

  home.packages = with pkgs; [
    # Neovim GUI
    neovide
    # Tool for controlling Neovim processes from a terminal
    neovim-remote
  ];

  xdg.configFile.nvimConfiguration = {
    source = "${nvimConfigurationPath}/lua";
    target = "nvim/lua";
    recursive = true;
  };

  xdg.configFile.nvimTheme = {
    text = nvimThemeConfiguration.${config.colorScheme.slug};
    target = "nvim/lua/theme.lua";
  };

  xdg.configFile.nvimUltiSnips = {
    source = "${nvimConfigurationPath}/ulti-snips";
    target = "nvim/UltiSnips";
    recursive = true;
  };

  xdg.configFile.nvimAsyncTasks = {
    source = "${nvimConfigurationPath}/tasks.ini";
    target = "nvim/tasks.ini";
  };
}
