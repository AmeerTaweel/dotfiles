{
  config,
  inputs,
  lib,
  params,
  pkgs,
  ...
}: let
  nvimConfigurationPath = ./config/nvim;

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
  imports = [
    inputs.nix-colors.homeManagerModules.default
    inputs.nixvim.homeManagerModules.nixvim
  ];

  home.sessionVariables = lib.mkIf (builtins.elem params.editor ["nvim" "neovim"]) {
    EDITOR = "nvim";
    MANPAGER = "nvim +Man!";
  };

  programs.nixvim = {
    enable = true;
    options = {
      # Hybrid Line Numbers
      relativenumber = true; # Relative line numbers
      number = true; # Show current line number

      # Search
      incsearch = true; # Select items found in search
      hlsearch = true; # Highlight searches by default
      ignorecase = true; # Ignore case when searching
      smartcase = true; # Unless you type a capital

      # Indentation
      breakindent = true; # Keep indentation for wrapped lines
      autoindent = true; # Enable auto-indentation
      smartindent = true; # Enable smart-indentation
      expandtab = false; # Use tabs not spaces
      shiftwidth = 4; # Set tab width
      tabstop = 4;
      softtabstop = 0; # Use hard tabs always

      # Temporary Files
      undofile = true; # Enable persistent undo
      backup = true; # Enable backup files
      swapfile = true; # Enable swap files

      # Other
      textwidth = 80; # Set chars-per-line-limit
      colorcolumn = "81"; # Show column indicating the chars-per-line limit
      cursorline = true;
      cursorcolumn = true;
      termguicolors = true; # Enable true color
      wrap = false; # Do not wrap lines
      laststatus = 2; # Always show status line for all windows
      confirm = true; # Confirm operations that would fail normally (like closing an unsaved buffer)
      backspace = "indent,eol,start"; # Allow backspacing over indention, line breaks and insertion start
      autoread = true; # Automatically re-read files if modified outside the editor
      hidden = true; # Allow leaving buffers without saving, leaving buffer in background
      mouse = "a"; # Enable mouse support for scrolling and resizing
      title = true; # Set the window’s title, reflecting the file currently being edited
      wildmenu = true; # Enable expanding commands with the tab key
      splitright = true;
      splitbelow = true;
      history = 10000; # Set the number of saved commands in history
      showcmd = true; # Show incomplete commands at the bottom
      modeline = false; # Ignore file’s mode lines
      signcolumn = "yes"; # Always show sign column
      guicursor = "i:block"; # Use a block cursor in insert mode
      completeopt = "menuone,noinsert,noselect,preview"; # Auto-completion options
      cmdheight = 1; # Give more space for displaying messages.
      updatetime = 100; # Long updatetime leads to noticeable delays
      timeoutlen = 500; # Time to wait for a keybinding to complete
      scrolloff = 10; # Number of vertical context lines
      sidescrolloff = 5; # Number of horizontal context lines
      list = true;
      listchars = {
        eol = "↲";
        trail = "·";
        space = ".";
        tab = "-->";
      };
      viewoptions = ["folds" "cursor"];
    };
    extraConfigLua = ''
      -- Don't clutter working directory with backup files
      vim.opt.backupdir:remove { "." }
      -- Use diff mode vertical split
      vim.opt.diffopt:append { "vertical" }
      -- Don't pass messages to |insertion-completion-menu|
      vim.opt.shortmess:append "c"

      ${nvimTheme.${config.colorScheme.slug}.config}
    '';
    globals = {
      netrw_banner = 0; # Remove the Netrw banner
    };
    filetype = {
      extension = {
        tmux = "tmux";
      };
    };
    autoGroups = {
      AUTO_REBALANCE.clear = true;
      DISABLE_AUTO_COMMENT.clear = true;
      SMART_LINE_NUMBERS.clear = true;
      DISABLE_NETRW_LIST.clear = true;
      DISABLE_AUTO_FOLD.clear = true;
    };
    autoCmd = [
      {
        # Automatically re-balance windows on resize
        group = "AUTO_REBALANCE";
        event = ["VimResized"];
        pattern = ["*"];
        command = "wincmd =";
      }
      {
        # Disable auto-commenting
        group = "DISABLE_AUTO_COMMENT";
        event = ["FileType"];
        pattern = "*";
        command = "setlocal formatoptions-=c formatoptions-=r formatoptions-=o";
      }
      {
        # Make line numbers absolute when in insert mode and on buffer leaving
        group = "SMART_LINE_NUMBERS";
        event = ["BufLeave" "FocusLost" "InsertEnter"];
        pattern = "*";
        command = "setlocal norelativenumber";
      }
      {
        # Restore relative line numbers when leaving insert move or entering buffer
        group = "SMART_LINE_NUMBERS";
        event = ["BufEnter" "FocusGained" "InsertLeave"];
        pattern = "*";
        command = "setlocal relativenumber";
      }
      {
        # Disable annoying folds when opening a file
        group = "DISABLE_AUTO_FOLD";
        event = ["BufWinEnter"];
        pattern = "*";
        command = "normal zR";
      }
      {
        # Disable whitespace character hints in netrw
        group = "DISABLE_NETRW_LIST";
        event = ["FileType"];
        pattern = "netrw";
        command = "setlocal nolist";
      }
    ];
    colorschemes.${nvimTheme.${config.colorScheme.slug}.name}.enable = true;
    plugins = {
      lsp = {
        enable = true;
        servers = {
          lua-ls = {
            enable = true;
          };
          pyright = {
            enable = true;
          };
        };
      };
      comment-nvim.enable = true;
      lualine = {
        enable = true;
        globalstatus = true;
        iconsEnabled = true;
      };
      treesitter = {
        enable = true;
        folding = true;
        indent = true;
      };
      treesitter-rainbow.enable = true;
    };
    extraPlugins = with pkgs.vimPlugins; [
      # Navigation
      vim-kitty-navigator

      # Others
      vim-sleuth
    ];
  };

  programs.neovim = {
    enable = false;
    # plugins = with pkgs.vimPlugins; [
    #   plenary-nvim # Required by many packages

    #   # Version Control
    #   neogit
    #   gitsigns-nvim

    #   # TreeSitter
    #   nvim-ts-context-commentstring
    #   nvim-ts-autotag
    #   nvim-autopairs
    #   indent-blankline-nvim

    #   # LSP
    #   nvim-lspconfig
    #   nvim-cmp
    #   cmp-nvim-lsp
    #   cmp-path
    #   cmp-cmdline
    #   cmp-buffer
    #   cmp-nvim-lua
    #   cmp-nvim-ultisnips
    #   fidget-nvim

    #   # Telescope
    #   telescope-nvim
    #   telescope-fzf-native-nvim
    #   telescope-ultisnips-nvim
    #   telescope-asynctasks-nvim

    #   # Other
    #   vim-eunuch
    #   vim-surround
    #   vim-repeat
    #   vim-highlightedyank
    #   which-key-nvim
    #   vim-matchup
    #   nvim-web-devicons
    #   tabular
    #   ultisnips
    #   lightspeed-nvim
    #   winshift-nvim
    #   asyncrun-vim
    #   asynctasks-vim
    #   todo-nvim
    #   taboo-vim
    #   # NOTE: Cheatsheet for targets-vim in the link below
    #   # https://github.com/wellle/targets.vim/blob/master/cheatsheet.md
    #   targets-vim
    #   vimtex
    # ];
    extraPackages = with pkgs; [
      #   # Clipboard Support
      #   xclip

      #   # Telescope
      #   ripgrep
      #   fd

      #   # Language Servers
      #   rnix-lsp
      #   pyright
      #   yaml-language-server
      #   ccls
      #   rust-analyzer
      #   nodePackages.bash-language-server
      #   nodePackages.vim-language-server
      #   nodePackages.vscode-langservers-extracted
      #   nodePackages.typescript-language-server
      #   nodePackages.diagnostic-languageserver
      #   lua-language-server
      #   texlab

      #   # Linters
      #   shellcheck
      #   vim-vint
      #   nodePackages.markdownlint-cli2
    ];
    # extraConfig = ''
    #   luafile ~/.config/nvim/lua/settings.lua
    #   luafile ~/.config/nvim/lua/plugins/init.lua
    #   luafile ~/.config/nvim/lua/keybindings.lua
    #   luafile ~/.config/nvim/lua/theme.lua
    #   luafile ~/.config/nvim/lua/autocmd.lua

    #   if exists("g:neovide")
    #   	luafile ~/.config/nvim/lua/neovide.lua
    #   endif
    # '';
  };

  # home.packages = with pkgs; [
  #   # Neovim GUI
  #   neovide
  #   # Tool for controlling Neovim processes from a terminal
  #   neovim-remote
  # ];

  # xdg.configFile.nvimConfiguration = {
  #   source = "${nvimConfigurationPath}/lua";
  #   target = "nvim/lua";
  #   recursive = true;
  # };

  # xdg.configFile.nvimTheme = {
  #   text = nvimThemeConfiguration.${config.colorScheme.slug};
  #   target = "nvim/lua/theme.lua";
  # };

  # xdg.configFile.nvimUltiSnips = {
  #   source = "${nvimConfigurationPath}/ulti-snips";
  #   target = "nvim/UltiSnips";
  #   recursive = true;
  # };

  # xdg.configFile.nvimAsyncTasks = {
  #   source = "${nvimConfigurationPath}/tasks.ini";
  #   target = "nvim/tasks.ini";
  # };
}
