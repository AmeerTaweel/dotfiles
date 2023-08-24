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

    ./autocmd.nix
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

      -- taboo.vim config
      vim.g.taboo_tab_format = " [%N] %f%m "
      vim.g.taboo_renamed_tab_format = " [%N] %l%m "

      ${nvimTheme.${config.colorScheme.slug}.config}
    '';
    extraConfigLuaPre = ''
      vim.g.mapleader = ","
    '';
    globals = {
      netrw_banner = 0; # Remove the Netrw banner
    };
    filetype = {
      extension = {
        tmux = "tmux";
      };
    };
    colorschemes.${nvimTheme.${config.colorScheme.slug}.name}.enable = true;
    maps = {
      normal = {
        "<leader>-" = {
          action = "<cmd>split<cr>";
          desc = "split window horizontally";
        };
        "<leader>/" = {
          action = "<cmd>vsplit<cr>";
          desc = "split window vertically";
        };
      };
    };
    plugins = {
      lualine = {
        enable = true;
        globalstatus = true;
        iconsEnabled = true;
      };
      comment-nvim.enable = true;
      gitsigns = {
        enable = true;
        currentLineBlame = true;
      };
      todo-comments = {
        enable = true;
        signs = false;
      };
      nvim-cmp = {
        enable = true;
        completion = {
          autocomplete = [ "TextChanged" ];
          keywordLength = 1;
        };
        sources = [
          { name = "nvim_lsp"; }
          { name = "nvim_lua"; }
          { name = "path"; }
          { name = "buffer"; }
        ];
        mapping = {
          "<tab>" = "cmp.mapping.select_next_item()";
          "<s-tab>" = "cmp.mapping.select_prev_item()";
          "<c-n>" = "cmp.mapping.select_next_item()";
          "<c-p>" = "cmp.mapping.select_prev_item()";
        };
      };
      vim-matchup = {
        enable = true;
        enableSurround = true;
      };

      telescope = {
        enable = true;
        extraOptions = {
          defaults = {
            sorting_strategy = "ascending";
            layout_config.prompt_position = "top";
            # Insert mode mappings (within the prompt)
            mappings.i = {
              "<esc>" = "close";
              "<tab>" = "move_selection_next";
              "<s-tab>" = "move_selection_previous";
            };
          };
        };
        extensions = {
          fzf-native.enable = true;
        };
      };

      which-key.enable = true;

      # -- Asynctasks
      # -- Quickfix list height
      # variables.global.asyncrun_open = 8
      #
      # -- Lightspeed
      # vim.cmd "map <space> <plug>Lightspeed_,_ft"

      lsp = {
        enable = true;
        servers = {
          lua-ls.enable = true;
          pyright.enable = true;
          nil_ls.enable = true;
          html.enable = true;
          cssls.enable = true;
          jsonls.enable = true;
          yamlls.enable = true;
          ccls.enable = true;
        };
      };

      treesitter = {
        enable = true;
        folding = true;
        indent = true;
      };
      treesitter-rainbow.enable = true;
      ts-context-commentstring.enable = true;
      indent-blankline = {
        enable = true;
        buftypeExclude = ["terminal" "nofile" "quickfix" "prompt" "help"];
        useTreesitter = true;
        useTreesitterScope = false;
        showCurrentContext = true;
      };
      nvim-autopairs = {
        enable = true;
        checkTs = true;
        disableInMacro = true;
        disableInVisualblock = true;
      };
      ts-autotag.enable = true;
    };
    extraPlugins = with pkgs.vimPlugins; [
      # Navigation
      vim-kitty-navigator

      # Others
      vim-sleuth

      # Better tab names
      taboo-vim
    ];
  };

  programs.neovim = {
    enable = false;
    # plugins = with pkgs.vimPlugins; [
    #   plenary-nvim # Required by many packages

    #   # Version Control
    #   neogit

    #   # LSP
    #   nvim-lspconfig
    #   nvim-cmp
    #   cmp-nvim-lsp
    #   cmp-path
    #   cmp-cmdline
    #   cmp-buffer
    #   cmp-nvim-lua
    #   cmp-nvim-ultisnips

    #   # Other
    #   vim-eunuch
    #   vim-surround
    #   vim-repeat
    #   vim-highlightedyank
    #   which-key-nvim
    #   nvim-web-devicons
    #   tabular
    #   ultisnips
    #   lightspeed-nvim
    #   winshift-nvim
    #   asyncrun-vim
    #   asynctasks-vim
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
    ];
  };

  # home.packages = with pkgs; [
  #   # Tool for controlling Neovim processes from a terminal
  #   neovim-remote
  # ];
}
