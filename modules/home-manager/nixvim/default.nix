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

    ./settings.nix
    ./autocmd.nix
  ];

  home.sessionVariables = lib.mkIf (builtins.elem params.editor ["nvim" "neovim"]) {
    EDITOR = "nvim";
    MANPAGER = "nvim +Man!";
  };

  programs.nixvim = {
    enable = true;
    extraConfigLua = ''
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
