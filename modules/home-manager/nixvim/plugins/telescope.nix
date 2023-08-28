{...}: {
  programs.nixvim = {
    plugins = {
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
    };
    maps.normal = {
      "<leader>fb" = {
        action = "<cmd>Telescope buffers<cr>";
        desc = "find buffer";
      };
      "<leader>fc" = {
        action = "<cmd>Telescope command_history<cr>";
        desc = "search command history";
      };
      "<leader>ff" = {
        action = "<cmd>Telescope find_files<cr>";
        desc = "find file";
      };
      "<leader>fh" = {
        action = "<cmd>Telescope help_tags<cr>";
        desc = "find help";
      };
      "<leader>fm" = {
        action = "<cmd>Telescope marks<cr>";
        desc = "find mark";
      };
      "<leader>fr" = {
        action = "<cmd>Telescope registers<cr>";
        desc = "find register";
      };
      "<leader>fs" = {
        action = "<cmd>Telescope search_history<cr>";
        desc = "search search history";
      };
      "<leader>ft" = {
        action = "<cmd>TodoTelescope<cr>";
        desc = "find todo";
      };
      "<leader>flb" = {
        action = "<cmd>Telescope current_buffer_fuzzy_find<cr>";
        desc = "find line in buffer";
      };
      "<leader>fld" = {
        action = "<cmd>Telescope live_grep<cr>";
        desc = "find line in cwd";
      };
      "<leader>fg" = {
        action = "<cmd>Telescope git_files<cr>";
        desc = "find git file";
      };
    };
  };
}
