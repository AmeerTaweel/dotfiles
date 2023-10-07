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
    keymaps = [
      {
        key = "<leader>fb";
        action = "<cmd>Telescope buffers<cr>";
        options.desc = "find buffer";
      } {
        key = "<leader>fc";
        action = "<cmd>Telescope command_history<cr>";
        options.desc = "search command history";
      } {
        key = "<leader>ff";
        action = "<cmd>Telescope find_files<cr>";
        options.desc = "find file";
      } {
        key = "<leader>fh";
        action = "<cmd>Telescope help_tags<cr>";
        options.desc = "find help";
      } {
        key = "<leader>fm";
        action = "<cmd>Telescope marks<cr>";
        options.desc = "find mark";
      } {
        key = "<leader>fr";
        action = "<cmd>Telescope registers<cr>";
        options.desc = "find register";
      } {
        key = "<leader>fs";
        action = "<cmd>Telescope search_history<cr>";
        options.desc = "search search history";
      } {
        key = "<leader>ft";
        action = "<cmd>TodoTelescope<cr>";
        options.desc = "find todo";
      } {
        key = "<leader>flb";
        action = "<cmd>Telescope current_buffer_fuzzy_find<cr>";
        options.desc = "find line in buffer";
      } {
        key = "<leader>fld";
        action = "<cmd>Telescope live_grep<cr>";
        options.desc = "find line in cwd";
      } {
        key = "<leader>fg";
        action = "<cmd>Telescope git_files<cr>";
        options.desc = "find git file";
      }
    ];
  };
}
