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
        desc = "find command from history";
      };
    };
  };
}
/*
		c = { telescope.builtin.command_history, "find command" },
		f = { telescope.builtin.find_files, "find file" },
		h = { telescope.builtin.help_tags, "find help" },
		j = { telescope.extensions.asynctasks.all, "find job" },
		l = {
			name = "find line",
			b = { telescope.builtin.current_buffer_fuzzy_find, "find line in buffer" },
			d = { telescope.builtin.live_grep, "find line in cwd" }
		},
		m = { telescope.builtin.marks, "find mark" },
		r = { telescope.builtin.registers, "find register" },
		s = { telescope.builtin.search_history, "find search" },
		t = { "<cmd>TODOTelescope<cr>", "find todo" },
		u = { telescope.extensions.ultisnips.ultisnips, "find ultisnippet" }
*/
