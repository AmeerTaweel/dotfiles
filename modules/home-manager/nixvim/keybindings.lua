-- { System Clipboard }

local clipboard_mappings = {
	normal_visual = {
		d = { [["+d]], "delete to clipboard" },
		p = { [["+p]], "paste from clipboard" },
		P = { [["+P]], "paste from clipboard" },
		y = { [["+y]], "yank to clipboard" }
	},
	normal_only = {
		D = { [["+d$]], "delete to clipboard (d$)" },
		Y = { [["+y$]], "yank to clipboard (y$)" }
	}
}

which_key.register(clipboard_mappings.normal_only,   { prefix = "<leader>", mode = "n" })
which_key.register(clipboard_mappings.normal_visual, { prefix = "<leader>", mode = "n" })
which_key.register(clipboard_mappings.normal_visual, { prefix = "<leader>", mode = "x" })

-- { WinShift }

which_key.register({
	w = {
		name = "window",
		m = { "<cmd>WinShift<cr>", "window move" },
		s = { "<cmd>WinShift swap<cr>", "window swap" }
	}
}, { prefix = "<leader>" })

-- { Tabs }

which_key.register({
	t = {
		c = { "<cmd>tabclose<cr>", "close tab" },
		n = { "<cmd>tabnew<cr>", "new tab" },
	}
}, { prefix = "<leader>" })


-- { Telescope }

which_key.register({
	f = {
		name = "find",
		b = { telescope.builtin.buffers, "find buffer" },
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
	}
}, { prefix = "<leader>" })

-- { Git }

which_key.register({
	vs = { "<cmd>Neogit<cr>", "open neogit" }
}, { prefix = "<leader>" })

-- { Toggle }

which_key.register({
	t = {
		h = { "<cmd>nohlsearch<cr>", "toggle search highlight" },
		s = { "<cmd>set spell!<cr>", "toggle spell checking" }
	}
}, { prefix = "<leader>" })


-- { Next / Previous }

which_key.register({
	["["] = {
		name = "previous",
		q = { "<cmd>cprevious<cr>", "previous quickfix list item" },
		l = { "<cmd>lprevious<cr>", "previous location list item" },
		d = { vim.diagnostic.goto_prev, "previous lsp diagnostic" }
	},
	["]"] = {
		name = "next",
		q = { "<cmd>cnext<cr>", "next quickfix list item" },
		l = { "<cmd>lnext<cr>", "next location list item" },
		d = { vim.diagnostic.goto_next, "next lsp diagnostic" }
	}
})

-- { Open / Close }

which_key.register({
	o = {
		name = "open",
		q = { "<cmd>copen<cr>", "open quickfix list" },
		l = { "<cmd>lopen<cr>", "open location list" },
	},
	c = {
		name = "close",
		q = { "<cmd>cclose<cr>", "close quickfix list" },
		l = { "<cmd>lclose<cr>", "close location list" },
	}
}, { prefix = "<leader>" })

-- { List }

which_key.register({
	l = {
		name = "list",
		d = { vim.diagnostic.setloclist, "list diagnostics <location list>" },
		r = { vim.lsp.buf.references, "list references <quickfix list>" },
		t = { "<cmd>TODOQuickfixList<cr>", "list todos <quickfix list>" }
	}
}, { prefix = "<leader>" })

-- { Go To }

which_key.register({
	g = {
		name = "go to",
		d = { vim.lsp.buf.definition, "go to definition" },
		D = { vim.lsp.buf.declaration, "go to declaration" },
		i = { vim.lsp.buf.implementation, "go to implementation" },
		t = { vim.lsp.buf.type_definition, "go to type definition" }
	}
}, { prefix = "<leader>" })

-- { Action }

which_key.register({
	a = {
		name = "action",
		r = { vim.lsp.buf.rename, "rename symbol" },
		c = { vim.lsp.buf.code_action, "code action" },
		p = { lsp_utils.peek_definition, "peek definition" },
		f = { vim.lsp.buf.formatting, "format code" },
		h = { vim.lsp.buf.hover, "show hover information" },
		s = { vim.lsp.buf.signature_help, "show signature help" }
	}
}, { prefix = "<leader>" })

which_key.register({
	a = {
		name = "action",
		c = { vim.lsp.buf.range_code_action, "range code action" },
		f = { vim.lsp.buf.range_formatting, "format range" }
	}
}, { prefix = "<leader>", mode = "x" })

-- { Job }

which_key.register({
	j = {
		name = "start job",
		f = {
			name = "file jobs",
			b = { "<cmd>AsyncTask file-build<cr>", "file build" },
			r = { "<cmd>AsyncTask file-run<cr>", "file run" }
		}
	}
}, { prefix = "<leader>" })

-- { Others }

which_key.register({
	["<cr>"] = { "o<esc>", "insert new line" }
})

which_key.register({
	["<space>"] = { "<plug>Lightspeed_,_ft", "repeat last f/t/F/T in opposite direction" }
}, { noremap = false })
