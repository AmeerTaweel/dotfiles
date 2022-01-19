require "globals"

local which_key = require "which-key"
local lsp_utils = require "utils.lsp"
local telescope = require "telescope.builtin"

-- Leader Key
variables.global.mapleader = ","

-- { System Clipboard }

local clipboard_mappings = {
	d = { [["+d]], "delete to clipboard" },
	p = { [["+p]], "paste from clipboard" },
	P = { [["+P]], "paste from clipboard" },
	y = { [["+y]], "yank to clipboard" }
}

which_key.register(clipboard_mappings, { prefix = "<leader>", mode = "n" })
which_key.register(clipboard_mappings, { prefix = "<leader>", mode = "x" })

-- { Split Windows }

which_key.register({
	["-"] = { "<cmd>split<cr>", "split window horizontally" },
	["/"] = { "<cmd>vsplit<cr>", "split window vertically" },
}, { prefix = "<leader>" })

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
		n = { "<cmd>tabnew<cr>", "new tab" }
	}
}, { prefix = "<leader>" })


-- { Telescope }

which_key.register({
	f = {
		name = "find",
		b = { telescope.buffers, "find buffer" },
		c = { telescope.command_history, "find command" },
		f = { telescope.find_files, "find file" },
		l = {
			name = "find line",
			b = { telescope.current_buffer_fuzzy_find, "find line in buffer" },
			d = { telescope.live_grep, "find line in cwd" }
		},
		m = { telescope.marks, "find mark" },
		r = { telescope.registers, "find register" },
		s = { telescope.search_history, "find search" }
	}
}, { prefix = "<leader>" })

-- { Git }

which_key.register({
	g = {
		name = "git",
		c = { "<cmd>Neogit commit<cr>", "commit" },
		l = { "<cmd>Neogit log<cr>", "log" },
		m = { "<cmd>Neogit<cr>", "manage" },
		p = { "<cmd>Neogit push<cr>", "push" },
	}
}, { prefix = "<leader>" })

-- { Toggle }

which_key.register({
	t = {
		h = { "<cmd>set hlsearch!<cr>", "toggle search highlight" },
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
		r = { vim.lsp.buf.references, "list references <quickfix list>" },
		d = { vim.diagnostic.setloclist, "list diagnostics <location list>" }
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


-- { Others }

which_key.register({
	["<cr>"] = { "o<esc>", "insert new line" },
	["<space>"] = { ",", "repeat last f/t/F/T in opposite direction" }
})
