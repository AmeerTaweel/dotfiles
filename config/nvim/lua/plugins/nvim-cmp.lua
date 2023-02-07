local cmp = require "cmp"
local types = require "cmp.types"

local t = function(str)
	return vim.api.nvim_replace_termcodes(str, true, true, true)
end

cmp.setup {
	completion = { autocomplete = { types.cmp.TriggerEvent.TextChanged } },
	snippet = {
		expand = function(args) vim.fn["UltiSnips#Anon"](args.body) end
	},
	mapping = {
		["<tab>"] = cmp.mapping({
			c = function()
				if cmp.visible() then
					return cmp.select_next_item({ behavior = cmp.SelectBehavior.Insert })
				end
				cmp.complete()
			end,
			i = function(fallback)
				if cmp.visible() then
					return cmp.select_next_item({ behavior = cmp.SelectBehavior.Insert })
				elseif vim.fn["UltiSnips#CanJumpForwards"]() == 1 then
					return vim.api.nvim_feedkeys(t("<plug>(ultisnips_jump_forward)"), "m", true)
				end
				fallback()
			end,
			s = function(fallback)
				if vim.fn["UltiSnips#CanJumpForwards"]() == 1 then
					return vim.api.nvim_feedkeys(t("<plug>(ultisnips_jump_forward)"), "m", true)
				end
				fallback()
			end
		}),
		["<s-tab>"] = cmp.mapping({
			c = function()
				if cmp.visible() then
					return cmp.select_prev_item({ behavior = cmp.SelectBehavior.Insert })
				end
				cmp.complete()
			end,
			i = function(fallback)
				if cmp.visible() then
					return cmp.select_prev_item({ behavior = cmp.SelectBehavior.Insert })
				elseif vim.fn["UltiSnips#CanJumpBackwards"]() == 1 then
					return vim.api.nvim_feedkeys(t("<plug>(ultisnips_jump_backward)"), "m", true)
				end
				fallback()
			end,
			s = function(fallback)
				if vim.fn["UltiSnips#CanJumpBackwards"]() == 1 then
					return vim.api.nvim_feedkeys(t("<plug>(ultisnips_jump_backward)"), "m", true)
				end
				fallback()
			end
		}),
		["<c-n>"] = cmp.mapping({
			c = function()
				if cmp.visible() then
					return cmp.select_next_item({ behavior = cmp.SelectBehavior.Select })
				end
				vim.api.nvim_feedkeys(t("<down>"), "n", true)
			end,
			i = function(fallback)
				if cmp.visible() then
					return cmp.select_next_item({ behavior = cmp.SelectBehavior.Select })
				end
				fallback()
			end
		}),
		["<c-p>"] = cmp.mapping({
			c = function()
				if cmp.visible() then
					return cmp.select_prev_item({ behavior = cmp.SelectBehavior.Select })
				end
				vim.api.nvim_feedkeys(t("<up>"), "n", true)
			end,
			i = function(fallback)
				if cmp.visible() then
					return cmp.select_prev_item({ behavior = cmp.SelectBehavior.Select })
				end
				fallback()
			end
		}),
		["<c-b>"] = cmp.mapping(cmp.mapping.scroll_docs(-4), {"i", "c"}),
		["<c-f>"] = cmp.mapping(cmp.mapping.scroll_docs(4), {"i", "c"}),
		["<c-e>"] = cmp.mapping({
			i = function(fallback)
				if cmp.visible() then
					return cmp.confirm({ behavior = cmp.ConfirmBehavior.Replace, select = false })
				end
				fallback()
			end
		})
	},
	sources = {
		{ name = "ultisnips" },
		{ name = "nvim_lsp" },
		{ name = "nvim_lua" },
		{ name = "buffer" },
		{ name = "path" }
	}
}

-- Configure autocompletion for searches
cmp.setup.cmdline("/", {
	completion = { autocomplete = { types.cmp.TriggerEvent.TextChanged } },
	sources = {
		{ name = "buffer" }
	}
})

-- Configure autocompletion for commands
cmp.setup.cmdline(":", {
	completion = { autocomplete = { types.cmp.TriggerEvent.TextChanged } },
	sources = {
		{ name = "path" },
		{ name = "cmdline" }
	}
})
