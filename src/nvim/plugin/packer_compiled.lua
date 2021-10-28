-- Automatically generated packer.nvim plugin loader code

if vim.api.nvim_call_function('has', {'nvim-0.5'}) ~= 1 then
  vim.api.nvim_command('echohl WarningMsg | echom "Invalid Neovim version for packer.nvim! | echohl None"')
  return
end

vim.api.nvim_command('packadd packer.nvim')

local no_errors, error_msg = pcall(function()

  local time
  local profile_info
  local should_profile = false
  if should_profile then
    local hrtime = vim.loop.hrtime
    profile_info = {}
    time = function(chunk, start)
      if start then
        profile_info[chunk] = hrtime()
      else
        profile_info[chunk] = (hrtime() - profile_info[chunk]) / 1e6
      end
    end
  else
    time = function(chunk, start) end
  end
  
local function save_profiles(threshold)
  local sorted_times = {}
  for chunk_name, time_taken in pairs(profile_info) do
    sorted_times[#sorted_times + 1] = {chunk_name, time_taken}
  end
  table.sort(sorted_times, function(a, b) return a[2] > b[2] end)
  local results = {}
  for i, elem in ipairs(sorted_times) do
    if not threshold or threshold and elem[2] > threshold then
      results[i] = elem[1] .. ' took ' .. elem[2] .. 'ms'
    end
  end

  _G._packer = _G._packer or {}
  _G._packer.profile_output = results
end

time([[Luarocks path setup]], true)
local package_path_str = "/home/ameer-taweel/.cache/nvim/packer_hererocks/2.1.0-beta3/share/lua/5.1/?.lua;/home/ameer-taweel/.cache/nvim/packer_hererocks/2.1.0-beta3/share/lua/5.1/?/init.lua;/home/ameer-taweel/.cache/nvim/packer_hererocks/2.1.0-beta3/lib/luarocks/rocks-5.1/?.lua;/home/ameer-taweel/.cache/nvim/packer_hererocks/2.1.0-beta3/lib/luarocks/rocks-5.1/?/init.lua"
local install_cpath_pattern = "/home/ameer-taweel/.cache/nvim/packer_hererocks/2.1.0-beta3/lib/lua/5.1/?.so"
if not string.find(package.path, package_path_str, 1, true) then
  package.path = package.path .. ';' .. package_path_str
end

if not string.find(package.cpath, install_cpath_pattern, 1, true) then
  package.cpath = package.cpath .. ';' .. install_cpath_pattern
end

time([[Luarocks path setup]], false)
time([[try_loadstring definition]], true)
local function try_loadstring(s, component, name)
  local success, result = pcall(loadstring(s))
  if not success then
    vim.schedule(function()
      vim.api.nvim_notify('packer.nvim: Error running ' .. component .. ' for ' .. name .. ': ' .. result, vim.log.levels.ERROR, {})
    end)
  end
  return result
end

time([[try_loadstring definition]], false)
time([[Defining packer_plugins]], true)
_G.packer_plugins = {
  ["ayu-vim"] = {
    loaded = true,
    path = "/home/ameer-taweel/.local/share/nvim/site/pack/packer/start/ayu-vim"
  },
  darcula = {
    loaded = true,
    path = "/home/ameer-taweel/.local/share/nvim/site/pack/packer/start/darcula"
  },
  dracula = {
    loaded = true,
    path = "/home/ameer-taweel/.local/share/nvim/site/pack/packer/start/dracula"
  },
  gruvbox = {
    loaded = true,
    path = "/home/ameer-taweel/.local/share/nvim/site/pack/packer/start/gruvbox"
  },
  ["indent-blankline.nvim"] = {
    config = { "\27LJ\2\nA\0\0\3\0\2\0\0046\0\0\0'\2\1\0B\0\2\1K\0\1\0&plugin-config.ts.indent-blankline\frequire\0" },
    loaded = true,
    path = "/home/ameer-taweel/.local/share/nvim/site/pack/packer/start/indent-blankline.nvim",
    wants = { "nvim-treesitter" }
  },
  ["nord-vim"] = {
    loaded = true,
    path = "/home/ameer-taweel/.local/share/nvim/site/pack/packer/start/nord-vim"
  },
  ["nvim-autopairs"] = {
    config = { "\27LJ\2\n?\0\0\3\0\2\0\0046\0\0\0'\2\1\0B\0\2\1K\0\1\0$plugin-config.ts.nvim-autopairs\frequire\0" },
    loaded = true,
    path = "/home/ameer-taweel/.local/share/nvim/site/pack/packer/start/nvim-autopairs",
    wants = { "nvim-treesitter" }
  },
  ["nvim-compe"] = {
    config = { "\27LJ\2\n8\0\0\3\0\2\0\0046\0\0\0'\2\1\0B\0\2\1K\0\1\0\29plugin-config.nvim-compe\frequire\0" },
    loaded = true,
    path = "/home/ameer-taweel/.local/share/nvim/site/pack/packer/start/nvim-compe"
  },
  ["nvim-lsp-installer"] = {
    config = { "\27LJ\2\n@\0\0\3\0\2\0\0046\0\0\0'\2\1\0B\0\2\1K\0\1\0%plugin-config.lsp.nvim-lspconfig\frequire\0" },
    loaded = true,
    path = "/home/ameer-taweel/.local/share/nvim/site/pack/packer/start/nvim-lsp-installer",
    wants = { "nvim-lspconfig" }
  },
  ["nvim-lspconfig"] = {
    loaded = true,
    path = "/home/ameer-taweel/.local/share/nvim/site/pack/packer/start/nvim-lspconfig",
    wants = { "which-key.nvim" }
  },
  ["nvim-tree.lua"] = {
    config = { "\27LJ\2\n7\0\0\3\0\2\0\0046\0\0\0'\2\1\0B\0\2\1K\0\1\0\28plugin-config.file-tree\frequire\0" },
    loaded = true,
    path = "/home/ameer-taweel/.local/share/nvim/site/pack/packer/start/nvim-tree.lua"
  },
  ["nvim-treesitter"] = {
    config = { "\27LJ\2\n@\0\0\3\0\2\0\0046\0\0\0'\2\1\0B\0\2\1K\0\1\0%plugin-config.ts.nvim-treesitter\frequire\0" },
    loaded = true,
    path = "/home/ameer-taweel/.local/share/nvim/site/pack/packer/start/nvim-treesitter"
  },
  ["nvim-ts-autotag"] = {
    config = { "\27LJ\2\n@\0\0\3\0\2\0\0046\0\0\0'\2\1\0B\0\2\1K\0\1\0%plugin-config.ts.nvim-ts-autotag\frequire\0" },
    loaded = true,
    path = "/home/ameer-taweel/.local/share/nvim/site/pack/packer/start/nvim-ts-autotag",
    wants = { "nvim-treesitter" }
  },
  ["nvim-ts-context-commentstring"] = {
    config = { "\27LJ\2\nN\0\0\3\0\2\0\0046\0\0\0'\2\1\0B\0\2\1K\0\1\0003plugin-config.ts.nvim-ts-context-commentstring\frequire\0" },
    loaded = true,
    path = "/home/ameer-taweel/.local/share/nvim/site/pack/packer/start/nvim-ts-context-commentstring",
    wants = { "nvim-treesitter" }
  },
  ["nvim-ts-rainbow"] = {
    config = { "\27LJ\2\n@\0\0\3\0\2\0\0046\0\0\0'\2\1\0B\0\2\1K\0\1\0%plugin-config.ts.nvim-ts-rainbow\frequire\0" },
    loaded = true,
    path = "/home/ameer-taweel/.local/share/nvim/site/pack/packer/start/nvim-ts-rainbow",
    wants = { "nvim-treesitter" }
  },
  ["nvim-web-devicons"] = {
    config = { "\27LJ\2\n3\0\0\3\0\2\0\0046\0\0\0'\2\1\0B\0\2\1K\0\1\0\24plugin-config.icons\frequire\0" },
    loaded = true,
    path = "/home/ameer-taweel/.local/share/nvim/site/pack/packer/start/nvim-web-devicons"
  },
  onehalf = {
    loaded = true,
    path = "/home/ameer-taweel/.local/share/nvim/site/pack/packer/start/onehalf/vim"
  },
  ["packer.nvim"] = {
    loaded = true,
    path = "/home/ameer-taweel/.local/share/nvim/site/pack/packer/start/packer.nvim"
  },
  playground = {
    loaded = true,
    path = "/home/ameer-taweel/.local/share/nvim/site/pack/packer/start/playground",
    wants = { "nvim-treesitter" }
  },
  ["plenary.nvim"] = {
    loaded = true,
    path = "/home/ameer-taweel/.local/share/nvim/site/pack/packer/start/plenary.nvim"
  },
  ["popup.nvim"] = {
    loaded = true,
    path = "/home/ameer-taweel/.local/share/nvim/site/pack/packer/start/popup.nvim"
  },
  ["telescope-fzy-native.nvim"] = {
    loaded = true,
    path = "/home/ameer-taweel/.local/share/nvim/site/pack/packer/start/telescope-fzy-native.nvim"
  },
  ["telescope.nvim"] = {
    config = { "\27LJ\2\n>\0\0\3\0\2\0\0046\0\0\0'\2\1\0B\0\2\1K\0\1\0#plugin-config.telescope-config\frequire\0" },
    loaded = true,
    path = "/home/ameer-taweel/.local/share/nvim/site/pack/packer/start/telescope.nvim"
  },
  ["vim-commentary"] = {
    loaded = true,
    path = "/home/ameer-taweel/.local/share/nvim/site/pack/packer/start/vim-commentary"
  },
  ["vim-eunuch"] = {
    loaded = true,
    path = "/home/ameer-taweel/.local/share/nvim/site/pack/packer/start/vim-eunuch"
  },
  ["vim-fugitive"] = {
    loaded = true,
    path = "/home/ameer-taweel/.local/share/nvim/site/pack/packer/start/vim-fugitive"
  },
  ["vim-highlightedyank"] = {
    loaded = true,
    path = "/home/ameer-taweel/.local/share/nvim/site/pack/packer/start/vim-highlightedyank"
  },
  ["vim-tmux-navigator"] = {
    config = { "\27LJ\2\n@\0\0\3\0\2\0\0046\0\0\0'\2\1\0B\0\2\1K\0\1\0%plugin-config.vim-tmux-navigator\frequire\0" },
    loaded = true,
    path = "/home/ameer-taweel/.local/share/nvim/site/pack/packer/start/vim-tmux-navigator"
  },
  ["vim-windowswap"] = {
    config = { "\27LJ\2\n9\0\0\3\0\2\0\0046\0\0\0'\2\1\0B\0\2\1K\0\1\0\30plugin-config.window-swap\frequire\0" },
    loaded = true,
    path = "/home/ameer-taweel/.local/share/nvim/site/pack/packer/start/vim-windowswap"
  },
  ["which-key.nvim"] = {
    config = { "\27LJ\2\n7\0\0\3\0\2\0\0046\0\0\0'\2\1\0B\0\2\1K\0\1\0\28plugin-config.which-key\frequire\0" },
    loaded = true,
    path = "/home/ameer-taweel/.local/share/nvim/site/pack/packer/start/which-key.nvim"
  }
}

time([[Defining packer_plugins]], false)
-- Runtimepath customization
time([[Runtimepath customization]], true)
vim.o.runtimepath = vim.o.runtimepath .. ",/home/ameer-taweel/.local/share/nvim/site/pack/packer/start/onehalf/vim"
time([[Runtimepath customization]], false)
-- Config for: nvim-compe
time([[Config for nvim-compe]], true)
try_loadstring("\27LJ\2\n8\0\0\3\0\2\0\0046\0\0\0'\2\1\0B\0\2\1K\0\1\0\29plugin-config.nvim-compe\frequire\0", "config", "nvim-compe")
time([[Config for nvim-compe]], false)
-- Config for: vim-tmux-navigator
time([[Config for vim-tmux-navigator]], true)
try_loadstring("\27LJ\2\n@\0\0\3\0\2\0\0046\0\0\0'\2\1\0B\0\2\1K\0\1\0%plugin-config.vim-tmux-navigator\frequire\0", "config", "vim-tmux-navigator")
time([[Config for vim-tmux-navigator]], false)
-- Config for: nvim-tree.lua
time([[Config for nvim-tree.lua]], true)
try_loadstring("\27LJ\2\n7\0\0\3\0\2\0\0046\0\0\0'\2\1\0B\0\2\1K\0\1\0\28plugin-config.file-tree\frequire\0", "config", "nvim-tree.lua")
time([[Config for nvim-tree.lua]], false)
-- Config for: nvim-web-devicons
time([[Config for nvim-web-devicons]], true)
try_loadstring("\27LJ\2\n3\0\0\3\0\2\0\0046\0\0\0'\2\1\0B\0\2\1K\0\1\0\24plugin-config.icons\frequire\0", "config", "nvim-web-devicons")
time([[Config for nvim-web-devicons]], false)
-- Config for: nvim-treesitter
time([[Config for nvim-treesitter]], true)
try_loadstring("\27LJ\2\n@\0\0\3\0\2\0\0046\0\0\0'\2\1\0B\0\2\1K\0\1\0%plugin-config.ts.nvim-treesitter\frequire\0", "config", "nvim-treesitter")
time([[Config for nvim-treesitter]], false)
-- Config for: vim-windowswap
time([[Config for vim-windowswap]], true)
try_loadstring("\27LJ\2\n9\0\0\3\0\2\0\0046\0\0\0'\2\1\0B\0\2\1K\0\1\0\30plugin-config.window-swap\frequire\0", "config", "vim-windowswap")
time([[Config for vim-windowswap]], false)
-- Config for: which-key.nvim
time([[Config for which-key.nvim]], true)
try_loadstring("\27LJ\2\n7\0\0\3\0\2\0\0046\0\0\0'\2\1\0B\0\2\1K\0\1\0\28plugin-config.which-key\frequire\0", "config", "which-key.nvim")
time([[Config for which-key.nvim]], false)
-- Config for: telescope.nvim
time([[Config for telescope.nvim]], true)
try_loadstring("\27LJ\2\n>\0\0\3\0\2\0\0046\0\0\0'\2\1\0B\0\2\1K\0\1\0#plugin-config.telescope-config\frequire\0", "config", "telescope.nvim")
time([[Config for telescope.nvim]], false)
-- Config for: nvim-ts-rainbow
time([[Config for nvim-ts-rainbow]], true)
try_loadstring("\27LJ\2\n@\0\0\3\0\2\0\0046\0\0\0'\2\1\0B\0\2\1K\0\1\0%plugin-config.ts.nvim-ts-rainbow\frequire\0", "config", "nvim-ts-rainbow")
time([[Config for nvim-ts-rainbow]], false)
-- Config for: nvim-autopairs
time([[Config for nvim-autopairs]], true)
try_loadstring("\27LJ\2\n?\0\0\3\0\2\0\0046\0\0\0'\2\1\0B\0\2\1K\0\1\0$plugin-config.ts.nvim-autopairs\frequire\0", "config", "nvim-autopairs")
time([[Config for nvim-autopairs]], false)
-- Config for: nvim-lsp-installer
time([[Config for nvim-lsp-installer]], true)
try_loadstring("\27LJ\2\n@\0\0\3\0\2\0\0046\0\0\0'\2\1\0B\0\2\1K\0\1\0%plugin-config.lsp.nvim-lspconfig\frequire\0", "config", "nvim-lsp-installer")
time([[Config for nvim-lsp-installer]], false)
-- Config for: indent-blankline.nvim
time([[Config for indent-blankline.nvim]], true)
try_loadstring("\27LJ\2\nA\0\0\3\0\2\0\0046\0\0\0'\2\1\0B\0\2\1K\0\1\0&plugin-config.ts.indent-blankline\frequire\0", "config", "indent-blankline.nvim")
time([[Config for indent-blankline.nvim]], false)
-- Config for: nvim-ts-autotag
time([[Config for nvim-ts-autotag]], true)
try_loadstring("\27LJ\2\n@\0\0\3\0\2\0\0046\0\0\0'\2\1\0B\0\2\1K\0\1\0%plugin-config.ts.nvim-ts-autotag\frequire\0", "config", "nvim-ts-autotag")
time([[Config for nvim-ts-autotag]], false)
-- Config for: nvim-ts-context-commentstring
time([[Config for nvim-ts-context-commentstring]], true)
try_loadstring("\27LJ\2\nN\0\0\3\0\2\0\0046\0\0\0'\2\1\0B\0\2\1K\0\1\0003plugin-config.ts.nvim-ts-context-commentstring\frequire\0", "config", "nvim-ts-context-commentstring")
time([[Config for nvim-ts-context-commentstring]], false)
if should_profile then save_profiles() end

end)

if not no_errors then
  vim.api.nvim_command('echohl ErrorMsg | echom "Error in packer_compiled: '..error_msg..'" | echom "Please check your config for correctness" | echohl None')
end
