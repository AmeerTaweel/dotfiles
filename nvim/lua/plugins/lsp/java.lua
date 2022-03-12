local paths = require "plugins.lsp.paths"

local customize_server_options = function(config)
	config.cmd = { paths.java };
end

return customize_server_options
