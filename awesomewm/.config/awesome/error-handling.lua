-- { Error handling }

local naughty = require("naughty") -- Notification library

-- {{ Startup Errors }}
-- Check if awesome encountered an error during startup.
-- Fall back to default config in the case of a startup error.
if awesome.startup_errors then
	naughty.notify({
		preset = naughty.config.presets.critical,
		title = "Startup Error",
		text = awesome.startup_errors
	})
end

-- {{ Runtime Errors }}
-- Handle runtime errors after startup.
do
	local in_error = false
	awesome.connect_signal("debug::error", function (err)
		-- Make sure we don't go into an endless error loop.
		if in_error then return end
		in_error = true

		naughty.notify({
			preset = naughty.config.presets.critical,
			title = "Runtime Error",
			text = tostring(err)
		})

		in_error = false
	end)
end
