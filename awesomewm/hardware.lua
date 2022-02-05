local metatables = require("lua.metatables")

local M = {}

--[[
Device types come from the SMBIOS specification.

The meaning of the values can be seen in official documentation PDFs available
at https://www.dmtf.org/standards/SMBIOS.

The following table contains values from the SMBIOS specification version 3.5.0,
that was released in the 22nd of September 2021.
--]]
local device_types = {
	["3\n"] = { name = "Desktop", is_laptop = false },
	["8\n"] = { name = "Portable", is_laptop = true },
	["9\n"] = { name = "Laptop", is_laptop = true },
	["10\n"] = { name = "Notebook", is_laptop = true },
	["14\n"] = { name = "Sub Notebook", is_laptop = true },
}

metatables.set_default_value(
	device_types,
	{ name = "Unknown", is_laptop = false }
)

local device_type_file = "/sys/class/dmi/id/chassis_type"

M.device_type = device_types[io.popen("cat " .. device_type_file):read("*all")]

return M
