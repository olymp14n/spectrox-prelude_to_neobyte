-- title:   core_pcx
-- author:  Olympian / Spectrox
-- desc:    n/a
-- site:    https://github.com/olymp14n
-- license: CC0 1.0 Universal
-- version: 1.0
-- script:  lua
-- references:
--   [1] https://www.fileformat.info/format/pcx/egff.htm
--   [2] https://moddingwiki.shikadi.net/wiki/PCX_Format

-------------------------------------------------------------------------------
--- PCX
-------------------------------------------------------------------------------

--- Decode PCX data.
--- @param data PCX data. Must have 4 bits per pixel for a maximum of 16 colors.
--- @return table
function core_pcx_decode(data)
	local id = data[1]
	if id ~= 0xa then
		error("Invalid PCX data!")
	end

	local bpp = data[4]
	if bpp ~= 4 then
		error("PCX data must have 4 bits per pixel!")
	end

	local w = data[10] << 8 | data[9] + 1
	local h = data[12] << 8 | data[11] + 1
	local bpl = data[68] << 8 | data[67] -- Number of undecoded bytes per line.

	-- Read palette.
	local pal = {}

	-- Start at byte 17 for a total length of 48 bytes.
	for i = 17, 64 do
		table.insert(pal, data[i])
	end

	-- Read pixel data.
	local tmp = {} -- Decoded pixel data. Still contains potential line padding.
	local i = 129 -- Start of pixel data.
	while i <= #data do
		local count = 0
		local value = 0
		if ((data[i] & 0xc0) == 0xc0) then
			count = data[i] & 0x3f
			i = i + 1
			value = data[i]
		else
			count = 1
			value = data[i]
		end

		for j = 1, count do
			table.insert(tmp, value >> 4)
			table.insert(tmp, value & 0xf)
		end
		i = i + 1
	end

	-- Final decoded pixel data.
	local decoded = {}

	-- Transfer pixel data to final buffer while skipping padding bytes.
	-- `bpl` is the number of undecoded bytes. Every byte contains two color indexes.
	for j = 1, #tmp, 2 * bpl do
		for k = 0, 2 * bpl do
			if k < w then
				table.insert(decoded, tmp[j + k])
			end
		end
	end

	return {
		width = w,
		height = h,
		width_1 = w - 1,
		height_1 = h - 1,
		width_half = w // 2,
		height_half = h // 2,
		palette = pal,
		pixels = decoded
	}
end
