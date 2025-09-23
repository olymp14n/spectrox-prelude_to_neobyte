-- title:   core_util
-- author:  Olympian / Spectrox
-- desc:    n/a
-- site:    https://github.com/olymp14n
-- license: CC0 1.0 Universal
-- version: 1.0
-- script:  lua
-- references:
--   [1] https://gist.github.com/tylerneylon/81333721109155b2d244
--   [2] https://gist.github.com/Uradamus/10323382

-------------------------------------------------------------------------------
--- UTIL
-------------------------------------------------------------------------------

function core_util_deep_copy(obj)
	-- https://gist.github.com/tylerneylon/81333721109155b2d244
	if type(obj) ~= 'table' then return obj end
	local res = {}
	for k, v in pairs(obj) do res[core_util_deep_copy(k)] = core_util_deep_copy(v) end
	return res
end

function core_util_shuffle(table)
	-- https://gist.github.com/Uradamus/10323382
	for i = #table, 2, -1 do
		local j = math.random(i)
		table[i], table[j] = table[j], table[i]
	end
end
