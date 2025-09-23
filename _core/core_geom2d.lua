-- title:   core_geom2d
-- author:  Olympian / Spectrox
-- desc:    n/a
-- site:    https://github.com/olymp14n
-- license: CC0 1.0 Universal
-- version: 1.0
-- script:  lua
-- requires: core_constants
-- references:
--  [1] https://gist.github.com/sixFingers/ee5c1dce72206edc5a42b3246a52ce2e
--  [2] https://en.wikibooks.org/wiki/Algorithm_Implementation/Geometry/Convex_hull/Monotone_chain

-------------------------------------------------------------------------------
--- GEOM2D
-------------------------------------------------------------------------------

local table_sort = table.sort
local table_insert = table.insert
local table_remove = table.remove

local function core_geom2d_cross(a, b, o)
	return (a[X] - o[X]) * (b[Y] - o[Y]) - (a[Y] - o[Y]) * (b[X] - o[X])
end

function core_geom2d_convex_hull(points)
	-- From: https://gist.github.com/sixFingers/ee5c1dce72206edc5a42b3246a52ce2e
	-- Based on: https://en.wikibooks.org/wiki/Algorithm_Implementation/Geometry/Convex_hull/Monotone_chain

	table_sort(points, function(a, b)
		return a[X] == b[X] and a[Y] > b[Y] or a[X] > b[X]
	end)

	local lower = {}
	for i = 1, #points do
		while (#lower >= 2 and core_geom2d_cross(lower[#lower - 1], lower[#lower], points[i]) <= 0) do
			table_remove(lower, #lower)
		end

		table_insert(lower, points[i])
	end

	local upper = {}
	for i = #points, 1, -1 do
		while (#upper >= 2 and core_geom2d_cross(upper[#upper - 1], upper[#upper], points[i]) <= 0) do
			table_remove(upper, #upper)
		end

		table_insert(upper, points[i])
	end

	table_remove(upper, #upper)
	table_remove(lower, #lower)
	for _, point in ipairs(lower) do
		table_insert(upper, point)
	end

	return upper
end
