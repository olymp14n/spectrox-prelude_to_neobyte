-- title:   core_dither
-- author:  Olympian / Spectrox
-- desc:    n/a
-- site:    https://github.com/olymp14n
-- license: CC0 1.0 Universal
-- version: 1.0
-- script:  lua
-- references:
--   [1] https://github.com/tromero/BayerMatrix?tab=readme-ov-file
--   [2] http://caca.zoy.org/study/part2.html
--   [3] https://github.com/makew0rld/dither/blob/v2..0/ordered_ditherers.go
--   [4] https://archive.is/71e9G
--   [5] Digital Halftoning https://core.ac.uk/download/pdf/4381809.pdf
--   [6] https://www.makeworld.space/garden/Computers/Image%20Dithering.html

-------------------------------------------------------------------------------
--- DITHER
-------------------------------------------------------------------------------

CoreDitherMatrixBayer2x2 = {
	size = 2,
	values = {
		.00, .50,
		.75, .25
	}
}

CoreDitherMatrixBayer4x4 = {
	size = 4,
	values = {
		.0000, .5000, .1250, .6250,
		.7500, .2500, .8750, .3750,
		.1875, .6875, .0625, .5625,
		.9375, .4375, .8125, .3125
	}
}

CoreDitherMatrixBayer8x8 = {
	size = 8,
	values = {
		.000000, .500000, .125000, .625000, .031250, .531250, .156250, .656250,
		.750000, .250000, .875000, .375000, .781250, .281250, .906250, .406250,
		.187500, .687500, .062500, .562500, .218750, .718750, .093750, .593750,
		.937500, .437500, .812500, .312500, .968750, .468750, .843750, .343750,
		.046875, .546875, .171875, .671875, .015625, .515625, .140625, .640625,
		.796875, .296875, .921875, .421875, .765625, .265625, .890625, .390625,
		.234375, .734375, .109375, .609375, .203125, .703125, .078125, .578125,
		.984375, .484375, .859375, .359375, .953125, .453125, .828125, .328125
	}
}

CoreDitherMatrixHalftone8x8 = {
	size = 8,
	values = {
		.500, .625, .750, .875, .875, .750, .625, .500,
		.625, .750, .875, 1.00, 1.00, .875, .750, .625,
		.750, .875, 1.00, .125, .125, 1.00, .875, .750,
		.875, 1.00, .125, .000, .000, .125, 1.00, .875,
		.875, 1.00, .125, .000, .000, .125, 1.00, .875,
		.750, .875, 1.00, .125, .125, 1.00, .875, .750,
		.625, .750, .875, 1.00, 1.00, .875, .750, .625,
		.500, .625, .750, .875, .875, .750, .625, .500
	}
}


CoreDitherMatrixHalftoneB8x8 = {
	size = 8,
	values = {
		.500, .625, .750, .875, .875, .750, .625, .500,
		.375, .500, .625, .750, .750, .625, .500, .375,
		.250, .375, .500, .625, .625, .500, .375, .250,
		.125, .250, .375, .500, .500, .375, .250, .125,
		.125, .250, .375, .500, .500, .375, .250, .125,
		.250, .375, .500, .625, .625, .500, .375, .250,
		.375, .500, .625, .750, .750, .625, .500, .375,
		.500, .625, .750, .875, .875, .750, .625, .500
	}
}

--- Modified Bayer8x8 with some vertical lines.
CoreDitherMatrixVertBayer8x8 = {
	size = 8,
	values = {
		.000000, .500000, .125000, .625000, .031250, .531250, .156250, .656250,
		.750000, .250000, .875000, .375000, .062500, .562500, .187500, .687500,
		.187500, .687500, .062500, .562500, .218750, .718750, .093750, .593750,
		.937500, .437500, .812500, .312500, .093750, .593750, .218750, .718750,
		.046875, .546875, .140625, .640625, .015625, .515625, .109375, .609375,
		.765625, .265625, .859375, .359375, .078125, .578125, .203125, .703125,
		.234375, .734375, .109375, .609375, .234375, .734375, .046875, .546875,
		.953125, .453125, .828125, .328125, .109375, .609375, .234375, .734375
	}
}

CoreDitherMatrixWildBayer8x8 = {
	size = 8,
	values = {
		.000, .258, .181, .336, .047, .304, .125, .383,
		.388, .219, .443, .274, .421, .251, .476, .186,
		.266, .649, .344, .602, .438, .695, .281, .367,
		.443, .361, .541, .372, .066, .449, .153, .503,
		.180, .563, .445, .635, .016, .516, .125, .625,
		.383, .301, .568, .410, .055, .405, .142, .492,
		.257, .500, .180, .609, .234, .719, .047, .547,
		.657, .511, .601, .432, .366, .372, .372, .514
	}
}

function core_dither(intensity, x, y, matrix)
	return intensity > matrix.values[1 + x % matrix.size + y % matrix.size * matrix.size]
end

function core_dither_rect(x, y, w, h, col, intensity, matrix)
	if intensity <= 0 then
		return
	end
	for yy = y, y + h do
		for xx = x, x + w do
			if intensity >= 1 or core_dither(intensity, xx, yy, matrix) then
				pix(xx, yy, col)
			end
		end
	end
end

function core_dither_render_vertical(x, y, w, h, col, bottomToTop, matrix)
	for yy = y, y + h do
		local br = bottomToTop and ((yy - y) / h) or (1 - (yy - y) / h)
		for xx = x, x + w do
			if core_dither(br, xx, yy, matrix) then
				pix(xx, yy, col)
			end
		end
	end
end

function core_dither_render_horizontal(x, y, w, h, col, leftToRight, matrix)
	for xx = x, x + w do
		local br = leftToRight and ((xx - x) / w) or (1 - (xx - x) / w)
		for yy = y, y + h do
			if core_dither(br, xx, yy, matrix) then
				pix(xx, yy, col)
			end
		end
	end
end
