-- title:   core_image
-- author:  Olympian / Spectrox
-- desc:    n/a
-- site:    https://github.com/olymp14n
-- license: CC0 1.0 Universal
-- version: 1.0
-- script:  lua

-------------------------------------------------------------------------------
--- IMAGE
-------------------------------------------------------------------------------

function core_image_new(w, h, palette, pixels)
	return {
		width = w,
		height = h,
		width_1 = w - 1,
		height_1 = h - 1,
		width_half = w // 2,
		height_half = h // 2,
		palette = palette,
		pixels = pixels
	}
end

function core_image_crop(image, x, y, w, h)
	local pixels = {}

	for iy = y, y + h - 1 do
		for ix = x, x + w - 1 do
			table.insert(pixels, image.pixels[ix + iy * image.width + 1])
		end
	end

	return core_image_new(w, h, image.palette, pixels)
end

function core_image_scale(image, s)
	local pixels = {}

	local sw = (image.width * s) // 1
	local sh = (image.height * s) // 1

	local sf = 1 / s

	for iy = 0, sh - 1 do
		local my = (sf * iy) // 1
		local stride = my * image.width + 1
		for ix = 0, sw - 1 do
			local mx = (sf * ix) // 1
			pixels[ix + iy * sw + 1] = image.pixels[stride + mx]
		end
	end

	return core_image_new(sw, sh, image.palette, pixels)
end

function core_image_get(image, x, y)
	return image.pixels[x + y * image.width + 1]
end

function core_image_from_canvas(x, y, w, h)
	local pixels = {}

	for iy = y, y + h - 1 do
		for ix = x, x + w - 1 do
			table.insert(pixels, pix(ix, iy))
		end
	end

	return core_image_new(w, h, nil, pixels)
end
