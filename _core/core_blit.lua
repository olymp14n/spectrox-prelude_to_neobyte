-- title:   core_blit
-- author:  Olympian / Spectrox
-- desc:    n/a
-- site:    https://github.com/olymp14n
-- license: CC0 1.0 Universal
-- version: 1.0
-- script:  lua

-------------------------------------------------------------------------------
--- BLIT
-------------------------------------------------------------------------------

local max = math.max
local min = math.min

function core_blit_render(image, x, y, colorTransparent, colorOverride)
	local sx0 = max(0, -x)              -- Source X start.
	local sx1 = min(image.width, 240 - x) -- Source X end.
	local sy0 = max(0, -y)              -- Source Y start.
	local sy1 = min(image.height, 136 - y) -- Source Y end.

	if sx0 >= sx1 or sy0 >= sy1 then return end

	local tx, ty, idx, c
	for iy = sy0, sy1 - 1 do
		tx = x + sx0
		ty = y + iy
		idx = iy * image.width + sx0 + 1

		for ix = sx0, sx1 - 1 do
			c = image.pixels[idx]
			if c ~= colorTransparent then
				pix(tx, ty, colorOverride or c)
			end
			idx = idx + 1
			tx = tx + 1
		end
	end
end

function core_blit_scale(srcImg, srcX, srcY, srcW, srcH, dstX, dstY, dstW, dstH, colorTransparent, colorOverride)
	if (dstW > -1 and dstW < 1) or (dstH > -1 and dstH < 1) then
		return
	end

	local xs, xe, rx
	if dstW >= 0 then
		xs, xe = 0, (dstW - 1)
		rx = .5
	else
		xs, xe = (dstW + 1), 0
		rx = -.5
	end

	local ys, ye, ry
	if dstH >= 0 then
		ys, ye = 0, (dstH - 1)
		ry = .5
	else
		ys, ye = (dstH + 1), 0
		ry = -.5
	end

	local xf, yf = srcW / dstW, srcH / dstH
	local xo, yo = rx + srcX, ry + srcY

	local fy -- final y location.
	for y = ys, ye do
		fy = dstY + y
		if fy > 136 then
			goto continue
		end
		local my = (yo + yf * y) // 1
		local stride = my * srcImg.width + 1
		for x = xs, xe do
			local mx = (xo + xf * x) // 1
			local c = srcImg.pixels[stride + mx]
			if c ~= colorTransparent then
				pix(dstX + x, fy, colorOverride or c)
			end
		end
		::continue::
	end
end
