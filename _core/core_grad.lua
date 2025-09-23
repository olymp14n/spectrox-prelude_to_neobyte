-- title:   core_grad
-- author:  Olympian / Spectrox
-- desc:    n/a
-- site:    https://github.com/olymp14n
-- license: CC0 1.0 Universal
-- version: 1.0
-- script:  lua
-- requires: core_pal

-------------------------------------------------------------------------------
--- GRAD
-------------------------------------------------------------------------------

function core_grad_bdr(scanline, gradient, colorIdx)
	if scanline >= 4 and scanline <= 139 then
		local i = 1 + (scanline - 4) * 3
		core_pal_apply_color(colorIdx, gradient[i], gradient[i + 1], gradient[i + 2])
	end
end

function core_grad_bdr_offset(scanline, gradient, colorIdx, off)
	if scanline >= 4 and scanline <= 139 then
		local i = 1 + ((scanline - 4 - off) * 3) % #gradient
		core_pal_apply_color(colorIdx, gradient[i], gradient[i + 1], gradient[i + 2])
	end
end

--- Clear 1xh gradient constisting of RGB values.
function core_grad_clear(grad, r, g, b)
	for i = 1, #grad, 3 do
		grad[i] = r
		grad[i + 1] = g
		grad[i + 2] = b
	end
end

--- Set RGB color at position `y` of 1xh gradient.
function core_grad_pix(grad, y, r, g, b)
	if y < 0 or y >= #grad / 3 then
		return
	end
	local i = y * 3 + 1
	grad[i] = r
	grad[i + 1] = g
	grad[i + 2] = b
end

function core_grad_mix(src1, src2, dst, p)
	maxIdx = #src1 > #src2 and #src1 or #src2

	local v1, v2
	for i = 1, maxIdx do
		v1 = i > #src1 and 0 or src1[i]
		v2 = i > #src2 and 0 or src2[i]
		dst[i] = (v1 + p * (v2 - v1)) // 1
	end
end

function core_grad_new(h)
	local grad = {}
	for i = 1, h * 3 do
		grad[i] = 0
	end
	return grad
end
