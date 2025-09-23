-- title:   PRELUDE TO NEOBYTE
-- author:  Olympian / Spectrox
-- desc:    TIC-80 demo.
-- site:    https://github.com/olymp14n
-- license: CC0 1.0 Universal
-- version: 1.0
-- script:  lua

-- title:   core_constants
-- author:  Olympian / Spectrox
-- desc:    n/a
-- site:    https://github.com/olymp14n
-- license: CC0 1.0 Universal
-- version: 1.0
-- script:  lua

-------------------------------------------------------------------------------
--- CONSTANTS
-------------------------------------------------------------------------------

CORE_WIDTH = 240
CORE_HEIGHT = 136
CORE_WIDTH_1 = 240 - 1
CORE_HEIGHT_1 = 136 - 1
CORE_WIDTH_HALF = CORE_WIDTH // 2
CORE_HEIGHT_HALF = CORE_HEIGHT // 2

CORE_ASPECT = CORE_WIDTH / CORE_HEIGHT

M_PI = math.pi
M_1_PI = 1 / math.pi
M_PI_2 = math.pi / 2
M_PI_4 = math.pi / 4
M_3PI_2 = 3 * math.pi / 2
M_TAU = math.pi * 2

X = 1
Y = 2
Z = 3
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
-- title:   core_timer
-- author:  Olympian / Spectrox
-- desc:    n/a
-- site:    https://github.com/olymp14n
-- license: CC0 1.0 Universal
-- version: 1.0
-- script:  lua

-------------------------------------------------------------------------------
--- TIMER
-------------------------------------------------------------------------------

local CORE_TIMER_GLOBAL_FRAME_COUNTER = { currentFrame = 0 }

--- Update frame count. Call at end of TIC() function.
function core_timer_update_global_frame_counter()
	CORE_TIMER_GLOBAL_FRAME_COUNTER.currentFrame = CORE_TIMER_GLOBAL_FRAME_COUNTER.currentFrame + 1
end

function core_timer_current_frame()
	return CORE_TIMER_GLOBAL_FRAME_COUNTER.currentFrame
end

function core_timer_create()
	return {}
end

--- Starts the timer. Can be called multiple times without side effects.
function core_timer_start(timer)
	if timer.startMillis == nil then
		timer.startMillis = core_timer_current_frame() / 60 * 1000
		timer.previousMillis = timer.startMillis
	end
end

function core_timer_update(timer)
	timer.currentMillis = core_timer_current_frame() / 60 * 1000
	timer.delta = (timer.currentMillis - timer.previousMillis) * .001
	timer.previousMillis = timer.currentMillis
	timer.elapsed = (timer.currentMillis - timer.startMillis) * .001
end

function core_timer_limiter_init(limiter)
	limiter.elapsed = 0
end

function core_timer_skip(limiter, fps, timer)
	if fps >= 60 then
		limiter.elapsed = 0
		return false
	end

	limiter.elapsed = limiter.elapsed + timer.delta
	if limiter.elapsed < 1 / fps then
		return true
	end

	limiter.elapsed = 0
	return false
end
-- title:   core_pal
-- author:  Olympian / Spectrox
-- desc:    n/a
-- site:    https://github.com/olymp14n
-- license: CC0 1.0 Universal
-- version: 1.0
-- script:  lua
-- references:
--   [1] https://github.com/nesbox/TIC-80/wiki/palette
--   [2] https://github.com/nesbox/TIC-80/wiki/Sample-RGB-Color-RAM-Address

-------------------------------------------------------------------------------
--- PAL
-------------------------------------------------------------------------------

local ADR_PALETTE = 0x3FC0
local ADR_PALETTE_MAP = 0x3FF0
local ADR_PALETTE_MAP_2 = ADR_PALETTE_MAP * 2

--- Apply color palette.
--- @param pal Palette of 16 colors. Array of 48 bytes in RGB layout.
function core_pal_apply(pal)
	for i = 1, #pal do
		poke(ADR_PALETTE + i - 1, pal[i])
	end
end

function core_pal_grab()
	local pal = {}
	for i = 1, 48 do
		pal[i] = peek(ADR_PALETTE + i - 1)
	end
	return pal
end

function core_pal_apply_color(index, r, g, b)
	local off = ADR_PALETTE + index * 3
	poke(off, r)
	poke(off + 1, g)
	poke(off + 2, b)
end

function core_pal_set_color(pal, index, r, g, b)
	local i    = (index - 1) * 3 + 1
	pal[i]     = r
	pal[i + 1] = g
	pal[i + 2] = b
end

function core_pal_swap(index, color)
	poke4(ADR_PALETTE_MAP_2 + index, color)
end

function core_pal_mix(srcPal1, srcPal2, dstPal, p)
	for i = 1, #srcPal1 do
		dstPal[i] = srcPal1[i] + p * (srcPal2[i] - srcPal1[i])
	end
end

function core_pal_apply_mix(srcPal1, srcPal2, p)
	p = p < 0 and 0 or p
	p = p > 1 and 1 or p
	local dstPal = {}
	core_pal_mix(srcPal1, srcPal2, dstPal, p)
	core_pal_apply(dstPal)
end

function core_pal_apply_white_fade(srcPal, p)
	p = p < 0 and 0 or p
	p = p > 1 and 1 or p
	local dstPal = {}
	for i = 1, #srcPal do
		dstPal[i] = srcPal[i] + p * (255 - srcPal[i])
	end
	core_pal_apply(dstPal)
end

function core_pal_apply_black_fade(srcPal, p)
	p = p < 0 and 0 or p
	p = p > 1 and 1 or p
	local dstPal = {}
	for i = 1, #srcPal do
		dstPal[i] = srcPal[i] * (1 - p)
	end
	core_pal_apply(dstPal)
end
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
-- title:   core_sprite
-- author:  Olympian / Spectrox
-- desc:    n/a
-- site:    https://github.com/olymp14n
-- license: CC0 1.0 Universal
-- version: 1.0
-- script:  lua

-------------------------------------------------------------------------------
--- SPRITE
-------------------------------------------------------------------------------

--- Define a sprite in a TIC-80 sprite sheet.
--- @param page any 0 or 1.
--- @param col any 0-based.
--- @param row any 0-based.
--- @param sprW any Composite sprite: number of horizontal 8x8 sprites.
--- @param sprH any Composite sprite: number of vertical 8x8 sprites.
--- @param colorTransparent any
--- @param imgW any Total height of (composite) sprites in pixels.
--- @param imgH any Total width of (composite) sprite in pixels.
--- @return table
function core_sprite_def(page, col, row, sprW, sprH, colorTransparent, imgW, imgH)
	return {
		idx = col + row * 16 + page * 256,
		col = col,
		row = row,
		sprW = sprW,
		sprH = sprH,
		bgCol = colorTransparent,
		imgW = imgW,
		imgH = imgH,
		imgW_1 = imgW - 1,
		imgH_1 = imgH - 1,
		imgW_half = imgW // 2,
		imgH_half = imgH // 2
	}
end

--- Transfer a 128x128 image to a sprite sheet page.
--- @param img any Image data.
--- @param page any Sprite sheet page (0 or 1).
function core_sprite_transfer_sheet(img, page)
	if img.width ~= 128 or img.height ~= 128 then
		error("Spritesheet dimension must be 128x128!")
	end
	local off = 256 * page
	for iy = 0, 15 do
		for ix = 0, 15 do
			core_sprite_transfer(img, ix * 8, iy * 8, off + ix + iy * 16) -- 16 is the number of sprites per row in TIC-80 memory.
		end
	end
end

function core_sprite_transfer(img, x, y, sprIdx)
	-- One sprite is 8x8 pixels. Every pixel is half a byte. So 8x8/2 = 32 = << 5.
	local sprAdr = 0x4000 + (sprIdx << 5)
	local off = 0
	local cl, cr
	for iy = y, y + 7 do
		for ix = x, x + 7, 2 do
			cl = core_image_get(img, ix, iy)
			cr = core_image_get(img, ix + 1, iy)
			poke(sprAdr + off, cr << 4 | cl)
			off = off + 1
		end
	end
end

--- Set a pixel in a 8x8 sprite.
--- There are 16x16 sprites on one sheet.
--- @param sprIdx any Sprite index. There are 8 sprites in the first row (indexes 0..15). The first sprite on the second sheet has index 16*16+0 = 256.
--- @param x any Relative X position within the sprite, so from 0..7
--- @param y any Relative Y position within the sprite, so from 0..7
--- @param color any Pixel color to set.
function core_sprite_pix(sprIdx, x, y, color)
	local sprAdr = 0x4000 + (sprIdx << 5) + (x >> 1) + (y << 2)
	local old = peek(sprAdr)
	poke(sprAdr, x & 1 == 0 and (old & 0xf0) | color or (old & 0x0f) | color << 4)
end
-- title:   core_math
-- author:  Olympian / Spectrox
-- desc:    n/a
-- site:    https://github.com/olymp14n
-- license: CC0 1.0 Universal
-- version: 1.0
-- script:  lua
-- references:
--   [1] https://en.wikipedia.org/wiki/Sawtooth_wave
--   [2] https://en.wikipedia.org/wiki/Triangle_wave

-------------------------------------------------------------------------------
--- MATH
-------------------------------------------------------------------------------

local abs = math.abs
local random = math.random

function core_math_randf(min, max)
	return min + (max - min) * random()
end

function core_math_lerp(src, dest, t)
	if t >= 1 then
		return dest
	end
	return src + t * (dest - src)
end

--- Sawtooth wave from -1 to +1 with periodicity of 1.
function core_math_sawtooth(t)
	-- https://en.wikipedia.org/wiki/Sawtooth_wave
	return 2 * (t % 1) - 1
end

-- Triangle wave.
function core_math_triangle(t)
	-- https://en.wikipedia.org/wiki/Triangle_wave
	return 4 * abs(t - ((t + .75) // 1) + .25) - 1
end

function core_math_clamp(v, min, max)
	if v >= max then return max end
	if v <= min then return min end
	return v
end

--- Triangle wave pulse.
--- @param cycleDuration any Length of a full cycle in seconds.
--- @param pulseDuration any Pulse duration that is executed at beginning of cycle.
--- @param t any timer
--- @return unknown value in [0,1]
function trianglePulse(cycleDuration, pulseDuration, timeElapsed)
	local pos = timeElapsed % cycleDuration

	if pos < pulseDuration then
		local n = pos / pulseDuration
		-- Inside pulse: ping-pong 0 → 1 → 0
		return 1 - math.abs(2 * n - 1)
	end

	return 0
end
-- title:   core_mat4
-- author:  Olympian / Spectrox
-- desc:    n/a
-- site:    https://github.com/olymp14n
-- license: CC0 1.0 Universal
-- version: 1.0
-- script:  lua
-- references:
--   [1] Cirno's Perfect Math Library https://github.com/excessive/cpml

-------------------------------------------------------------------------------
--- MAT4
-------------------------------------------------------------------------------

local rad = math.rad
local sin = math.sin
local cos = math.cos
local tan = math.tan

function core_mat4()
    return {
        0, 0, 0, 0, --
        0, 0, 0, 0, --
        0, 0, 0, 0, --
        0, 0, 0, 0  --
    }
end

function core_mat4_identity()
    return {
        1, 0, 0, 0, --
        0, 1, 0, 0, --
        0, 0, 1, 0, --
        0, 0, 0, 1  --
    }
end

function core_mat4_set_identity(mat)
    mat[1], mat[2], mat[3], mat[4] = 1, 0, 0, 0
    mat[5], mat[6], mat[7], mat[8] = 0, 1, 0, 0
    mat[9], mat[10], mat[11], mat[12] = 0, 0, 1, 0
    mat[13], mat[14], mat[15], mat[16] = 0, 0, 0, 1
end

local function core_mat4_sign(n)
    if n > 0 then
        return 1
    elseif n < 0 then
        return -1
    else
        return 0
    end
end

function core_mat4_perspective(fovy, aspect, near, far)
    local t = tan(rad(fovy) / 2)
    local out = core_mat4()
    out[1] = 1 / (t * aspect)
    out[6] = 1 / t
    out[11] = -(far + near) / (far - near)
    out[12] = -1
    out[15] = -(2 * far * near) / (far - near)
    out[16] = 0

    return out
end

function core_mat4_translate(x, y, z)
    local out = core_mat4_identity()
    out[13] = x
    out[14] = y
    out[15] = z
    return out
end

function core_mat4_scale(x, y, z)
    local out = core_mat4_identity()
    out[1] = x
    out[6] = y
    out[11] = z
    return out
end

function core_mat4_rot_x(angle)
    local c, s, out = cos(angle), sin(angle), core_mat4_identity()
    out[6] = c
    out[7] = s
    out[10] = -s
    out[11] = c
    return out
end

function core_mat4_rot_y(angle)
    local c, s, out = cos(angle), sin(angle), core_mat4_identity()
    out[1] = c
    out[3] = -s
    out[9] = s
    out[11] = c
    return out
end

function core_mat4_rot_z(angle)
    local c, s, out = cos(angle), sin(angle), core_mat4_identity()
    out[1] = c
    out[2] = -s
    out[5] = s
    out[6] = c
    return out
end

function core_mat4_mul_vec3(out, a, b)
    x = b[1] * a[1] + b[2] * a[5] + b[3] * a[9] + a[13]
    y = b[1] * a[2] + b[2] * a[6] + b[3] * a[10] + a[14]
    z = b[1] * a[3] + b[2] * a[7] + b[3] * a[11] + a[15]

    out[1] = x
    out[2] = y
    out[3] = z

    return out
end

function core_mat4_mul(out, a, b)
    local tmp = { 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0 }
    tmp[1]    = b[1] * a[1] + b[2] * a[5] + b[3] * a[9] + b[4] * a[13]
    tmp[2]    = b[1] * a[2] + b[2] * a[6] + b[3] * a[10] + b[4] * a[14]
    tmp[3]    = b[1] * a[3] + b[2] * a[7] + b[3] * a[11] + b[4] * a[15]
    tmp[4]    = b[1] * a[4] + b[2] * a[8] + b[3] * a[12] + b[4] * a[16]
    tmp[5]    = b[5] * a[1] + b[6] * a[5] + b[7] * a[9] + b[8] * a[13]
    tmp[6]    = b[5] * a[2] + b[6] * a[6] + b[7] * a[10] + b[8] * a[14]
    tmp[7]    = b[5] * a[3] + b[6] * a[7] + b[7] * a[11] + b[8] * a[15]
    tmp[8]    = b[5] * a[4] + b[6] * a[8] + b[7] * a[12] + b[8] * a[16]
    tmp[9]    = b[9] * a[1] + b[10] * a[5] + b[11] * a[9] + b[12] * a[13]
    tmp[10]   = b[9] * a[2] + b[10] * a[6] + b[11] * a[10] + b[12] * a[14]
    tmp[11]   = b[9] * a[3] + b[10] * a[7] + b[11] * a[11] + b[12] * a[15]
    tmp[12]   = b[9] * a[4] + b[10] * a[8] + b[11] * a[12] + b[12] * a[16]
    tmp[13]   = b[13] * a[1] + b[14] * a[5] + b[15] * a[9] + b[16] * a[13]
    tmp[14]   = b[13] * a[2] + b[14] * a[6] + b[15] * a[10] + b[16] * a[14]
    tmp[15]   = b[13] * a[3] + b[14] * a[7] + b[15] * a[11] + b[16] * a[15]
    tmp[16]   = b[13] * a[4] + b[14] * a[8] + b[15] * a[12] + b[16] * a[16]

    for i = 1, 16 do
        out[i] = tmp[i]
    end
end

function core_mat4_mul_vec3_perspective(out, a, b)
    x = b[1] * a[1] + b[2] * a[5] + b[3] * a[9] + a[13]
    y = b[1] * a[2] + b[2] * a[6] + b[3] * a[10] + a[14]
    z = b[1] * a[3] + b[2] * a[7] + b[3] * a[11] + a[15]
    w = b[1] * a[4] + b[2] * a[8] + b[3] * a[12] + a[16]

    local inv_w = 0
    if w ~= 0 then
        inv_w = core_mat4_sign(w) / w
    end

    -- Normalized Device Coordinates (NDC):
    -- Xndc = x' / w'
    -- Yndc = y' / w'
    -- Viewport transformation:
    --  Xscreen = WIDTH/2 * (1 + Xndc)
    --  Yscreen = HEIGHT/2 * (1 - Yndc)
    out[1] = CORE_WIDTH_HALF * (1 + x * inv_w)
    out[2] = CORE_HEIGHT_HALF * (1 - y * inv_w)

    return out
end

function core_mat4_projection()
    return core_mat4_perspective(45, CORE_ASPECT, 0, 100)
end
-- title:   core_anim
-- author:  Olympian / Spectrox
-- desc:    n/a
-- site:    https://github.com/olymp14n
-- license: CC0 1.0 Universal
-- version: 1.0
-- script:  lua

-------------------------------------------------------------------------------
--- ANIM
-------------------------------------------------------------------------------

local CoreAnimState = {
	INITIALIZED = 0,
	RUNNING = 1,
	COMPLETED = 2,
}

local CoreAnimKeyframeType = {
	TWEEN = 0,
	FIRE_ONCE = 1,
	NOP = 2,
}

function core_anim_process_keyframe_nop(keyframe, timeOffset, timer)
	local keyTimeStart = keyframe.timeStart + timeOffset

	if not keyframe.isCompleted and timer.elapsed >= keyTimeStart then
		keyframe.isCompleted = true
	end
end

function core_anim_process_keyframe_fire_once(keyframe, timeOffset, timer)
	local keyTimeStart = keyframe.timeStart + timeOffset

	if not keyframe.isCompleted and timer.elapsed >= keyTimeStart then
		keyframe.isCompleted = true
		keyframe.started_proc(timer)
	end
end

function core_anim_process_keyframe_tween(keyframe, timeOffset, timer)
	local keyTimeStart = keyframe.timeStart + timeOffset
	local keyTimeEnd = keyframe.timeEnd + timeOffset

	if (timer.elapsed >= keyTimeEnd) then
		for j = 1, #keyframe.value do
			keyframe.value[j] = keyframe.to[j]
		end
		keyframe.isCompleted = true
		goto continue
	end

	if (timer.elapsed >= keyTimeStart and timer.elapsed < keyTimeEnd) then
		local p = (timer.elapsed - keyTimeStart) / (keyTimeEnd - keyTimeStart)
		p = keyframe.easing_func(p)

		for j = 1, #keyframe.value do
			keyframe.value[j] = keyframe.from[j] + p * (keyframe.to[j] - keyframe.from[j])
		end
	end
	::continue::
end

function core_anim_process_keyframes(keyframes, timeOffset, timer)
	for i = 1, #keyframes do
		if keyframes[i].isCompleted then
			goto continue
		end

		if keyframes[i].type == CoreAnimKeyframeType.TWEEN then
			core_anim_process_keyframe_tween(keyframes[i], timeOffset, timer)
		elseif keyframes[i].type == CoreAnimKeyframeType.FIRE_ONCE then
			core_anim_process_keyframe_fire_once(keyframes[i], timeOffset, timer)
		else
			core_anim_process_keyframe_nop(keyframes[i], timeOffset, timer)
		end
		::continue::
	end
end

function core_anim_reset_keyframes(keyframes)
	for i = 1, #keyframes do
		keyframes[i].isCompleted = false
	end
end

function core_anim_start(anim, timer)
	core_anim_reset_keyframes(anim.keyframes)

	anim.state = CoreAnimState.RUNNING
	anim.timeStartedAt = timer.elapsed

	if anim.started_proc then
		anim.started_proc(anim, timer)
	end
end

function core_anim_process(anim, timer)
	if (anim.isAutostart and anim.state == CoreAnimState.INITIALIZED) then
		core_anim_start(anim, timer)
	end

	if (anim.state ~= CoreAnimState.RUNNING) then
		return
	end

	core_anim_process_keyframes(anim.keyframes, anim.timeStartedAt, timer)
end

function core_anim_is_completed(anim)
	for i = 1, #anim.keyframes do
		if not anim.keyframes[i].isCompleted then
			return false
		end
	end
	return true
end

function core_anim_handle_completed(anim, timer)
	if (anim.state == CoreAnimState.RUNNING and core_anim_is_completed(anim)) then
		anim.state = CoreAnimState.COMPLETED

		if anim.completed_proc then
			anim.completed_proc(anim, timer)
		end
	end
end

function core_anim_process_schedule(schedule, timer)
	for i = 1, #schedule.anims do
		core_anim_process(schedule.anims[i], timer)
		core_anim_handle_completed(schedule.anims[i], timer)
	end

	-- Make sure anims are being processed that have been started by an end listener in a previous anim.
	for i = 1, #schedule.anims do
		core_anim_process(schedule.anims[i], timer)
	end
end

function core_anim_new_keyframe_tween(timeStart, timeEnd, from, to, easing_func, value)
	return {
		type = CoreAnimKeyframeType.TWEEN,
		timeStart = timeStart,
		timeEnd = timeEnd,
		from = from,
		to = to,
		easing_func = easing_func,
		value = value,
	}
end

function core_anim_new_keyframe_fire_once(timeStart, started_proc)
	return {
		type = CoreAnimKeyframeType.FIRE_ONCE,
		timeStart = timeStart,
		started_proc = started_proc,
	}
end

function core_anim_new_keyframe_nop(timeStart, started_proc)
	return {
		type = CoreAnimKeyframeType.NOP,
		timeStart = timeStart,
		started_proc = started_proc,
	}
end

function core_anim_new(isAutostart, keyframes, started_proc, completed_proc)
	return {
		isAutostart = isAutostart,
		keyframes = keyframes,
		state = CoreAnimState.INITIALIZED,
		started_proc = started_proc,
		completed_proc = completed_proc,
	}
end

function core_anim_new_schedule()
	return {
		anims = {}
	}
end

--- Adds a new anim to the schedule.
--- @param name Must be a integer value. Must result in sequential numeric indices starting at 1.
function core_anim_add_anim(name, schedule, anim)
	schedule.anims[name] = anim
end

function core_anim_is_running(schedule, animName)
	return schedule.anims[animName].state == CoreAnimState.RUNNING
end

function core_anim_update_local_timer(schedule, animName, localTimer, globalTimer)
	localTimer.startMillis = globalTimer.startMillis
	localTimer.currentMillis = globalTimer.currentMillis
	localTimer.delta = globalTimer.delta
	localTimer.previousMillis = globalTimer.previousMillis
	localTimer.elapsed = globalTimer.elapsed - schedule.anims[animName].timeStartedAt
end
-- title:   core_easing
-- author:  Olympian / Spectrox
-- desc:    n/a
-- site:    https://github.com/olymp14n
-- license: CC0 1.0 Universal
-- version: 1.0
-- script:  lua
-- references:
--   [1] https://github.com/warrenm/AHEasing

-------------------------------------------------------------------------------
--- EASING
-------------------------------------------------------------------------------

local sin = math.sin
local cos = math.cos
local pow = math.pow

--- Modeled after the line y = x
function core_easing_lerp(p)
	return p
end

--- Modeled after the parabola y = x^2.
function core_easing_quadratic_in(p)
	return p * p
end

--- Modeled after the parabola y = -x^2 + 2x.
function core_easing_quadratic_out(p)
	return -(p * (p - 2))
end

--- Modeled after the piecewise quadratic
--- y = (1/2)((2x)^2)             ; [0, 0.5)
--- y = -(1/2)((2x-1)*(2x-3) - 1) ; [0.5, 1]
function core_easing_quadratic_in_out(p)
	if p < .5 then
		return 2 * p * p
	else
		return (-2 * p * p) + (4 * p) - 1
	end
end

--- Modeled after the overshooting cubic y = x^3-x*sin(x*pi)
function core_easing_back_in(p)
	return p * p * p - p * sin(p * M_PI)
end

--- Modeled after overshooting cubic y = 1-((1-x)^3-(1-x)*sin((1-x)*pi))
function core_easing_back_out(p)
	local f = (1 - p)
	return 1 - (f * f * f - f * sin(f * M_PI))
end

--- Modeled after the piecewise overshooting cubic function:
--- y = (1/2)*((2x)^3-(2x)*sin(2*x*pi))           ; [0, 0.5)
--- y = (1/2)*(1-((1-x)^3-(1-x)*sin((1-x)*pi))+1) ; [0.5, 1]
function core_easing_back_in_out(p)
	if p < .5 then
		local f = 2 * p
		return .5 * (f * f * f - f * sin(f * M_PI))
	else
		f = (1 - (2 * p - 1));
		return .5 * (1 - (f * f * f - f * sin(f * M_PI))) + .5
	end
end

function core_easing_bounce_in(p)
	return 1 - core_easing_bounce_out(1 - p)
end

function core_easing_bounce_out(p)
	if p < 4 / 11 then
		return (121 * p * p) / 16
	elseif p < 8 / 11 then
		return (363 / 40 * p * p) - (99 / 10 * p) + 17 / 5
	elseif p < 9 / 10 then
		return (4356 / 361 * p * p) - (35442 / 1805 * p) + 16061 / 1805
	else
		return (54 / 5.0 * p * p) - (513 / 25 * p) + 268 / 25
	end
end

--- Bounce only once.
function core_easing_bounce_once_out(p)
	if p < 4 / 11 then
		return (121 * p * p) / 16
	elseif p < 8 / 11 then
		return (363 / 40 * p * p) - (99 / 10 * p) + 17 / 5
	else
		return 1
	end
end

function core_easing_bounce_in_out(p)
	if p < .5 then
		return .5 * core_easing_bounce_in(p * 2)
	else
		return .5 * core_easing_bounce_out(p * 2 - 1) + .5
	end
end

--- Modeled after the damped sine wave y = sin(13pi/2*x)*pow(2, 10 * (x - 1))
function core_easing_elastic_in(p)
	return sin(13 * M_PI_2 * p) * pow(2, 10 * (p - 1))
end

--- Modeled after the damped sine wave y = sin(-13pi/2*(x + 1))*pow(2, -10x) + 1
function core_easing_elastic_out(p)
	return sin(-13 * M_PI_2 * (p + 1)) * pow(2, -10 * p) + 1
end

--- Modeled after the piecewise exponentially-damped sine wave:
--- y = (1/2)*sin(13pi/2*(2*x))*pow(2, 10 * ((2*x) - 1))      ; [0,0.5)
--- y = (1/2)*(sin(-13pi/2*((2x-1)+1))*pow(2,-10(2*x-1)) + 2) ; [0.5, 1]
function core_easing_elastic_in_out(p)
	if p < .5 then
		return .5 * sin(13 * M_PI_2 * (2 * p)) * pow(2, 10 * ((2 * p) - 1))
	else
		return .5 * (sin(-13 * M_PI_2 * ((2 * p - 1) + 1)) * pow(2, -10 * (2 * p - 1)) + 2)
	end
end

--- Modeled after quarter-cycle of sine wave
function core_easing_sine_in(p)
	return sin((p - 1) * M_PI_2) + 1
end

--- Modeled after quarter-cycle of sine wave (different phase)
function core_easing_sine_out(p)
	return sin(p * M_PI_2)
end

--- Modeled after half sine wave
function core_easing_sine_in_out(p)
	return 0.5 * (1 - cos(p * M_PI))
end

--- Modeled after the cubic y = x^3
function core_easing_cubic_in(p)
	return p * p * p
end

--- Modeled after the cubic y = (x - 1)^3 + 1
function core_easing_cubic_out(p)
	local f = (p - 1)
	return f * f * f + 1
end

--- Modeled after the piecewise cubic
--- y = (1/2)((2x)^3)       ; [0, 0.5)
--- y = (1/2)((2x-2)^3 + 2) ; [0.5, 1]
function core_easing_cubic_in_out(p)
	if p < 0.5 then
		return 4 * p * p * p
	else
		local f = ((2 * p) - 2)
		return 0.5 * f * f * f + 1
	end
end

--- Modeled after the exponential function y = 2^(10(x - 1))
function core_easing_exponential_in(p)
	return (p == 0) and p or pow(2, 10 * (p - 1))
end

--- Modeled after the exponential function y = -2^(-10x) + 1
function core_easing_exponential_out(p)
	return (p == 1) and p or 1 - pow(2, -10 * p)
end

--- Modeled after the piecewise exponential
--- y = (1/2)2^(10(2x - 1))         ; [0,0.5)
--- y = -(1/2)*2^(-10(2x - 1))) + 1 ; [0.5,1]
function core_easing_exponential_in_out(p)
	if p == 0 or p == 1 then
		return p
	end

	if p < .5 then
		return 0.5 * pow(2, (20 * p) - 10)
	else
		return -0.5 * pow(2, (-20 * p) + 10) + 1;
	end
end
-- title:   core_sound
-- author:  Olympian / Spectrox
-- desc:    n/a
-- site:    https://github.com/olymp14n
-- license: CC0 1.0 Universal
-- version: 1.0
-- script:  lua
-- references:
--   [1] "Sound State" in https://github.com/nesbox/TIC-80/wiki/RAM

-------------------------------------------------------------------------------
--- SOUND
-------------------------------------------------------------------------------

local ADR_SOUND_STATE_TRACK = 0x13FFC
local ADR_SOUND_STATE_PATTERN = 0x13FFD
local ADR_SOUND_STATE_ROW = 0x13FFE

function core_sound_create_soundstate()
	return {
		track = -1,
		pattern = -1,
		row = -1,
		rowFired = -1
	}
end

function core_sound_update(soundState)
	soundState.track = peek(ADR_SOUND_STATE_TRACK)
	soundState.pattern = peek(ADR_SOUND_STATE_PATTERN)
	soundState.row = peek(ADR_SOUND_STATE_ROW)
end

--- The same row is active for multiple frames.
--- Make sure to only trigger actions once per row.
function core_sound_fire(soundState)
	soundState.rowFired = soundState.row
end

function core_sound_not_fired(soundState)
	return soundState.rowFired ~= soundState.row
end

--- Sets up the track list to be played.
---@param tracks any List of tracks to be played in order.
---					 STRUCTURE:
---                  	bank: The bank number starting at 0
---                  	track: The track number starting at 0
---                  	pattern: Number of the pattern to play. -1 for first pattern.
---                  	rowFired: Number of the row to play. -1 for first row.
---                  	lastPattern: This is the last pattern of the track. -1 for default.
---                  	lastRow: This is the last row of the lastPattern of the track. After this the track will be changed. -1 for default.
--- 				 EXAMPLE:
---						{bank = 0, track = 1, pattern = -1, row = -1, lastPattern = -1, lastRow = -1}
---@return table
function core_sound_create_tracklist(tracks)
	return {
		current = 0,
		tracks = tracks
	}
end

function core_sound_play(tracklist, isLooped)
	-- See "Sound State" in https://github.com/nesbox/TIC-80/wiki/RAM
	if peek(ADR_SOUND_STATE_TRACK) == 255 or
		(
			peek(ADR_SOUND_STATE_PATTERN) == tracklist.tracks[tracklist.current].lastPattern
			and peek(ADR_SOUND_STATE_ROW) > tracklist.tracks[tracklist.current].lastRow
		)

	then
		-- No track is playing (255) at the moment.
		-- Look up if a new track has to be started in the tracklist.

		if isLooped then
			tracklist.current = tracklist.current % #tracklist.tracks + 1
			sync(16, tracklist.tracks[tracklist.current].bank)
			music(tracklist.tracks[tracklist.current].track, tracklist.tracks[tracklist.current].pattern,
				tracklist.tracks[tracklist.current].row, false)
		else
			tracklist.current = tracklist.current + 1
			if (tracklist.current <= #tracklist.tracks) then
				sync(16, tracklist.tracks[tracklist.current].bank)
				music(tracklist.tracks[tracklist.current].track, tracklist.tracks[tracklist.current].pattern,
					tracklist.tracks[tracklist.current].row, false)
			end
		end
	end
end

function core_sound_stop()
	music()
end
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

RES_PCX_FONT =
{
  10,5,1,4,0,0,0,0,127,0,127,0,0,0,0,0,
  0,0,0,255,0,255,255,0,255,255,0,255,255,0,255,255,
  0,255,255,0,255,255,0,255,255,0,255,255,0,255,255,0,
  255,255,0,255,255,0,255,255,0,255,255,0,255,255,0,255,
  0,1,64,0,1,0,0,0,0,0,0,0,0,0,0,0,
  0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,
  0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,
  0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,
  255,0,0,255,0,0,255,0,0,255,0,0,255,0,0,255,
  0,0,255,0,0,255,0,0,255,0,0,255,0,0,255,0,
  0,255,0,0,255,0,0,255,0,0,255,0,0,255,0,0,
  196,0,193,255,204,0,194,240,194,0,194,240,194,0,193,255,
  194,0,193,255,195,0,15,196,0,193,240,216,0,15,0,196,
  0,193,255,203,0,195,15,0,15,194,255,0,15,193,255,193,
  240,0,15,195,0,193,240,196,0,15,216,0,193,255,0,196,
  0,193,255,203,0,15,0,15,0,15,194,255,0,15,193,255,
  193,240,197,0,193,240,196,0,15,195,0,15,203,0,193,255,
  193,240,198,0,15,193,240,0,209,0,194,240,194,0,193,255,
  193,240,194,0,193,255,198,0,193,240,196,0,15,215,0,193,
  255,194,0,196,0,193,255,204,0,15,195,0,15,202,0,15,
  196,0,193,240,202,0,193,240,199,0,193,240,196,0,193,240,
  194,0,240,0,193,240,207,0,255,0,0,255,0,0,195,255,
  193,240,15,193,255,194,0,195,255,193,240,195,255,194,240,194,
  0,193,240,195,255,193,240,195,255,193,240,195,255,193,240,195,
  255,193,240,195,255,193,240,212,0,15,194,255,193,240,193,240,
  194,0,193,240,0,15,197,0,193,240,195,0,194,240,194,0,
  194,240,195,0,193,240,198,0,194,240,194,0,194,240,194,0,
  193,240,0,15,193,240,209,0,15,194,0,193,240,193,240,194,
  0,193,240,0,15,194,0,195,255,193,240,15,194,255,193,240,
  195,255,193,240,195,255,193,240,195,255,193,240,195,0,193,240,
  195,255,193,240,195,255,193,240,213,0,15,193,255,193,240,193,
  240,194,0,193,240,0,15,194,0,193,240,198,0,193,240,195,
  0,193,240,195,0,194,240,194,0,193,240,195,0,194,240,194,
  0,193,240,195,0,193,240,0,15,193,240,210,0,15,194,0,
  195,255,193,240,195,255,193,240,195,255,193,240,195,255,193,240,
  195,0,193,240,195,255,193,240,195,255,193,240,195,0,193,240,
  195,255,193,240,195,255,193,240,216,0,253,0,15,194,0,255,
  0,0,255,0,0,196,0,195,255,193,240,195,255,0,195,255,
  0,195,255,0,195,255,0,195,255,193,240,195,255,0,193,255,
  0,15,193,240,193,255,197,0,15,193,240,193,255,0,193,255,
  0,193,255,195,0,195,255,193,240,195,255,193,240,195,255,193,
  240,0,15,193,240,0,193,255,0,15,193,240,193,255,0,193,
  255,0,193,255,195,0,193,255,0,15,193,240,193,255,195,0,
  193,255,195,0,193,255,195,0,193,255,0,15,193,240,193,255,
  197,0,15,193,240,193,255,0,193,255,0,193,255,195,0,193,
  255,194,15,193,240,193,255,0,15,193,240,193,255,0,15,193,
  240,0,15,193,240,0,195,255,193,240,195,255,193,240,193,255,
  195,0,193,255,0,15,193,240,194,255,193,240,0,194,255,193,
  240,0,193,255,15,193,255,193,240,195,255,193,240,193,255,195,
  0,193,255,0,15,193,240,195,255,193,240,193,255,195,0,193,
  255,194,15,193,240,193,255,0,15,193,240,193,255,0,15,193,
  240,196,0,193,255,0,15,193,240,193,255,0,15,193,240,193,
  255,195,0,193,255,0,15,193,240,193,255,195,0,193,255,195,
  0,193,255,0,15,193,240,193,255,0,15,193,240,193,255,195,
  0,193,255,0,15,193,240,193,255,0,15,193,240,193,255,195,
  0,193,255,0,15,193,240,193,255,0,15,193,240,193,255,0,
  15,193,240,196,0,193,255,0,15,193,240,195,255,193,240,195,
  255,193,240,195,255,193,240,195,255,193,240,193,255,195,0,195,
  255,193,240,193,255,0,15,193,240,193,255,195,0,195,255,193,
  240,193,255,0,15,193,240,195,255,193,240,193,255,0,15,193,
  240,193,255,0,15,193,240,195,255,193,240,255,0,0,255,0,
  0,255,0,0,195,255,193,240,195,255,193,240,195,255,0,195,
  255,0,195,255,0,193,255,0,15,193,240,193,255,0,15,193,
  240,193,255,0,15,193,240,193,255,0,15,193,240,193,255,0,
  15,193,240,15,194,255,193,240,212,0,193,255,0,15,193,240,
  193,255,0,15,193,240,193,255,0,193,255,0,193,255,196,0,
  193,255,194,0,193,255,0,15,193,240,193,255,0,15,193,240,
  193,255,0,15,193,240,193,255,0,15,193,240,193,255,0,15,
  193,240,194,0,15,193,240,212,0,195,255,193,240,193,255,0,
  15,193,240,195,255,193,240,195,255,193,240,0,193,255,194,0,
  193,255,0,15,193,240,193,255,0,15,193,240,193,255,194,15,
  193,240,15,194,255,0,195,255,193,240,195,255,193,240,212,0,
  193,255,195,0,193,255,194,15,193,240,193,255,0,15,193,240,
  194,0,15,193,240,0,193,255,194,0,193,255,0,15,193,240,
  193,255,0,15,193,240,193,255,194,15,193,240,193,255,0,15,
  193,240,194,0,15,193,240,193,255,215,0,193,255,195,0,195,
  255,193,240,193,255,0,15,193,240,195,255,193,240,0,193,255,
  194,0,195,255,193,240,195,255,0,195,255,193,240,193,255,0,
  15,193,240,15,194,255,193,240,195,255,193,240,212,0,255,0,
  0,255,0,0,255,0,0,196,0,195,255,193,240,195,255,193,
  240,195,255,193,240,195,255,193,240,195,255,193,240,195,255,193,
  240,195,255,194,240,194,0,193,240,195,255,193,240,195,0,194,
  240,0,15,0,193,240,195,0,195,255,193,240,195,255,193,240,
  195,255,193,240,196,0,193,240,194,0,194,240,194,0,194,240,
  195,0,193,240,194,0,194,240,195,0,193,240,195,0,193,240,
  195,0,193,240,194,0,193,240,0,15,197,0,194,240,0,15,
  0,193,240,195,0,193,240,15,0,194,240,194,0,194,240,194,
  0,193,240,196,0,195,255,193,240,195,255,0,193,240,195,0,
  193,240,194,0,193,240,194,255,193,240,0,194,255,193,240,0,
  193,240,194,255,193,240,195,255,193,240,0,15,197,0,193,240,
  195,255,194,240,195,0,193,240,15,0,194,240,194,0,194,240,
  194,0,193,240,196,0,193,240,194,0,194,240,194,0,194,240,
  195,0,193,240,194,0,194,240,195,0,193,240,195,0,193,240,
  194,0,194,240,194,0,193,240,0,15,194,0,193,240,194,0,
  194,240,194,0,194,240,195,0,193,240,15,0,194,240,194,0,
  194,240,194,0,193,240,196,0,193,240,194,0,193,240,195,255,
  193,240,195,255,193,240,195,255,0,195,255,194,240,195,0,195,
  255,194,240,194,0,193,240,195,255,193,240,195,255,194,240,194,
  0,193,240,195,255,194,240,194,0,194,240,194,0,193,240,195,
  255,193,240,255,0,0,255,0,0,255,0,0,195,255,193,240,
  195,255,193,240,195,255,193,240,195,255,193,240,195,255,194,240,
  194,0,194,240,194,0,194,240,194,0,193,240,15,0,15,0,
  193,240,194,0,193,240,195,255,193,240,212,0,193,240,194,0,
  194,240,194,0,194,240,194,0,194,240,196,0,15,194,0,193,
  240,194,0,194,240,194,0,194,240,15,0,193,240,15,0,15,
  0,193,240,194,0,193,240,195,0,193,240,212,0,195,255,194,
  240,194,0,193,240,195,255,0,195,255,193,240,0,15,194,0,
  193,240,194,0,194,240,194,0,194,240,15,0,193,240,195,255,
  193,240,195,255,193,240,195,255,193,240,212,0,193,240,195,0,
  193,240,0,195,240,194,0,193,240,195,0,193,240,0,15,194,
  0,193,240,194,0,194,240,194,0,194,240,15,0,194,240,194,
  0,193,240,0,15,194,0,193,240,215,0,193,240,195,0,195,
  255,194,240,194,0,193,240,195,255,193,240,0,15,194,0,195,
  255,193,240,195,255,0,195,255,194,240,194,0,193,240,0,15,
  194,0,195,255,193,240,212,0,255,0,0,255,0,0,255,0,
  0,255,0,0,255,0,0,255,0,0,255,0,0,255,0,0,
  255,0,0,255,0,0,255,0,0,255,0,0,255,0,0,255,
  0,0,255,0,0,255,0,0,255,0,0,255,0,0,255,0,
  0,255,0,0,255,0,0,255,0,0,255,0,0,255,0,0,
  255,0,0,255,0,0,255,0,0,255,0,0,255,0,0,255,
  0,0,255,0,0,255,0,0,255,0,0,255,0,0,255,0,
  0,255,0,0,255,0,0,255,0,0,255,0,0,255,0,0,
  255,0,0,255,0,0,255,0,0,255,0,0,255,0,0,255,
  0,0,255,0,0,255,0,0,255,0,0,255,0,0,255,0,
  0,255,0,0,255,0,0,255,0,0,255,0,0,255,0,0,
  255,0,0,255,0,0,255,0,0,255,0,0,255,0,0,255,
  0,0,255,0,0,255,0,0,255,0,0,255,0,0,255,0,
  0
}

RES_PCX_PRELUDE =
{
  10,5,1,4,0,0,0,0,187,0,18,0,0,0,0,0,
  0,0,0,21,21,41,42,42,83,64,64,102,85,85,122,99,
  40,75,198,81,151,210,106,158,223,132,165,226,158,182,229,184,
  199,242,219,227,0,0,0,0,0,0,0,0,0,0,0,0,
  0,1,94,0,1,0,0,0,0,0,0,0,0,0,0,0,
  0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,
  0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,
  0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,
  169,136,119,194,118,194,102,103,119,194,120,137,23,194,118,196,
  102,104,102,137,6,196,102,103,119,194,118,194,104,120,144,198,
  0,6,103,137,197,0,197,102,118,194,102,103,102,135,137,6,
  194,102,103,102,103,119,194,120,144,196,0,22,194,102,195,119,
  118,136,104,144,0,5,135,194,102,194,118,134,137,148,16,201,
  0,8,70,65,199,0,8,70,203,0,128,198,0,6,0,8,
  197,0,96,202,0,8,6,200,0,128,195,0,1,70,199,0,
  20,128,0,89,197,0,1,72,129,51,34,50,199,34,194,38,
  18,35,50,51,50,195,34,54,38,50,51,201,34,112,198,0,
  6,50,39,197,0,99,194,50,200,34,38,6,51,194,50,197,
  34,96,195,0,4,54,51,35,194,34,194,35,34,65,96,5,
  162,196,34,35,36,22,112,49,201,0,7,6,3,16,198,0,
  194,6,203,0,128,198,0,6,0,6,197,0,96,202,0,194,
  6,200,0,96,195,0,3,6,199,0,32,96,89,198,0,2,
  6,195,102,118,196,102,194,103,16,194,6,199,102,16,194,6,
  201,102,96,0,96,198,0,6,0,7,197,0,96,0,198,102,
  150,153,0,194,6,200,102,96,195,0,3,22,194,102,96,194,
  0,22,194,102,97,128,0,5,194,102,134,138,3,6,20,48,
  199,0,6,65,6,2,67,197,0,6,65,6,3,201,0,96,
  0,96,198,0,6,0,6,197,0,96,0,96,198,0,6,0,
  6,3,200,0,48,195,0,3,65,0,20,96,0,1,70,1,
  52,20,112,0,86,16,0,1,72,2,6,1,67,56,137,154,
  194,50,34,51,55,52,6,8,136,153,19,195,51,25,52,194,
  7,198,119,194,121,163,112,0,112,198,0,7,0,7,197,0,
  112,0,115,194,119,115,194,51,67,73,0,194,7,199,119,151,
  160,195,0,4,194,51,65,96,0,4,42,35,65,33,96,1,
  97,35,51,36,22,2,6,194,0,8,65,9,195,0,1,165,
  3,7,8,65,9,65,194,0,1,149,19,194,7,200,0,144,
  112,0,112,198,0,7,0,7,197,0,112,0,112,114,34,112,
  195,0,7,0,194,7,200,0,144,198,0,32,112,0,2,7,
  194,0,32,112,19,112,194,0,2,7,2,7,194,0,7,19,
  57,51,194,0,25,80,19,194,7,19,56,52,16,0,23,80,
  20,87,7,198,34,194,36,194,112,0,112,198,0,7,0,7,
  197,0,112,0,194,112,0,112,195,0,7,0,194,7,199,34,
  50,112,198,0,32,112,0,2,9,194,0,48,112,48,112,194,
  0,3,7,2,7,194,0,8,2,24,194,135,119,117,1,33,
  133,24,2,8,194,135,119,117,1,69,113,7,200,0,194,112,
  0,112,198,0,7,0,7,197,0,112,0,194,112,0,112,195,
  0,7,0,194,7,200,0,112,198,0,32,128,0,2,8,194,
  0,32,128,32,128,194,0,2,8,2,8,194,0,7,2,0,
  2,195,34,33,9,81,39,2,0,1,195,34,33,118,16,39,
  0,7,194,119,120,195,136,194,128,0,128,198,0,8,0,8,
  197,0,112,0,194,112,0,112,195,0,88,0,194,8,0,8,
  198,136,128,198,0,32,128,0,2,8,194,0,32,128,32,128,
  194,0,2,24,2,8,194,0,7,2,197,0,16,133,20,23,
  2,198,0,26,98,7,0,8,198,0,32,128,0,128,198,0,
  8,0,8,197,0,128,0,194,128,0,128,194,0,5,130,0,
  194,8,0,8,198,0,32,198,0,32,128,0,2,8,194,0,
  32,144,32,128,194,0,1,133,2,9,194,0,7,2,7,194,
  120,119,102,119,81,65,8,2,8,135,102,103,133,0,1,149,
  8,0,8,196,136,194,137,144,128,0,195,136,194,137,194,136,
  8,0,8,195,136,137,136,32,0,194,128,0,196,136,18,0,
  194,8,0,8,137,136,137,194,153,137,136,194,137,160,195,0,
  32,128,0,2,8,194,0,32,128,33,196,136,80,2,9,194,
  0,7,2,8,64,196,0,18,16,8,2,8,64,194,0,88,
  80,0,24,8,0,2,198,0,194,128,0,32,197,0,194,8,
  0,2,196,0,4,32,5,194,128,0,32,194,0,4,33,0,
  88,8,0,2,201,0,144,195,0,32,128,0,2,8,194,0,
  48,144,36,16,194,0,33,0,4,149,194,0,8,2,8,3,
  196,34,33,0,8,2,8,4,34,35,33,133,0,8,72,0,
  2,196,34,194,36,194,128,0,195,34,194,35,34,40,8,0,
  2,34,194,51,50,35,16,90,32,128,0,195,34,35,16,5,
  146,8,0,2,35,34,35,194,51,35,34,35,34,128,195,0,
  32,128,0,2,8,194,0,32,144,66,195,34,16,0,25,80,
  194,0,8,2,8,2,198,0,8,2,8,2,195,0,24,80,
  194,8,200,0,194,128,199,0,194,8,199,0,5,145,32,128,
  198,0,89,18,8,203,0,128,195,0,32,128,0,2,8,194,
  0,48,144,197,0,1,133,0,194,0,8,194,136,18,198,0,
  8,194,136,18,195,0,1,194,136,24,200,136,128,201,136,8,
  194,136,153,194,136,137,194,136,18,16,136,194,137,194,153,152,
  137,129,33,8,203,136,128,195,0,33,194,136,194,137,194,0,
  49,194,136,194,137,153,152,136,80,0,194,0,1,66,1,66,
  198,0,1,66,1,66,196,0,20,17,67,200,0,48,32,199,
  0,194,3,199,0,4,33,0,48,198,0,66,16,3,203,0,
  32,195,0,36,16,0,2,65,194,0,52,16,197,0,49,194,
  0,195,0,19,34,35,199,0,19,34,35,196,0,1,50,35,
  200,34,48,50,199,34,52,3,194,34,50,194,51,50,34,35,
  16,0,50,196,34,194,35,49,0,3,203,34,48,195,0,50,
  194,34,35,16,194,0,66,34,194,35,50,34,35,16,194,0
}

RES_PCX_NEOBYTE =
{
  10,5,1,4,0,0,0,0,219,0,33,0,0,0,0,0,
  0,0,0,21,21,41,42,42,83,64,64,102,85,85,122,99,
  40,75,198,81,151,210,106,158,223,132,165,226,158,182,229,184,
  199,242,219,227,0,0,0,0,0,0,0,0,0,0,0,0,
  0,1,110,0,1,0,0,0,0,0,0,0,0,0,0,0,
  0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,
  0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,
  0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,
  120,154,152,118,102,118,194,119,195,136,16,196,0,169,135,198,
  102,118,119,103,136,153,154,16,196,0,23,119,120,135,120,136,
  153,154,153,136,135,119,103,102,118,195,102,194,119,136,119,136,
  137,153,160,10,152,118,194,102,197,0,102,120,137,153,160,26,
  152,118,102,194,118,103,119,120,135,120,136,153,154,1,169,135,
  102,194,119,136,119,136,137,153,160,200,0,99,16,200,0,5,
  129,196,0,99,16,203,0,6,65,195,0,1,133,216,0,96,
  6,195,0,6,197,0,96,195,0,145,70,203,0,1,73,20,
  96,200,0,19,96,200,0,97,65,201,0,89,16,195,0,97,
  65,203,0,6,20,16,194,0,24,83,216,0,96,6,195,0,
  6,197,0,96,195,0,132,22,203,0,20,24,65,96,199,0,
  1,65,96,200,0,96,20,194,67,197,51,194,50,53,161,195,
  0,96,20,202,51,35,38,50,32,0,1,149,4,194,67,211,
  51,35,34,35,96,6,194,51,35,38,197,0,99,194,50,34,
  131,54,201,51,194,50,65,7,51,99,197,51,194,50,36,16,
  96,200,0,96,3,202,0,90,16,194,0,96,3,203,0,6,
  0,32,0,26,80,49,216,0,96,6,195,0,6,197,0,96,
  195,0,128,6,203,0,64,7,0,96,199,0,3,0,96,200,
  0,96,4,202,0,5,145,194,0,96,3,203,0,6,0,48,
  1,165,2,16,216,0,96,6,195,0,6,197,0,96,195,0,
  112,6,203,0,48,6,0,96,199,0,2,0,96,200,0,96,
  3,194,0,102,119,136,153,162,196,0,88,16,0,206,102,16,
  32,25,80,33,194,0,90,153,136,135,119,103,102,118,199,102,
  103,136,153,154,195,0,5,112,6,195,0,6,197,0,96,195,
  0,112,22,196,102,96,194,0,1,197,102,1,202,102,96,200,
  0,96,4,194,0,99,16,194,0,90,32,195,0,5,129,194,
  0,3,203,0,1,194,49,133,2,16,0,5,163,209,0,6,
  195,0,89,48,6,195,0,6,197,0,96,195,0,113,65,195,
  0,18,96,194,0,18,96,194,0,49,65,19,16,199,0,3,
  202,0,96,3,194,0,97,49,194,0,5,162,196,0,130,16,
  0,3,204,0,18,22,80,33,194,0,89,35,195,0,6,196,
  102,0,196,102,96,195,0,6,194,0,5,145,48,6,195,0,
  6,103,137,169,135,118,96,195,0,100,16,194,0,1,33,96,
  0,1,33,96,194,0,52,17,49,200,0,3,202,0,96,3,
  194,0,96,20,194,67,51,90,32,195,0,129,33,198,102,194,
  118,194,119,136,137,154,170,19,101,3,16,0,5,146,2,194,
  50,34,38,195,34,38,34,98,195,34,98,50,194,51,102,194,
  0,87,16,48,6,195,0,3,197,0,48,195,0,115,35,195,
  34,16,96,0,2,50,98,194,35,65,3,17,198,102,194,119,
  136,119,136,137,153,160,197,0,96,3,194,0,96,4,195,0,
  5,162,195,0,128,18,100,16,203,0,10,49,96,33,194,0,
  88,32,49,195,0,6,195,0,6,0,96,195,0,196,102,99,
  194,0,194,102,96,6,195,0,3,197,0,32,195,0,96,195,
  0,2,0,96,0,2,0,96,196,0,19,96,203,0,19,96,
  197,0,96,3,194,0,96,3,196,0,147,32,194,0,128,2,
  97,65,203,0,9,19,99,16,194,0,114,2,16,195,0,6,
  195,34,38,0,98,195,34,48,195,0,18,194,0,194,33,96,
  6,195,0,3,51,194,50,34,35,48,195,0,96,195,0,3,
  0,96,0,3,0,96,195,0,1,49,96,202,0,1,65,96,
  197,0,112,3,194,0,96,4,196,0,145,66,194,0,128,3,
  96,19,203,51,56,51,97,195,0,112,33,196,0,6,195,0,
  6,0,96,195,0,48,194,0,1,35,194,0,34,16,96,6,
  205,0,96,195,0,3,0,96,0,2,0,96,195,0,3,51,
  99,201,51,194,35,16,96,197,0,96,3,194,0,96,4,196,
  0,128,20,194,0,112,3,112,3,203,0,7,0,112,195,0,
  115,16,196,0,7,195,0,7,0,112,195,0,196,34,48,194,
  0,194,34,112,7,205,0,112,195,0,2,0,112,0,3,0,
  112,195,0,2,0,112,202,0,3,0,112,197,0,112,3,194,
  0,112,3,196,0,128,3,194,0,112,3,112,2,203,0,7,
  0,112,195,0,113,197,0,7,195,0,7,0,112,204,0,112,
  7,198,119,136,153,154,196,0,112,195,0,3,0,112,0,2,
  0,112,195,0,3,0,112,202,0,2,0,112,197,0,112,3,
  194,0,96,4,196,0,112,4,194,0,112,3,112,3,194,0,
  202,119,16,112,195,0,112,197,0,41,195,0,7,0,112,195,
  0,119,103,136,153,154,196,0,112,2,200,0,7,196,0,112,
  195,0,2,0,112,0,2,0,112,195,0,2,0,112,194,0,
  1,201,119,112,197,0,112,3,194,0,112,3,196,0,112,3,
  194,0,96,3,128,3,194,0,128,200,0,1,49,128,195,0,
  128,196,0,2,165,195,0,8,0,128,195,0,128,195,0,8,
  196,0,128,2,200,0,88,196,0,128,195,0,2,0,128,0,
  2,0,128,195,0,2,0,128,194,0,19,128,198,0,2,199,
  0,112,3,194,0,112,3,196,0,96,3,194,0,96,2,128,
  2,194,0,128,201,0,19,128,195,0,144,196,0,42,83,195,
  0,8,0,128,195,0,128,195,0,131,196,0,128,2,199,34,
  37,131,196,0,128,195,0,2,0,128,0,2,0,128,195,0,
  2,0,128,0,1,49,128,198,0,2,199,0,128,3,194,0,
  112,3,196,0,96,3,194,0,96,3,128,3,194,0,202,136,
  33,128,195,0,169,194,152,194,136,133,2,195,0,88,0,128,
  195,0,196,136,35,195,0,5,128,8,200,136,35,195,0,5,
  128,195,0,2,0,128,0,2,0,128,195,0,2,0,128,0,
  2,16,207,136,128,3,194,0,128,3,196,0,96,3,194,0,
  112,3,144,2,194,0,19,16,199,0,9,50,144,195,0,48,
  196,0,48,33,194,0,5,147,0,144,195,0,32,195,0,18,
  195,0,89,48,9,200,0,2,195,0,89,48,195,0,2,0,
  144,0,2,0,144,195,0,2,0,144,0,2,3,16,204,0,
  1,57,128,3,194,0,128,3,196,0,112,3,194,0,112,3,
  144,2,194,0,1,49,199,0,9,35,144,195,0,48,196,0,
  34,16,194,0,89,19,0,144,195,0,32,194,0,1,33,194,
  0,5,145,48,9,200,0,32,194,0,5,145,32,195,0,2,
  0,144,0,2,0,144,195,0,2,0,144,0,2,49,205,0,
  19,25,144,2,194,0,128,3,196,0,112,3,194,0,112,3,
  144,2,195,0,19,199,34,41,34,144,195,0,197,34,33,194,
  0,5,145,2,0,144,195,0,196,34,16,194,0,89,16,32,
  9,200,34,195,0,89,16,48,195,0,2,0,144,0,2,0,
  144,195,0,2,0,144,0,3,206,34,49,9,144,2,194,0,
  144,2,196,0,128,3,194,0,112,2,160,2,203,0,10,0,
  160,203,0,90,16,33,0,160,201,0,5,161,2,16,10,202,
  0,5,161,2,196,0,2,0,160,0,2,0,160,195,0,2,
  0,160,208,0,32,10,160,2,194,0,144,3,196,0,128,2,
  194,0,128,3,160,2,203,0,10,0,160,202,0,5,161,2,
  16,0,160,201,0,90,16,33,0,10,202,0,90,16,32,196,
  0,2,0,160,0,2,0,160,195,0,2,0,160,208,0,32,
  10,196,170,161,3,196,0,128,3,194,0,128,2,206,170,16,
  204,170,16,33,194,0,202,170,161,2,16,0,10,202,170,161,
  2,197,0,2,1,196,170,160,195,0,2,1,211,170,19,18,
  194,0,19,18,196,0,128,2,194,0,128,3,35,18,203,0,
  1,49,64,202,0,4,2,16,194,0,48,201,0,48,33,194,
  0,4,202,0,64,32,197,0,2,19,16,0,2,19,16,195,
  0,2,19,16,208,0,1,49,1,50,194,0,1,50,196,0,
  144,2,194,0,128,2,1,50,204,0,19,48,202,0,3,33,
  195,0,32,201,0,34,16,194,0,3,202,0,50,198,0,2,
  49,194,0,2,49,196,0,2,49,209,0,19,16,0,19,195,
  34,35,196,0,144,3,194,0,144,2,0,19,205,34,50,203,
  34,16,195,0,50,201,34,49,195,0,2,202,34,32,198,0,
  3,195,34,35,16,196,0,3,210,34,49,0,202,0,144,2,
  194,0,144,2,255,0,223,0,202,0,160,2,194,0,144,2,
  255,0,223,0,202,0,196,170,161,2,255,0,223,0,202,0,
  19,18,194,0,19,18,255,0,223,0,202,0,1,50,194,0,
  1,50,255,0,223,0,203,0,19,195,34,35,255,0,223,0
}

RES_PCX_BRICKS =
{
  10,5,1,4,0,0,0,0,239,0,135,0,0,0,0,0,
  0,0,0,25,28,32,51,57,65,62,70,81,74,84,98,91,
  100,119,109,117,141,139,147,175,179,185,209,218,224,234,223,62,
  35,249,163,27,36,159,222,32,214,199,188,74,155,232,106,115,
  0,1,120,0,1,0,0,0,0,0,0,0,0,0,0,0,
  0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,
  0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,
  0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,
  255,68,232,68,194,136,104,194,102,72,136,134,136,134,104,195,
  102,100,194,136,72,194,136,102,195,136,104,102,72,136,134,136,
  104,196,136,100,194,136,104,196,102,72,195,136,195,104,194,102,
  100,194,136,104,134,104,195,102,100,194,136,104,134,195,102,68,
  194,136,102,136,194,134,195,102,72,136,194,134,195,102,72,195,
  136,104,136,195,102,72,136,104,195,136,102,104,69,84,72,136,
  104,102,84,72,136,134,136,134,104,102,101,84,68,134,196,102,
  72,200,102,100,134,102,72,200,102,72,200,102,100,134,198,102,
  72,200,102,100,134,199,102,100,199,102,72,201,102,72,198,102,
  72,200,102,72,198,102,132,86,100,134,195,102,101,72,199,102,
  101,68,197,102,72,200,102,100,134,102,72,200,102,70,200,102,
  84,134,198,102,70,200,102,100,86,199,102,100,199,102,72,201,
  102,72,197,102,101,70,200,102,72,198,102,69,102,100,197,102,
  72,200,102,84,197,102,70,200,102,100,194,102,70,199,102,101,
  70,200,102,100,198,102,101,70,200,102,100,134,199,102,100,199,
  102,70,200,102,101,70,198,102,70,199,102,101,70,200,102,100,
  197,102,70,200,102,100,196,102,101,70,200,102,84,134,102,72,
  200,102,70,200,102,84,134,198,102,70,200,102,100,86,199,102,
  84,199,102,72,201,102,70,197,102,101,69,200,102,72,200,102,
  100,196,102,101,70,200,102,84,196,102,101,70,200,102,84,194,
  102,70,199,102,101,70,200,102,84,198,102,101,70,200,102,84,
  200,102,100,199,102,70,200,102,101,70,197,102,101,69,199,102,
  101,70,200,102,84,196,102,101,70,200,102,100,196,102,101,70,
  200,102,84,194,102,70,199,102,101,70,200,102,84,134,197,102,
  101,70,200,102,84,200,102,84,86,198,102,70,194,102,86,197,
  102,101,70,197,102,101,68,199,102,101,70,200,102,84,196,102,
  101,69,200,102,84,196,102,101,70,199,102,85,84,194,102,70,
  199,102,101,69,200,102,84,102,101,134,195,102,101,70,200,102,
  84,200,102,84,70,198,102,70,194,102,84,197,102,101,70,197,
  102,101,68,86,199,102,69,200,102,84,197,102,69,86,199,102,
  84,197,102,70,198,102,101,84,68,86,102,70,199,102,84,68,
  86,199,102,84,86,101,72,194,102,101,84,70,199,102,101,68,
  86,198,102,101,68,69,198,102,69,194,102,101,70,196,102,84,
  70,197,102,101,68,69,198,102,101,68,86,199,102,84,86,195,
  102,101,68,85,199,102,84,196,102,101,70,198,102,84,194,68,
  69,102,70,195,102,86,85,101,85,194,68,69,86,194,102,195,
  101,85,84,69,102,84,69,101,85,68,70,194,102,194,101,86,
  194,85,84,68,69,194,102,86,195,85,84,194,68,86,194,102,
  101,194,85,68,85,194,102,84,85,101,194,85,194,68,194,102,
  194,86,194,85,195,68,84,85,102,101,194,85,68,85,86,102,
  101,102,195,85,84,69,194,102,194,85,194,68,84,86,194,85,
  102,194,85,84,195,102,194,85,70,195,102,195,85,196,68,86,
  255,68,249,68,136,134,102,100,86,72,136,134,136,194,102,101,
  68,194,136,134,104,136,104,195,102,100,194,136,195,102,101,68,
  200,102,100,72,134,195,136,194,134,194,102,72,136,194,134,102,
  100,71,197,153,152,153,136,72,136,134,136,102,134,195,102,72,
  195,136,194,134,101,68,194,136,134,104,136,104,195,102,100,194,
  136,196,102,100,195,136,134,136,134,102,101,68,72,134,195,136,
  194,134,194,102,72,136,194,134,102,196,102,69,70,198,102,84,
  134,200,102,100,134,197,102,84,200,102,100,134,200,102,72,196,
  102,100,120,200,136,69,200,102,72,198,102,84,134,200,102,100,
  134,197,102,100,134,199,102,84,134,200,102,72,196,102,196,102,
  84,70,198,102,100,134,200,102,100,134,197,102,100,200,102,100,
  134,200,102,70,196,102,100,201,136,69,86,199,102,70,198,102,
  100,134,200,102,100,134,197,102,84,200,102,100,134,200,102,70,
  196,102,197,102,70,198,102,100,201,102,84,134,197,102,100,200,
  102,84,200,102,101,70,196,102,100,201,136,69,200,102,70,198,
  102,100,201,102,84,134,197,102,100,200,102,84,200,102,101,70,
  196,102,196,102,101,70,198,102,100,134,200,102,100,134,197,102,
  100,200,102,100,134,200,102,70,196,102,84,200,136,135,69,199,
  102,101,70,198,102,100,134,200,102,100,134,197,102,84,200,102,
  100,134,200,102,70,196,102,196,102,101,70,198,102,100,201,102,
  84,198,102,100,200,102,84,200,102,101,70,196,102,84,201,136,
  69,86,198,102,101,70,198,102,100,201,102,84,198,102,84,200,
  102,84,200,102,101,70,196,102,196,102,101,70,198,102,100,201,
  102,84,134,197,102,100,200,102,84,200,102,101,70,196,102,84,
  200,136,135,69,86,198,102,101,70,198,102,100,201,102,84,134,
  197,102,84,200,102,84,200,102,101,70,196,102,197,102,70,198,
  102,84,201,102,68,198,102,100,68,86,198,102,100,86,199,102,
  101,70,196,102,100,120,199,136,135,69,86,198,102,101,69,198,
  102,100,101,200,102,84,198,102,84,200,102,100,86,199,102,101,
  70,196,102,196,102,101,68,197,102,101,84,200,102,101,68,198,
  102,84,104,69,198,102,84,69,199,102,85,70,196,102,68,120,
  199,136,116,68,86,198,102,85,68,86,197,102,194,84,86,199,
  102,84,69,197,102,84,86,199,102,84,69,199,102,85,70,196,
  102,101,194,85,101,84,68,70,195,102,194,85,68,70,194,102,
  194,101,86,194,85,194,68,70,102,195,101,85,84,70,132,68,
  195,102,194,85,84,68,69,194,102,86,101,195,85,70,194,102,
  101,84,68,71,195,135,196,119,194,68,69,102,194,101,194,85,
  101,194,68,69,195,102,194,85,194,68,69,102,194,101,86,195,
  85,84,68,86,195,101,85,68,69,85,101,195,102,194,85,194,
  68,69,194,102,86,101,195,85,70,194,102,101,85,255,68,249,
  68,134,136,104,196,136,100,194,136,104,196,102,72,136,104,136,
  134,136,104,194,102,100,194,153,137,194,153,195,136,132,194,136,
  104,134,195,102,68,194,136,102,136,194,134,195,102,72,136,134,
  200,68,85,88,195,102,72,136,104,195,136,194,102,101,84,72,
  136,104,102,84,72,136,134,136,134,104,102,101,84,68,194,136,
  104,195,102,100,195,136,134,136,134,194,102,100,136,134,195,136,
  134,102,100,85,68,136,134,199,102,100,134,198,102,72,200,102,
  100,152,199,136,132,86,198,102,72,201,102,72,194,102,101,195,
  85,69,195,85,197,102,72,200,102,100,134,195,102,101,72,199,
  102,101,68,134,197,102,100,134,199,102,100,134,200,102,72,194,
  102,199,102,84,134,198,102,72,200,102,100,200,136,132,85,198,
  102,72,201,102,72,197,102,101,70,200,102,72,200,102,100,197,
  102,72,200,102,84,134,197,102,84,200,102,100,134,200,102,70,
  194,102,199,102,100,198,102,101,70,200,102,100,152,199,136,132,
  85,198,102,70,200,102,101,70,198,102,70,199,102,101,70,200,
  102,100,197,102,70,200,102,100,198,102,100,200,102,84,201,102,
  70,194,102,199,102,100,198,102,101,70,200,102,100,152,199,136,
  132,85,198,102,70,200,102,101,70,198,102,70,199,102,101,70,
  200,102,100,197,102,70,200,102,100,198,102,100,200,102,84,201,
  102,70,194,102,199,102,84,134,198,102,72,200,102,100,200,136,
  116,85,198,102,72,201,102,70,197,102,101,69,200,102,72,200,
  102,100,196,102,101,70,200,102,84,198,102,84,86,199,102,100,
  134,200,102,70,194,102,199,102,84,198,102,101,70,200,102,84,
  200,136,132,85,198,102,70,200,102,101,70,197,102,101,69,199,
  102,101,70,200,102,84,196,102,101,70,200,102,100,198,102,84,
  86,199,102,84,200,102,101,70,194,102,199,102,84,134,197,102,
  101,70,200,102,84,200,136,116,85,198,102,70,200,102,101,70,
  197,102,101,68,199,102,101,70,200,102,84,196,102,101,69,200,
  102,84,198,102,84,70,199,102,84,200,102,101,70,194,102,199,
  102,84,198,102,101,69,200,102,84,104,199,136,116,69,198,102,
  70,200,102,101,70,197,102,101,68,86,199,102,69,200,102,84,
  197,102,69,86,199,102,84,198,102,84,69,199,102,100,86,199,
  102,101,70,194,102,199,102,84,86,196,102,101,84,68,86,198,
  102,101,68,150,120,198,136,116,85,198,102,69,200,102,84,70,
  197,102,101,68,69,198,102,101,68,86,199,102,84,86,195,102,
  101,68,85,199,102,84,198,102,84,68,86,198,102,84,69,199,
  102,101,69,194,102,86,194,102,195,101,85,84,69,102,84,69,
  101,85,194,68,69,85,102,101,102,86,85,84,68,137,70,194,
  120,119,135,194,119,116,68,86,194,102,101,194,85,68,85,194,
  102,84,85,101,194,85,194,68,194,102,194,86,194,85,195,68,
  84,85,102,101,194,85,68,85,86,102,101,102,195,85,84,69,
  194,102,194,85,194,68,84,86,194,85,102,194,85,84,70,102,
  195,101,85,84,194,68,194,69,86,102,194,85,84,69,85,194,
  102,86,101,195,85,68,86,102,217,68,69,117,255,68,222,68,
  101,68,194,136,134,104,136,104,195,102,100,194,136,196,102,100,
  72,194,136,134,136,133,201,68,86,194,102,72,136,194,134,102,
  100,194,136,104,136,102,134,195,102,72,136,134,136,102,134,195,
  102,72,195,136,194,134,101,68,194,136,134,104,136,104,195,102,
  100,195,153,152,153,152,136,135,68,88,134,194,136,104,194,134,
  194,102,72,136,134,136,102,134,195,102,72,136,134,136,195,102,
  100,72,136,134,104,136,102,102,84,134,200,102,100,134,197,102,
  100,134,197,102,194,85,100,198,85,195,102,72,196,102,100,134,
  200,102,72,200,102,72,198,102,84,134,200,102,100,152,199,136,
  116,85,200,102,72,200,102,70,198,102,100,134,197,102,102,100,
  134,200,102,100,134,197,102,84,200,102,100,134,200,102,70,196,
  102,100,134,200,102,72,200,102,70,198,102,100,134,200,102,100,
  200,136,132,69,200,102,72,200,102,70,198,102,100,134,197,102,
  102,100,201,102,84,134,197,102,100,200,102,84,200,102,101,70,
  196,102,100,201,102,70,200,102,70,198,102,100,201,102,84,200,
  136,116,69,200,102,72,200,102,70,198,102,100,198,102,102,100,
  134,200,102,100,134,197,102,84,200,102,100,134,200,102,70,196,
  102,84,200,102,101,72,199,102,101,70,198,102,100,134,200,102,
  100,200,136,132,69,200,102,70,199,102,101,70,198,102,100,134,
  197,102,102,100,201,102,84,198,102,84,200,102,84,200,102,101,
  70,196,102,84,200,102,101,70,199,102,101,70,198,102,100,201,
  102,84,199,136,135,84,69,199,102,101,72,200,102,70,198,102,
  100,198,102,102,100,201,102,84,134,197,102,84,200,102,84,200,
  102,101,70,196,102,84,200,102,101,70,199,102,101,70,198,102,
  100,201,102,84,198,136,135,84,68,69,199,102,101,72,199,102,
  101,69,198,102,100,198,102,102,84,201,102,68,198,102,84,200,
  102,84,86,199,102,101,70,196,102,100,200,102,101,69,199,102,
  101,69,198,102,100,101,200,102,84,198,136,116,152,100,69,199,
  102,101,70,199,102,101,68,198,102,100,198,102,101,84,200,102,
  101,68,198,102,84,199,102,101,68,69,199,102,85,70,196,102,
  84,199,102,101,194,68,86,198,102,85,68,86,197,102,194,84,
  86,199,102,84,120,196,136,135,89,136,116,69,199,102,84,69,
  199,102,84,68,86,197,102,100,86,197,102,85,68,70,194,102,
  194,101,86,194,85,194,68,70,102,195,101,85,84,198,102,85,
  84,194,68,69,194,102,86,101,195,85,70,194,102,101,85,84,
  195,102,101,194,85,84,195,68,69,102,194,101,194,85,101,194,
  68,69,195,102,194,85,194,68,69,102,194,101,86,195,85,84,
  71,119,135,194,136,118,151,119,194,68,85,86,102,86,194,101,
  85,194,68,86,102,194,101,195,85,194,68,69,195,102,194,85,
  84,69,86,102,101,69,85,255,68,249,68,134,195,102,68,194,
  136,102,136,194,134,195,102,72,136,194,134,195,102,72,195,136,
  104,136,195,102,72,136,104,195,136,194,102,69,84,194,136,134,
  104,194,136,194,134,100,194,136,104,134,196,136,134,72,136,194,
  134,195,102,100,136,134,194,136,104,194,134,194,102,72,136,134,
  136,85,198,68,69,86,104,136,104,195,102,100,194,136,104,195,
  102,100,194,136,104,195,102,100,195,136,134,136,134,194,102,100,
  136,134,196,102,72,201,102,72,198,102,72,200,102,72,198,102,
  100,86,100,134,199,102,100,134,200,102,72,198,102,100,134,200,
  102,72,195,102,101,196,85,84,194,85,199,102,100,134,197,102,
  100,134,197,102,100,134,199,102,100,134,102,196,102,72,201,102,
  72,197,102,101,70,200,102,72,198,102,69,102,100,134,199,102,
  100,200,102,101,72,198,102,100,134,200,102,72,200,102,100,134,
  200,102,100,134,197,102,100,134,197,102,84,200,102,100,134,102,
  196,102,70,200,102,101,70,198,102,70,199,102,101,70,200,102,
  100,200,102,84,201,102,70,198,102,84,201,102,72,200,102,100,
  201,102,84,198,102,84,198,102,100,200,102,84,194,102,196,102,
  72,201,102,70,197,102,101,69,200,102,72,200,102,100,134,199,
  102,100,200,102,101,72,198,102,100,134,200,102,70,200,102,100,
  134,200,102,100,198,102,100,198,102,84,86,199,102,100,134,102,
  196,102,70,200,102,101,70,197,102,101,69,199,102,101,70,200,
  102,84,200,102,84,200,102,101,70,198,102,84,200,102,101,72,
  200,102,100,201,102,84,198,102,84,198,102,84,86,199,102,84,
  194,102,196,102,70,194,102,86,197,102,101,70,197,102,101,68,
  199,102,101,70,200,102,84,200,102,84,200,102,101,72,198,102,
  84,200,102,101,72,200,102,100,201,102,84,198,102,84,198,102,
  84,70,199,102,84,194,102,196,102,70,194,102,84,197,102,101,
  70,197,102,101,68,86,199,102,69,200,102,84,200,102,84,86,
  199,102,101,70,102,86,196,102,84,86,199,102,101,70,200,102,
  100,201,102,84,198,102,84,198,102,84,69,199,102,100,86,102,
  196,102,69,194,102,101,70,196,102,84,70,197,102,101,68,69,
  198,102,101,68,86,199,102,84,199,102,101,68,69,199,102,101,
  69,197,102,85,68,69,199,102,84,69,200,102,100,86,194,102,
  100,196,102,101,68,197,102,101,68,198,102,84,68,86,198,102,
  84,69,102,102,101,194,85,68,85,194,102,84,85,101,194,85,
  194,68,194,102,194,86,194,85,195,68,84,85,102,101,194,85,
  68,85,86,102,101,102,195,85,84,195,102,194,101,86,85,84,
  194,68,85,195,102,194,86,194,85,68,86,194,102,86,85,84,
  194,68,85,86,102,86,194,101,85,194,68,86,102,101,195,102,
  194,85,84,69,86,102,101,69,86,194,85,84,68,70,102,86,
  194,85,84,68,70,102,195,101,85,84,194,68,194,69,86,102,
  194,85,84,69,85,255,68,249,68,72,136,194,134,102,100,194,
  136,104,136,102,134,195,102,72,136,134,136,102,134,195,102,72,
  195,136,194,134,101,68,194,136,134,104,136,104,102,104,136,194,
  102,69,100,194,136,104,134,194,102,84,72,194,136,102,136,194,
  134,195,102,72,136,134,196,102,68,195,136,104,136,195,102,68,
  136,104,195,136,104,194,102,100,194,136,104,194,102,72,136,134,
  136,134,104,195,102,100,194,136,104,134,104,195,102,100,196,136,
  104,102,84,72,72,196,102,100,134,200,102,72,200,102,72,198,
  102,84,134,202,102,100,84,198,102,101,72,201,102,72,198,102,
  72,200,102,72,200,102,100,134,196,102,72,200,102,100,134,199,
  102,100,134,197,102,101,72,70,196,102,100,134,200,102,72,200,
  102,70,198,102,100,134,202,102,101,68,199,102,72,201,102,72,
  197,102,101,70,200,102,72,200,102,100,197,102,72,200,102,100,
  134,199,102,100,199,102,72,70,196,102,100,201,102,70,200,102,
  70,198,102,100,204,102,100,199,102,70,200,102,101,72,198,102,
  70,199,102,101,70,200,102,84,197,102,70,200,102,100,200,102,
  100,199,102,70,70,196,102,84,200,102,101,72,199,102,101,70,
  198,102,100,134,203,102,84,199,102,72,201,102,72,197,102,101,
  70,200,102,72,200,102,100,196,102,101,70,200,102,84,134,199,
  102,84,199,102,72,70,196,102,84,200,102,101,70,199,102,101,
  70,198,102,100,204,102,84,199,102,70,200,102,101,70,197,102,
  101,70,199,102,101,70,200,102,84,196,102,101,70,200,102,84,
  200,102,84,199,102,70,70,196,102,84,200,102,101,70,199,102,
  101,70,198,102,100,204,102,84,199,102,70,200,102,101,70,197,
  102,101,70,199,102,101,70,200,102,84,196,102,101,70,200,102,
  84,200,102,84,199,102,70,70,196,102,84,199,102,101,85,70,
  199,102,101,70,198,102,100,204,102,84,199,102,70,200,102,101,
  72,197,102,101,70,199,102,101,70,200,102,84,196,102,101,70,
  199,102,85,84,200,102,84,199,102,70,70,196,102,100,199,102,
  85,68,69,199,102,101,69,198,102,100,101,203,102,100,198,102,
  101,70,200,102,100,70,197,102,101,68,69,199,102,69,200,102,
  84,197,102,70,198,102,101,84,68,86,199,102,84,86,198,102,
  70,70,196,102,84,198,102,101,195,68,86,198,102,85,68,86,
  197,102,194,84,86,202,102,84,70,197,102,85,70,200,102,84,
  70,197,102,101,70,132,86,197,102,101,68,86,198,102,101,84,
  196,102,101,70,198,102,84,194,68,69,198,102,101,84,69,197,
  102,101,69,70,194,102,101,85,84,195,102,101,194,85,84,195,
  68,69,102,194,101,194,85,101,194,68,69,195,102,194,85,194,
  68,69,102,194,101,86,85,86,194,85,86,85,194,68,195,102,
  101,85,84,68,195,102,86,85,101,85,84,194,68,194,102,194,
  86,194,85,68,104,68,70,194,102,101,194,85,194,68,86,102,
  101,102,195,85,84,195,102,194,85,70,195,102,195,85,196,68,
  86,102,86,194,85,86,84,194,68,86,194,102,101,85,84,68,
  255,68,249,68,195,102,72,136,194,134,195,102,72,195,136,104,
  136,195,102,72,136,104,195,136,194,102,69,84,194,153,137,153,
  136,152,153,137,196,153,132,88,136,104,194,134,194,102,72,136,
  194,134,195,102,72,195,136,104,136,195,102,72,136,134,136,102,
  134,194,102,72,136,134,136,195,102,100,72,136,134,104,136,104,
  195,102,100,194,136,104,195,102,100,195,136,134,136,134,194,102,
  100,136,134,195,136,134,102,132,101,68,136,194,134,195,102,72,
  198,102,72,200,102,72,200,102,100,152,203,136,132,69,198,102,
  72,198,102,72,200,102,72,199,102,70,198,102,100,134,200,102,
  100,134,197,102,100,134,199,102,100,134,200,102,72,195,102,195,
  102,72,197,102,101,70,200,102,72,200,102,100,152,203,136,116,
  69,198,102,72,197,102,101,70,200,102,72,199,102,70,198,102,
  100,134,200,102,100,134,197,102,84,200,102,100,134,200,102,70,
  195,102,194,102,101,70,198,102,70,199,102,101,70,200,102,100,
  204,136,132,69,198,102,70,198,102,70,199,102,101,72,199,102,
  70,198,102,100,201,102,84,198,102,100,200,102,84,201,102,70,
  195,102,195,102,70,197,102,101,69,200,102,72,200,102,100,204,
  136,116,69,198,102,70,197,102,101,69,200,102,70,198,102,101,
  70,198,102,100,134,200,102,100,198,102,84,86,199,102,100,134,
  200,102,70,195,102,194,102,101,70,197,102,101,69,199,102,101,
  70,200,102,84,204,136,116,69,198,102,70,197,102,101,69,199,
  102,101,72,199,102,70,198,102,100,201,102,84,198,102,84,86,
  199,102,84,200,102,101,70,195,102,194,102,101,70,197,102,101,
  68,199,102,101,70,200,102,84,120,203,136,116,69,197,102,101,
  70,197,102,101,68,199,102,101,72,198,102,101,69,198,102,100,
  201,102,84,198,102,84,70,199,102,84,200,102,101,70,195,102,
  194,102,101,70,197,102,101,68,86,199,102,69,200,102,84,120,
  203,136,116,69,197,102,100,70,197,102,101,68,86,199,102,70,
  198,102,101,68,198,102,100,201,102,84,198,102,68,69,199,102,
  100,86,199,102,101,70,195,102,194,102,84,70,197,102,101,68,
  69,198,102,101,68,86,199,102,84,71,203,136,116,69,196,102,
  101,84,70,197,102,84,68,69,198,102,101,69,198,102,84,68,
  86,197,102,100,86,199,102,101,68,197,102,101,194,68,86,198,
  102,84,69,199,102,101,69,195,102,194,85,194,68,194,102,194,
  86,194,85,195,68,84,85,102,101,194,85,68,85,86,102,101,
  102,195,85,84,68,71,119,135,119,120,197,136,119,116,69,102,
  84,69,101,85,194,68,194,102,194,86,85,196,68,84,85,102,
  101,194,85,68,86,102,194,101,194,85,194,68,69,195,102,194,
  85,84,69,86,102,101,69,86,194,85,84,68,70,102,195,101,
  84,195,68,194,69,86,102,194,85,84,69,85,194,102,86,101,
  195,85,68,86,102,101,255,68,249,68,134,195,102,72,136,134,
  136,102,134,195,102,72,195,136,194,134,101,68,194,136,134,104,
  136,104,194,102,85,84,202,68,69,195,102,100,194,136,104,195,
  102,100,72,194,136,134,136,134,194,102,100,72,134,195,136,194,
  134,194,102,72,136,194,134,102,100,194,136,104,136,102,134,195,
  102,72,136,134,136,102,134,195,102,72,195,136,194,134,101,68,
  194,136,134,104,136,104,195,102,100,136,134,195,102,72,136,196,
  102,72,200,102,72,198,102,84,134,199,102,101,194,85,69,200,
  85,86,195,102,100,134,197,102,100,134,199,102,100,134,200,102,
  72,196,102,100,134,200,102,72,200,102,72,198,102,84,134,200,
  102,100,134,196,102,72,102,196,102,72,200,102,70,198,102,100,
  134,201,102,101,70,204,102,100,134,197,102,84,200,102,100,134,
  200,102,70,196,102,100,134,200,102,72,200,102,70,198,102,100,
  134,200,102,100,134,196,102,72,102,196,102,70,200,102,70,198,
  102,100,203,102,70,204,102,84,134,197,102,100,200,102,84,200,
  102,101,70,196,102,100,201,102,70,200,102,70,198,102,100,201,
  102,84,134,195,102,101,72,102,195,102,101,72,199,102,101,70,
  198,102,100,134,201,102,101,70,204,102,100,134,197,102,84,200,
  102,100,134,200,102,70,196,102,84,200,102,101,72,199,102,101,
  70,198,102,100,134,200,102,100,134,196,102,72,102,195,102,101,
  70,199,102,101,70,198,102,100,202,102,101,70,204,102,84,198,
  102,84,200,102,84,200,102,101,70,196,102,84,200,102,101,70,
  199,102,101,70,198,102,100,201,102,84,196,102,101,70,102,195,
  102,101,70,199,102,101,70,198,102,100,202,102,101,70,204,102,
  84,134,197,102,84,200,102,84,200,102,101,70,196,102,84,200,
  102,101,70,199,102,101,70,198,102,100,201,102,84,134,195,102,
  101,72,102,195,102,101,69,199,102,101,69,198,102,100,101,201,
  102,101,70,194,102,101,201,102,68,198,102,84,200,102,84,86,
  199,102,101,70,196,102,100,200,102,101,69,199,102,101,69,198,
  102,100,101,200,102,84,196,102,101,70,102,194,102,101,194,68,
  86,198,102,85,68,86,197,102,194,84,86,200,102,101,69,194,
  102,84,200,102,101,68,86,196,102,101,68,199,102,101,68,69,
  199,102,85,70,196,102,84,199,102,101,194,68,86,198,102,85,
  68,86,197,102,194,84,86,199,102,84,69,195,102,101,68,86,
  85,84,195,68,69,102,194,101,194,85,101,194,68,69,195,102,
  194,85,194,68,69,102,194,101,86,194,85,86,85,84,68,194,
  85,68,70,194,102,194,101,86,194,85,194,68,69,102,101,85,
  101,84,68,198,102,85,84,194,68,69,194,102,86,101,195,85,
  70,194,102,101,85,84,195,102,101,194,85,84,195,68,69,102,
  194,101,194,85,101,194,68,69,195,102,194,85,194,68,69,102,
  194,101,86,195,85,84,68,196,85,68,69,255,68,249,68,194,
  136,102,136,194,134,195,102,72,136,194,134,195,102,72,195,136,
  104,136,195,102,72,136,104,195,136,194,102,69,84,72,136,104,
  102,72,194,136,102,195,136,104,102,72,136,134,136,104,196,136,
  100,194,136,104,196,102,73,195,153,195,137,194,136,132,88,136,
  104,134,104,195,102,100,194,136,104,134,195,102,68,194,136,102,
  136,194,134,195,102,72,136,194,134,195,102,72,195,136,104,136,
  102,100,68,72,136,104,201,102,72,198,102,72,200,102,72,200,
  102,100,134,195,102,72,200,102,72,200,102,100,134,198,102,73,
  200,136,132,86,199,102,100,199,102,72,201,102,72,198,102,72,
  198,102,132,68,72,194,102,201,102,72,197,102,101,70,200,102,
  72,200,102,100,196,102,72,200,102,70,200,102,84,134,198,102,
  72,200,136,132,85,199,102,100,199,102,72,201,102,72,197,102,
  101,70,197,102,104,69,100,72,194,102,200,102,101,70,198,102,
  70,199,102,101,70,200,102,100,196,102,70,199,102,101,70,200,
  102,100,198,102,101,72,200,136,132,85,199,102,100,199,102,70,
  200,102,101,70,198,102,70,197,102,100,86,101,70,194,102,201,
  102,70,197,102,101,69,200,102,72,200,102,100,196,102,72,200,
  102,70,200,102,84,134,198,102,72,200,136,132,85,199,102,84,
  199,102,72,201,102,70,197,102,101,69,197,102,101,194,102,72,
  194,102,200,102,101,70,197,102,101,69,199,102,101,70,200,102,
  84,196,102,70,199,102,101,70,200,102,84,198,102,101,72,200,
  136,116,85,199,102,100,199,102,70,200,102,101,70,197,102,101,
  69,199,102,101,70,194,102,200,102,101,70,197,102,101,68,199,
  102,101,70,200,102,84,196,102,70,199,102,101,70,200,102,84,
  134,197,102,101,72,200,136,116,85,199,102,84,86,198,102,70,
  194,102,86,197,102,101,70,197,102,101,68,199,102,101,70,194,
  102,200,102,101,70,197,102,101,68,86,199,102,69,200,102,84,
  196,102,70,199,102,101,69,200,102,84,198,102,101,72,200,136,
  116,85,199,102,84,70,198,102,70,194,102,84,197,102,101,70,
  197,102,101,68,86,199,102,69,194,102,195,102,70,196,102,84,
  70,197,102,101,68,69,198,102,101,68,86,199,102,84,86,195,
  102,70,199,102,84,68,86,199,102,84,86,196,102,101,84,72,
  199,136,135,68,85,198,102,101,68,69,198,102,69,194,102,101,
  70,196,102,84,70,197,102,101,68,69,198,102,101,68,86,102,
  85,194,102,84,85,101,194,85,194,68,194,102,194,86,194,85,
  195,68,84,85,102,101,194,85,68,85,86,102,101,102,195,85,
  84,69,194,102,85,70,195,102,86,85,101,85,194,68,69,86,
  194,102,195,101,85,84,69,195,102,101,85,68,72,194,136,194,
  135,120,194,119,116,68,69,86,102,86,195,85,84,194,68,86,
  194,102,101,194,85,68,85,194,102,84,85,101,194,85,194,68,
  194,102,194,86,194,85,195,68,84,85,102,101,194,85,68,85,
  86,255,68,249,68,100,198,153,152,153,136,72,136,134,136,102,
  134,195,102,72,195,136,194,134,101,68,194,136,134,104,136,104,
  195,102,100,195,136,134,102,100,86,72,136,134,136,194,102,101,
  68,194,136,134,104,136,104,195,102,100,194,136,102,84,195,68,
  195,153,137,194,153,194,136,132,69,134,195,136,194,134,194,102,
  69,121,197,153,137,152,84,194,136,104,134,104,195,102,100,196,
  136,104,102,84,72,136,194,134,101,68,136,100,152,200,136,69,
  200,102,72,198,102,84,134,200,102,100,134,197,102,69,70,198,
  102,84,134,200,102,100,134,194,102,101,194,85,84,152,199,136,
  132,69,200,102,72,151,199,136,132,86,199,102,100,134,197,102,
  101,72,196,102,84,134,100,201,136,69,86,199,102,70,198,102,
  100,134,200,102,100,134,197,102,84,70,198,102,100,134,200,102,
  100,134,196,102,85,84,152,199,136,132,69,200,102,72,137,120,
  198,136,132,85,199,102,100,199,102,72,196,102,100,134,100,152,
  200,136,69,200,102,70,198,102,100,201,102,84,134,198,102,70,
  198,102,100,201,102,84,134,197,102,100,200,136,116,69,199,102,
  101,72,136,104,198,136,132,86,199,102,100,199,102,70,196,102,
  100,102,84,200,136,135,69,199,102,101,70,198,102,100,134,200,
  102,100,134,197,102,101,70,198,102,100,134,200,102,100,134,197,
  102,100,152,199,136,116,69,200,102,72,136,150,198,136,116,86,
  199,102,84,199,102,72,196,102,100,134,84,201,136,69,86,198,
  102,101,70,198,102,100,201,102,84,198,102,101,70,198,102,100,
  201,102,84,198,102,100,200,136,116,69,199,102,101,72,136,133,
  120,197,136,132,85,199,102,84,199,102,70,196,102,100,102,84,
  200,136,135,69,86,198,102,101,70,198,102,100,201,102,84,134,
  197,102,101,70,198,102,100,201,102,84,134,197,102,100,200,136,
  116,69,199,102,101,72,194,136,71,197,136,116,85,199,102,84,
  199,102,70,196,102,100,102,100,200,136,135,69,86,198,102,101,
  69,198,102,100,101,200,102,84,199,102,70,198,102,84,201,102,
  68,198,102,100,200,136,132,69,199,102,101,71,194,136,148,103,
  196,136,116,85,199,102,84,86,198,102,70,86,195,102,100,101,
  84,120,199,136,116,68,86,198,102,85,68,86,197,102,194,84,
  86,199,102,84,69,197,102,101,68,197,102,101,84,200,102,101,
  68,198,102,84,120,199,136,116,69,199,102,85,69,194,136,137,
  68,120,194,136,135,68,69,198,102,101,84,69,197,102,101,69,
  70,195,102,194,84,84,71,136,194,135,196,119,194,68,69,102,
  194,101,194,85,101,194,68,69,195,102,194,85,194,68,69,102,
  194,101,86,195,85,84,68,86,101,194,85,101,84,68,70,195,
  102,194,85,68,70,194,102,194,101,86,194,85,194,68,70,102,
  195,101,85,84,119,120,195,136,135,194,119,116,68,69,194,102,
  86,101,195,85,68,87,194,136,100,70,194,119,116,194,68,86,
  102,86,194,85,86,84,194,68,86,194,102,101,85,84,68,70,
  102,194,85,194,68,255,68,249,68,134,200,68,85,88,195,102,
  72,136,104,195,136,194,102,101,84,72,136,104,102,84,72,136,
  134,136,134,104,102,101,84,68,134,136,104,196,136,100,194,136,
  104,196,102,72,136,104,136,134,136,104,194,102,100,194,153,137,
  194,153,195,136,132,196,68,69,194,102,68,194,136,196,102,100,
  199,68,69,85,134,194,102,100,136,134,195,136,134,102,68,85,
  68,136,194,134,101,84,72,136,104,102,102,101,195,85,69,195,
  85,197,102,72,200,102,100,134,195,102,101,72,199,102,101,68,
  199,102,100,134,198,102,72,200,102,100,152,199,136,132,196,85,
  86,194,102,72,199,102,195,85,84,195,85,86,196,102,100,134,
  198,102,68,102,72,196,102,100,134,195,102,196,102,101,70,200,
  102,72,200,102,100,197,102,72,200,102,84,199,102,84,134,198,
  102,72,200,102,100,200,136,132,85,198,102,72,202,102,84,200,
  102,100,134,198,102,100,102,70,196,102,100,196,102,197,102,70,
  199,102,101,70,200,102,100,197,102,70,200,102,100,199,102,100,
  198,102,101,70,200,102,100,152,199,136,132,85,198,102,70,202,
  102,100,200,102,84,201,102,70,196,102,100,196,102,197,102,70,
  199,102,101,70,200,102,100,197,102,70,200,102,100,199,102,100,
  198,102,101,70,200,102,100,152,199,136,132,85,198,102,70,202,
  102,100,200,102,84,201,102,70,196,102,100,196,102,196,102,101,
  69,200,102,72,200,102,100,196,102,101,70,200,102,84,199,102,
  84,134,198,102,72,200,102,100,200,136,116,85,198,102,72,202,
  102,84,86,199,102,100,134,200,102,70,196,102,100,196,102,196,
  102,101,69,199,102,101,70,200,102,84,196,102,101,70,200,102,
  100,199,102,84,198,102,101,70,200,102,84,200,136,132,85,198,
  102,70,202,102,84,86,199,102,84,200,102,101,70,196,102,84,
  196,102,196,102,101,68,199,102,101,70,200,102,84,196,102,101,
  69,200,102,84,199,102,84,134,197,102,101,70,200,102,84,200,
  136,116,85,198,102,70,202,102,84,70,199,102,84,200,102,101,
  70,196,102,84,196,102,196,102,101,68,86,199,102,69,200,102,
  84,197,102,69,86,199,102,84,86,198,102,84,198,102,101,69,
  200,102,84,200,136,116,69,198,102,70,202,102,84,69,199,102,
  100,86,199,102,101,70,196,102,84,196,102,196,102,101,68,69,
  198,102,101,68,86,199,102,84,86,195,102,101,68,85,199,102,
  84,69,198,102,84,86,196,102,101,84,68,86,198,102,101,68,
  200,136,116,85,198,102,69,202,102,84,68,86,198,102,84,69,
  199,102,101,69,196,102,84,86,195,102,102,194,86,194,85,195,
  68,84,85,102,101,194,85,68,85,86,102,101,102,195,85,84,
  69,194,102,194,85,194,68,84,86,194,85,102,194,85,84,100,
  86,102,195,101,85,84,69,102,84,69,101,85,194,68,69,85,
  102,101,102,86,85,84,68,195,136,120,119,135,194,119,116,68,
  86,194,102,101,194,85,68,85,194,102,84,70,102,195,101,85,
  84,194,68,194,69,86,102,194,85,84,69,85,194,102,86,101,
  195,85,68,86,102,101,85,84,69,194,102,85,255,68,249,68,
  136,134,136,102,134,195,102,72,195,136,194,134,101,68,194,136,
  134,104,136,104,195,102,100,194,136,196,102,100,195,136,134,136,
  134,101,68,194,136,134,104,136,104,195,102,100,194,136,196,102,
  100,72,194,136,134,136,133,201,68,86,194,102,72,136,194,134,
  102,100,194,136,104,136,102,134,195,102,72,136,134,136,102,134,
  195,102,72,195,136,194,134,101,68,194,136,134,104,136,104,195,
  102,100,194,136,200,102,72,198,102,84,134,200,102,100,134,197,
  102,100,134,198,102,84,134,200,102,100,134,197,102,100,134,197,
  102,194,85,100,198,85,195,102,72,196,102,100,134,200,102,72,
  200,102,72,198,102,84,134,200,102,100,134,102,200,102,70,198,
  102,100,134,200,102,100,134,197,102,84,199,102,100,134,200,102,
  100,134,197,102,84,200,102,100,134,200,102,70,196,102,100,134,
  200,102,72,200,102,70,198,102,100,134,200,102,100,134,102,200,
  102,70,198,102,100,201,102,84,134,197,102,100,199,102,100,201,
  102,84,134,197,102,100,200,102,84,200,102,101,70,196,102,100,
  201,102,70,200,102,70,198,102,100,201,102,84,134,102,199,102,
  101,70,198,102,100,134,200,102,100,134,197,102,84,199,102,100,
  134,200,102,100,134,197,102,84,200,102,100,134,200,102,70,196,
  102,84,200,102,101,72,199,102,101,70,198,102,100,134,200,102,
  100,134,102,199,102,101,70,198,102,100,201,102,84,198,102,84,
  199,102,100,201,102,84,198,102,84,200,102,84,200,102,101,70,
  196,102,84,200,102,101,70,199,102,101,70,198,102,100,201,102,
  84,194,102,199,102,101,70,198,102,100,201,102,84,134,197,102,
  84,199,102,100,201,102,84,134,197,102,84,200,102,84,200,102,
  101,70,196,102,84,200,102,101,70,199,102,101,70,198,102,100,
  201,102,84,134,102,199,102,101,69,198,102,100,101,200,102,84,
  198,102,84,199,102,84,201,102,68,198,102,84,200,102,84,86,
  199,102,101,70,196,102,100,200,102,101,69,199,102,101,69,198,
  102,100,101,200,102,84,194,102,86,198,102,85,68,86,197,102,
  194,84,86,199,102,84,69,197,102,84,86,197,102,101,84,200,
  102,101,68,198,102,84,199,102,101,68,69,199,102,85,70,196,
  102,84,199,102,101,194,68,86,198,102,85,68,86,197,102,194,
  84,86,199,102,84,69,102,69,102,194,101,194,85,101,194,68,
  69,195,102,194,85,194,68,69,102,194,101,86,195,85,84,68,
  86,195,101,85,68,69,85,101,195,102,85,68,70,194,102,194,
  101,86,194,85,194,68,70,102,195,101,85,84,198,102,85,84,
  194,68,69,194,102,86,101,195,85,70,194,102,101,85,84,195,
  102,101,194,85,84,195,68,69,102,194,101,194,85,101,194,68,
  69,195,102,194,85,194,68,69,102,194,101,86,195,85,84,68,
  86,255,68,249,68
}

RES_PCX_RUNNER_SHEET =
{
  10,5,1,4,0,0,0,0,127,0,127,0,0,0,0,0,
  0,0,0,20,52,100,255,255,255,36,159,222,188,74,155,255,
  0,255,255,0,255,255,0,255,255,0,255,255,0,255,255,0,
  255,255,0,255,255,0,255,255,0,255,255,0,255,255,0,255,
  0,1,64,0,1,0,0,0,0,0,0,0,0,0,0,0,
  0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,
  0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,
  0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,
  255,0,0,205,0,68,64,210,0,68,64,212,0,4,68,199,
  0,204,0,4,51,52,209,0,68,51,52,212,0,67,51,64,
  198,0,204,0,67,34,35,64,207,0,4,51,34,35,64,210,
  0,4,50,34,52,198,0,203,0,4,50,17,18,52,207,0,
  67,34,17,18,52,210,0,67,33,17,35,64,197,0,202,0,
  68,67,33,194,17,35,64,205,0,4,50,195,17,35,64,208,
  0,4,50,194,17,35,64,197,0,200,0,4,68,51,50,195,
  17,35,64,204,0,4,68,50,195,17,35,64,208,0,67,33,
  194,17,18,52,197,0,200,0,67,51,194,34,195,17,35,52,
  203,0,4,67,51,50,195,17,35,64,207,0,4,50,195,17,
  18,52,197,0,199,0,4,50,34,196,17,18,50,35,64,202,
  0,67,50,194,34,194,17,18,52,208,0,4,50,195,17,35,
  64,197,0,199,0,67,33,197,17,35,33,18,52,201,0,4,
  50,33,196,17,18,52,207,0,4,67,33,194,17,18,52,198,
  0,199,0,67,33,196,17,18,51,33,18,52,201,0,67,33,
  197,17,35,64,207,0,67,50,195,17,35,64,198,0,199,0,
  67,33,18,34,194,17,18,51,33,35,64,201,0,67,33,196,
  17,18,52,0,4,64,204,0,4,50,33,194,17,18,52,199,
  0,198,0,4,50,17,18,33,195,17,34,17,35,64,201,0,
  67,33,196,17,18,52,68,67,52,204,0,67,33,195,17,18,
  52,199,0,198,0,4,50,17,34,196,17,34,17,35,64,200,
  0,4,50,17,18,195,17,18,52,51,50,35,64,203,0,67,
  33,196,17,35,64,198,0,198,0,4,50,17,34,196,17,34,
  17,35,64,200,0,4,50,17,18,195,17,18,51,34,33,18,
  52,203,0,67,33,196,17,35,64,198,0,198,0,67,33,17,
  34,198,17,35,64,200,0,4,50,17,34,195,17,18,34,195,
  17,35,64,202,0,67,33,195,17,18,52,199,0,198,0,67,
  33,17,34,197,17,18,52,201,0,4,50,17,34,199,17,34,
  52,203,0,67,33,195,17,18,52,199,0,198,0,67,33,18,
  50,195,17,34,18,35,64,201,0,4,50,17,34,198,17,34,
  51,64,202,0,4,50,196,17,18,51,64,198,0,198,0,67,
  33,18,50,195,17,194,35,52,202,0,4,50,17,34,197,17,
  34,51,68,203,0,4,50,197,17,34,52,64,197,0,198,0,
  4,50,35,50,195,17,35,52,64,202,0,4,50,17,34,195,
  17,194,34,51,68,204,0,67,33,198,17,35,52,197,0,199,
  0,67,52,50,195,17,18,52,64,203,0,67,34,50,195,17,
  35,51,68,205,0,67,33,195,17,18,194,17,18,35,64,196,
  0,199,0,4,68,50,196,17,35,52,203,0,4,51,33,195,
  17,35,68,205,0,4,50,196,17,18,34,194,17,18,52,196,
  0,200,0,4,50,196,17,18,35,64,202,0,4,50,196,17,
  18,52,64,204,0,4,50,197,17,35,34,33,17,35,64,195,
  0,200,0,67,33,197,17,18,52,202,0,4,50,197,17,35,
  52,204,0,67,33,18,196,17,18,51,50,34,52,196,0,199,
  0,4,50,199,17,35,64,201,0,4,50,197,17,18,35,64,
  203,0,67,33,34,197,17,35,67,51,64,196,0,199,0,4,
  50,199,17,18,52,201,0,4,50,198,17,18,52,202,0,194,
  4,194,50,197,17,18,52,68,197,0,199,0,4,50,200,17,
  35,64,200,0,4,50,199,17,35,64,201,0,67,64,67,50,
  198,17,35,64,197,0,199,0,4,50,194,17,18,34,33,195,
  17,35,64,200,0,4,50,199,17,35,64,200,0,68,50,52,
  4,50,198,17,18,52,197,0,199,0,4,50,194,17,18,51,
  50,34,194,17,18,52,196,0,4,68,64,0,67,33,195,17,
  194,34,194,17,18,52,199,0,4,51,33,35,67,51,33,197,
  17,18,52,197,0,198,0,4,67,33,194,17,35,68,67,51,
  33,17,35,64,195,0,4,67,51,52,68,67,33,194,17,18,
  194,51,33,17,18,52,199,0,67,34,17,35,50,34,33,194,
  17,34,195,17,35,64,196,0,195,0,4,0,68,67,50,33,
  17,18,52,0,4,67,33,17,35,64,195,0,67,50,34,35,
  194,51,33,194,17,18,52,68,50,17,18,52,198,0,4,50,
  194,17,18,33,196,17,35,33,194,17,35,64,196,0,195,0,
  67,68,51,50,33,194,17,18,52,194,0,67,33,18,52,195,
  0,4,50,33,17,18,194,34,33,194,17,35,64,4,50,17,
  18,52,198,0,4,50,200,17,35,50,33,17,35,64,196,0,
  194,0,4,50,51,34,33,195,17,35,64,0,4,50,17,18,
  52,195,0,67,33,200,17,35,64,4,50,17,18,52,198,0,
  67,33,17,194,34,197,17,35,67,50,17,35,64,196,0,194,
  0,67,33,34,196,17,18,52,194,0,4,50,17,18,52,195,
  0,67,33,199,17,18,52,0,4,50,17,18,52,198,0,4,
  50,34,194,51,34,195,17,18,52,4,67,33,18,52,196,0,
  0,4,50,197,17,18,35,64,194,0,4,50,17,18,52,194,
  0,4,50,17,194,34,33,196,17,35,64,194,0,67,33,18,
  52,199,0,67,51,194,68,51,33,194,17,18,52,0,67,33,
  18,52,196,0,0,67,33,195,17,18,34,35,52,195,0,4,
  50,17,35,64,194,0,67,33,18,194,51,50,34,33,194,17,
  35,64,194,0,67,33,18,52,199,0,4,68,194,0,68,50,
  34,17,35,64,0,67,33,18,52,196,0,4,50,195,17,34,
  35,51,52,64,195,0,4,50,17,35,64,194,0,4,50,18,
  52,68,67,51,50,194,34,52,195,0,67,33,18,52,64,203,
  0,67,51,34,52,194,0,67,33,18,52,196,0,67,33,194,
  17,34,51,52,68,64,196,0,67,33,17,35,52,195,0,67,
  35,64,0,4,68,67,194,51,64,195,0,67,33,18,51,52,
  203,0,4,68,51,64,194,0,67,33,18,52,196,0,67,33,
  17,34,51,68,64,198,0,67,33,17,18,35,64,194,0,4,
  52,196,0,4,194,68,196,0,67,33,17,34,35,64,204,0,
  68,195,0,67,33,18,52,68,195,0,67,33,18,51,68,200,
  0,4,50,33,17,18,52,195,0,64,203,0,67,33,194,17,
  18,52,208,0,67,33,18,194,51,64,194,0,67,33,35,68,
  202,0,67,50,33,18,52,207,0,67,33,195,17,35,64,207,
  0,67,33,17,194,34,52,194,0,4,50,52,203,0,4,67,
  50,35,64,207,0,4,50,195,34,52,208,0,67,33,195,17,
  35,64,0,0,67,64,204,0,4,67,52,209,0,67,195,51,
  64,208,0,4,50,194,17,18,52,194,0,0,4,206,0,4,
  64,209,0,4,195,68,210,0,67,194,34,35,64,194,0,249,
  0,4,194,51,52,195,0,250,0,194,68,64,195,0,255,0,
  0,255,0,0,203,0,68,244,0,202,0,4,51,64,237,0,
  68,197,0,201,0,4,67,34,52,222,0,68,205,0,68,51,
  64,196,0,201,0,67,50,17,35,64,220,0,68,51,64,203,
  0,4,51,34,52,196,0,200,0,4,50,33,17,35,64,219,
  0,4,51,34,52,203,0,67,34,17,35,64,195,0,200,0,
  67,33,194,17,18,52,219,0,67,34,17,35,64,201,0,4,
  50,194,17,35,64,195,0,199,0,4,50,195,17,18,52,218,
  0,4,50,194,17,18,52,201,0,67,33,194,17,18,52,195,
  0,199,0,4,50,195,17,18,52,218,0,4,50,194,17,18,
  52,201,0,67,33,194,17,18,52,195,0,199,0,67,33,195,
  17,35,64,218,0,67,33,194,17,18,52,200,0,4,51,33,
  194,17,18,52,195,0,198,0,4,50,195,17,34,52,219,0,
  67,33,194,17,18,52,200,0,67,34,195,17,35,64,195,0,
  198,0,67,33,194,17,18,51,64,218,0,4,50,195,17,35,
  64,199,0,4,50,195,17,34,52,194,68,194,0,198,0,67,
  33,195,17,35,64,218,0,67,33,194,17,18,52,200,0,67,
  33,195,17,35,68,194,51,64,0,197,0,4,50,196,17,35,
  64,218,0,67,33,194,17,35,64,199,0,4,50,196,17,35,
  67,194,34,52,0,197,0,4,50,196,17,35,64,217,0,4,
  50,195,17,35,64,199,0,67,33,196,17,35,50,194,17,35,
  64,197,0,4,50,196,17,35,64,217,0,4,50,195,17,18,
  52,68,197,0,4,50,197,17,34,33,17,34,52,0,197,0,
  4,50,195,17,18,52,218,0,67,33,195,17,18,52,51,64,
  196,0,67,33,197,17,33,17,34,51,64,0,197,0,4,50,
  195,17,18,52,218,0,67,33,195,17,18,51,34,52,195,0,
  4,50,199,17,18,51,68,194,0,197,0,4,50,195,17,35,
  64,217,0,4,50,197,17,34,17,35,64,194,0,4,50,199,
  17,35,68,195,0,197,0,4,50,195,17,35,64,217,0,4,
  50,199,17,18,52,195,0,67,33,197,17,34,52,196,0,197,
  0,67,33,195,17,35,64,217,0,67,33,199,17,18,52,195,
  0,67,33,196,17,34,51,64,196,0,197,0,67,33,195,17,
  35,64,217,0,67,33,195,17,18,195,34,35,64,195,0,67,
  33,196,17,35,68,197,0,197,0,67,33,195,17,18,52,217,
  0,67,33,195,17,35,195,51,52,196,0,67,33,195,17,18,
  52,198,0,196,0,4,50,196,17,18,52,217,0,67,33,195,
  17,35,195,68,64,195,0,4,50,196,17,18,52,68,197,0,
  196,0,4,50,196,17,18,52,217,0,67,33,195,17,35,64,
  198,0,4,50,197,17,35,51,64,196,0,196,0,64,67,33,
  196,17,35,64,216,0,67,33,195,17,18,52,198,0,4,50,
  197,17,18,34,52,196,0,195,0,4,52,67,33,196,17,35,
  64,216,0,67,33,196,17,35,64,198,0,67,33,198,17,35,
  68,195,0,194,0,4,67,35,68,50,196,17,18,52,216,0,
  67,33,196,17,18,52,198,0,4,50,198,17,18,51,64,194,
  0,194,0,67,50,18,51,50,196,17,18,52,216,0,67,33,
  196,17,18,35,64,197,0,4,50,199,17,34,52,194,0,0,
  4,50,33,17,34,51,33,196,17,35,64,215,0,67,33,197,
  17,18,52,197,0,4,50,200,17,35,64,0,0,67,33,195,
  17,34,33,196,17,35,64,215,0,4,50,34,197,17,35,64,
  197,0,67,33,199,17,35,64,0,4,50,202,17,35,64,215,
  0,67,194,34,18,196,17,35,64,197,0,67,33,17,18,34,
  33,194,17,18,52,194,0,67,33,18,195,34,196,17,33,17,
  35,64,214,0,4,50,194,17,18,34,195,17,18,52,197,0,
  67,33,17,18,33,195,17,35,64,194,0,4,50,35,195,51,
  34,195,17,33,17,35,64,214,0,4,50,199,17,18,52,197,
  0,67,33,17,18,194,17,18,34,52,195,0,0,67,52,195,
  68,51,33,17,18,50,17,35,64,214,0,4,50,199,17,35,
  64,197,0,67,33,196,17,35,51,64,195,0,0,4,64,195,
  0,68,50,33,35,50,17,35,64,214,0,67,33,197,17,18,
  34,52,198,0,67,33,195,17,18,52,68,196,0,199,0,67,
  50,52,50,17,35,64,214,0,67,33,18,34,17,18,34,35,
  51,64,197,0,4,50,195,17,18,35,64,197,0,199,0,4,
  67,68,50,17,35,64,214,0,67,33,18,33,17,18,51,52,
  68,198,0,67,33,195,17,35,52,198,0,200,0,194,4,50,
  17,35,64,214,0,67,33,18,33,17,18,52,64,198,0,4,
  50,195,17,34,52,64,198,0,201,0,4,50,17,35,64,214,
  0,4,50,35,33,17,18,52,199,0,67,33,194,17,18,51,
  64,199,0,201,0,4,50,17,35,64,215,0,67,50,194,17,
  18,52,198,0,4,67,33,194,17,35,68,200,0,201,0,4,
  50,17,35,68,215,0,4,50,194,17,35,64,198,0,67,50,
  194,17,18,52,201,0,201,0,4,50,17,34,51,64,214,0,
  67,33,194,17,35,64,197,0,4,50,33,194,17,35,64,201,
  0,201,0,4,50,194,17,34,52,214,0,67,33,17,18,52,
  198,0,67,33,194,17,34,52,202,0,201,0,4,50,195,17,
  35,64,212,0,4,50,194,17,18,52,198,0,4,50,194,17,
  35,64,202,0,202,0,67,195,34,52,213,0,67,33,194,17,
  34,52,199,0,67,34,17,35,64,202,0,202,0,4,195,51,
  64,213,0,4,50,195,17,35,64,198,0,4,51,33,18,52,
  202,0,203,0,195,68,215,0,67,34,194,17,18,52,199,0,
  68,50,17,35,64,201,0,229,0,4,51,194,34,35,64,200,
  0,67,34,52,202,0,198,0,4,68,222,0,68,194,51,52,
  201,0,4,51,64,202,0,197,0,4,67,51,64,222,0,194,
  68,64,202,0,68,203,0,197,0,67,50,34,52,247,0,196,
  0,4,50,33,17,35,64,246,0,196,0,67,33,194,17,35,
  52,246,0,195,0,4,50,195,17,18,35,64,245,0,196,0,
  67,194,34,194,17,18,52,245,0,196,0,4,194,51,34,194,
  17,35,64,197,0,64,0,194,68,64,234,0,197,0,194,68,
  50,194,17,18,52,196,0,68,52,68,194,51,52,217,0,64,
  208,0,199,0,67,33,194,17,35,68,194,0,4,51,35,51,
  194,34,35,64,215,0,4,52,208,0,199,0,4,50,194,17,
  35,51,194,68,67,34,18,34,194,17,18,52,215,0,67,35,
  64,207,0,199,0,4,50,194,17,18,34,194,51,50,198,17,
  35,64,213,0,4,50,18,52,207,0,200,0,67,33,195,17,
  195,34,33,197,17,18,52,213,0,67,33,17,35,64,206,0,
  200,0,67,33,199,17,194,34,195,17,18,52,213,0,4,50,
  17,18,52,206,0,200,0,4,50,201,17,194,34,194,17,35,
  64,213,0,67,33,17,35,194,68,64,203,0,201,0,67,33,
  204,17,35,64,213,0,67,33,18,52,67,51,52,64,202,0,
  201,0,4,50,204,17,35,68,64,212,0,67,33,18,52,50,
  34,35,52,194,68,64,199,0,201,0,68,67,34,203,17,18,
  51,52,212,0,67,33,18,51,33,17,18,35,194,51,52,199,
  0,200,0,4,51,68,51,34,33,201,17,18,34,35,64,211,
  0,67,33,18,34,195,17,18,194,34,35,68,198,0,200,0,
  67,34,51,68,51,50,203,17,18,52,206,0,4,196,68,50,
  201,17,18,51,194,68,196,0,199,0,4,50,17,34,51,68,
  50,195,17,34,200,17,35,64,205,0,67,196,51,50,202,17,
  34,194,51,68,195,0,199,0,67,33,194,17,34,51,50,194,
  17,18,51,34,33,198,17,18,52,204,0,4,50,197,34,203,
  17,194,34,51,64,194,0,198,0,4,50,196,17,194,34,194,
  17,18,52,51,33,198,17,18,52,204,0,67,33,210,17,34,
  52,194,0,198,0,67,33,197,17,18,194,17,18,52,68,50,
  17,194,34,33,194,17,35,64,203,0,4,50,212,17,35,64,
  0,198,0,67,33,17,18,198,17,35,64,0,67,33,18,35,
  50,33,18,52,204,0,4,50,202,17,18,33,200,17,18,52,
  0,198,0,67,33,17,35,33,197,17,35,64,0,67,33,17,
  18,35,50,35,64,204,0,4,50,17,194,34,197,17,18,34,
  35,50,200,17,18,52,0,198,0,67,33,34,52,50,33,196,
  17,35,64,0,67,33,194,17,18,51,52,205,0,4,50,17,
  35,51,194,34,195,17,35,51,52,67,196,34,33,196,17,35,
  64,198,0,4,50,51,64,67,50,33,194,17,18,52,194,0,
  4,50,194,17,18,52,64,205,0,4,50,18,52,68,51,50,
  194,17,18,52,68,64,4,196,51,50,33,194,17,18,52,0,
  199,0,67,68,0,4,67,50,33,17,18,52,195,0,67,34,
  17,35,64,207,0,67,35,64,0,68,67,33,17,35,64,195,
  0,196,68,67,50,194,17,35,64,0,199,0,4,195,0,4,
  67,50,34,35,64,195,0,4,51,34,52,208,0,4,52,195,
  0,4,50,34,52,200,0,4,67,194,34,52,194,0,204,0,
  4,67,51,52,197,0,68,51,64,209,0,64,196,0,67,51,
  64,201,0,4,194,51,64,194,0,205,0,4,68,64,198,0,
  68,215,0,4,68,203,0,194,68,195,0
}

RES_PCX_SKYLINE_FRONT =
{
  10,5,1,4,0,0,0,0,114,1,56,0,0,0,0,0,
  0,0,0,20,52,100,255,255,255,36,159,222,188,74,155,121,
  58,128,232,106,115,245,160,151,250,214,184,254,243,192,0,255,
  0,190,83,149,64,94,153,0,255,0,0,255,0,94,94,94,
  0,1,186,0,1,0,0,0,0,0,0,0,0,0,0,0,
  0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,
  0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,
  0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,
  255,0,255,0,201,0,16,242,0,255,0,255,0,201,0,16,
  242,0,255,0,255,0,201,0,16,242,0,255,0,255,0,201,
  0,16,242,0,255,0,242,0,196,17,210,0,16,242,0,255,
  0,217,0,16,212,0,201,17,16,199,0,1,200,17,16,242,
  0,255,0,217,0,193,240,212,0,201,17,16,199,0,1,193,
  241,17,193,241,194,17,194,241,17,16,242,0,255,0,217,0,
  193,240,212,0,196,17,31,196,255,193,240,199,0,1,200,17,
  16,242,0,255,0,217,0,16,212,0,201,17,16,199,0,1,
  200,241,17,242,0,255,0,217,0,16,212,0,196,17,31,196,
  255,193,240,199,0,1,201,17,242,0,255,0,217,0,16,212,
  0,201,17,16,199,0,1,193,241,31,198,241,17,242,0,255,
  0,217,0,16,212,0,196,17,31,196,255,193,240,199,0,1,
  201,17,16,241,0,255,0,217,0,16,212,0,201,17,16,199,
  0,1,201,241,16,241,0,255,0,217,0,16,212,0,196,17,
  31,196,255,193,240,199,0,1,201,17,16,241,0,255,0,217,
  0,16,212,0,201,17,16,199,0,1,195,241,17,197,241,17,
  241,0,255,0,205,0,1,203,0,16,212,0,196,17,31,196,
  255,193,240,199,0,1,201,17,16,241,0,255,0,205,0,15,
  203,0,16,212,0,201,17,16,199,0,1,197,241,17,195,241,
  17,241,0,255,0,205,0,1,203,0,16,212,0,196,17,31,
  196,255,193,240,199,0,1,197,17,193,241,195,17,16,241,0,
  255,0,205,0,1,202,0,1,17,212,0,201,17,16,199,0,
  1,201,241,17,241,0,255,0,205,0,194,1,200,0,196,17,
  16,210,0,196,17,31,196,255,193,240,199,0,1,202,17,241,
  0,255,0,205,0,194,1,200,0,17,194,31,17,16,210,0,
  201,17,16,199,0,1,193,241,17,199,241,17,241,0,255,0,
  205,0,194,1,200,0,196,17,16,210,0,196,17,31,196,255,
  193,240,199,0,1,198,17,194,241,194,17,241,0,255,0,204,
  0,196,17,199,0,31,17,31,17,16,210,0,201,17,16,199,
  0,1,198,241,17,194,241,17,241,0,225,0,1,17,16,227,
  0,1,201,17,197,0,196,17,16,210,0,196,17,31,196,255,
  193,240,199,0,1,202,17,207,0,195,17,223,0,224,0,1,
  195,17,16,226,0,15,193,255,31,193,241,198,17,197,0,31,
  17,194,31,16,210,0,201,17,16,199,0,1,194,241,17,198,
  241,17,207,0,195,17,223,0,224,0,1,195,17,16,226,0,
  1,200,17,16,197,0,196,17,16,210,0,196,17,31,196,255,
  193,240,199,0,1,202,17,206,0,197,17,222,0,223,0,199,
  17,225,0,15,194,255,193,241,198,17,197,0,196,31,16,210,
  0,201,17,16,199,0,1,201,241,17,206,0,196,17,193,241,
  222,0,223,0,198,17,193,241,225,0,1,201,17,197,0,196,
  17,16,210,0,196,17,31,196,255,193,240,199,0,1,202,17,
  206,0,196,17,31,197,0,1,196,17,212,0,223,0,1,17,
  195,31,193,241,17,16,224,0,15,194,255,193,241,197,17,16,
  197,0,31,17,194,31,16,210,0,201,17,16,199,0,1,201,
  241,17,206,0,196,17,193,241,197,0,1,193,241,195,17,212,
  0,223,0,198,17,193,241,16,224,0,1,201,17,197,0,31,
  17,31,17,16,210,0,197,17,196,255,193,240,199,0,1,202,
  17,16,205,0,196,17,31,197,0,1,196,17,212,0,223,0,
  1,193,241,17,194,31,193,241,17,225,0,15,193,255,194,241,
  198,17,197,0,17,31,17,31,16,210,0,201,17,16,199,0,
  1,17,200,241,17,1,16,204,0,196,17,193,241,197,0,1,
  194,241,194,17,212,0,223,0,198,17,193,241,16,224,0,1,
  200,17,16,197,0,196,31,16,210,0,198,17,195,255,193,240,
  199,0,1,202,17,0,1,17,203,0,196,17,31,197,0,1,
  196,17,212,0,223,0,1,17,193,255,17,31,193,241,17,225,
  0,15,193,241,193,255,193,241,198,17,197,0,196,17,16,210,
  0,201,17,16,199,0,1,197,241,193,255,195,241,17,195,0,
  17,16,201,0,17,194,241,17,193,241,197,0,1,194,241,194,
  17,212,0,223,0,198,17,193,241,16,224,0,1,201,17,197,
  0,17,195,31,16,210,0,199,17,194,255,193,240,199,0,1,
  202,17,196,0,1,17,1,199,0,196,17,31,197,0,1,196,
  17,212,0,223,0,1,193,241,193,255,194,31,193,241,17,219,
  0,16,1,196,0,15,194,255,193,241,197,17,16,197,0,17,
  195,31,16,210,0,201,17,16,199,0,1,17,196,241,31,195,
  241,17,198,0,195,17,195,16,194,0,17,194,241,17,193,241,
  197,0,1,195,241,17,212,0,208,0,1,197,17,201,0,198,
  17,193,241,16,218,0,17,1,16,195,0,1,201,17,197,0,
  196,17,16,210,0,200,17,193,255,193,240,199,0,1,202,17,
  201,0,1,200,17,31,197,0,1,196,17,212,0,208,0,1,
  193,241,196,17,16,200,0,1,17,193,255,194,31,193,241,17,
  218,0,1,195,17,16,194,0,15,194,255,193,241,198,17,197,
  0,194,31,17,31,16,210,0,201,17,16,199,0,1,196,241,
  193,255,196,241,17,206,0,17,194,241,17,193,241,197,0,1,
  193,241,194,17,193,241,212,0,208,0,1,196,17,31,17,200,
  0,198,17,193,241,16,217,0,1,195,17,16,194,0,1,200,
  17,16,197,0,196,17,16,210,0,200,17,31,193,240,199,0,
  1,202,17,206,0,196,17,31,197,0,1,196,17,212,0,208,
  0,1,194,241,195,17,193,241,200,0,1,193,241,193,255,194,
  31,193,241,17,218,0,1,31,17,31,16,194,0,15,193,255,
  31,193,241,198,17,197,0,195,31,17,16,195,0,194,17,205,
  0,201,17,16,199,0,1,200,241,194,17,206,0,194,17,193,
  241,17,193,241,197,0,1,195,17,193,241,212,0,208,0,1,
  196,17,31,17,200,0,198,17,193,241,16,217,0,1,193,241,
  194,17,16,194,0,1,201,17,197,0,31,17,31,17,16,194,
  0,1,194,17,205,0,201,17,193,240,195,0,1,206,17,206,
  0,196,17,31,197,0,1,196,17,212,0,208,0,1,195,241,
  194,17,193,241,200,0,1,17,193,255,194,31,193,241,17,218,
  0,1,31,17,31,16,194,0,15,194,255,193,241,197,17,16,
  197,0,17,31,17,31,16,194,0,196,17,204,0,201,17,16,
  195,0,1,204,255,193,241,17,206,0,196,17,193,241,197,0,
  1,194,241,17,193,241,212,0,208,0,1,196,17,31,17,200,
  0,198,17,193,241,16,202,0,1,17,16,204,0,1,193,241,
  194,17,16,194,0,1,201,17,197,0,196,31,16,194,0,195,
  17,193,255,204,0,201,17,16,195,0,1,206,17,206,0,196,
  17,31,197,0,1,196,17,194,0,1,17,16,207,0,208,0,
  1,17,193,241,195,17,193,241,200,0,1,194,241,194,31,193,
  241,17,203,0,194,17,16,204,0,1,31,17,193,255,16,194,
  0,15,194,255,193,241,198,17,197,0,196,17,16,194,0,196,
  17,194,0,1,194,17,199,0,201,17,16,195,0,1,203,255,
  31,193,241,17,206,0,196,17,193,241,197,0,1,194,241,199,
  17,16,206,0,208,0,1,196,17,31,17,200,0,198,17,193,
  241,16,201,0,1,195,17,16,203,0,1,193,241,194,17,16,
  194,0,1,200,17,16,197,0,196,31,16,194,0,195,17,193,
  255,194,0,1,194,17,0,194,17,16,195,0,201,17,16,195,
  0,1,206,17,195,0,1,195,17,16,198,0,196,17,31,197,
  0,1,198,17,31,194,255,193,240,206,0,208,0,1,194,241,
  195,17,193,241,200,0,1,194,241,17,31,193,241,17,202,0,
  1,194,17,31,193,240,203,0,1,31,17,31,16,194,0,15,
  31,193,255,193,241,198,17,197,0,196,17,16,0,1,196,17,
  194,0,1,194,17,0,17,193,255,193,240,195,0,201,17,16,
  195,0,1,204,255,193,241,17,195,0,1,17,194,255,193,240,
  198,0,196,17,193,241,197,0,1,195,241,198,17,16,206,0,
  208,0,1,196,17,31,17,200,0,198,17,193,241,16,201,0,
  1,195,17,16,194,0,194,17,16,198,0,1,193,241,194,17,
  16,194,0,1,201,17,197,0,195,31,17,16,0,1,195,17,
  193,255,194,0,1,194,17,16,194,17,16,194,17,0,201,17,
  16,195,0,1,206,17,195,0,1,195,17,16,198,0,196,17,
  31,197,0,1,198,17,31,194,255,193,240,194,17,16,203,0,
  208,0,1,194,241,195,17,193,241,200,0,1,193,241,194,17,
  31,193,241,197,17,0,194,17,1,194,0,1,194,17,31,193,
  240,194,0,194,17,16,1,194,17,195,0,1,194,17,193,255,
  16,194,0,15,194,255,201,17,16,194,0,31,17,31,17,16,
  0,1,196,17,0,198,17,193,255,193,241,194,17,0,201,17,
  16,195,0,1,194,255,193,241,193,255,31,199,255,193,241,17,
  195,0,1,17,31,193,255,193,240,0,195,17,194,0,200,17,
  194,0,1,195,241,198,17,16,31,17,16,203,0,208,0,1,
  198,17,200,0,198,17,193,241,17,31,193,255,193,241,1,195,
  17,16,0,196,17,16,194,0,194,17,16,1,194,241,195,0,
  1,195,17,16,194,0,1,198,17,31,17,194,31,17,16,194,
  0,202,17,193,255,0,202,17,16,203,17,16,0,1,206,17,
  195,0,1,195,17,16,197,17,0,198,17,194,241,194,0,1,
  198,17,31,194,255,193,240,17,31,16,203,0,208,0,1,193,
  241,196,17,193,241,196,0,195,17,0,1,193,241,17,194,31,
  193,241,197,17,1,195,17,16,0,195,17,31,193,240,194,0,
  195,17,1,194,17,1,17,16,1,194,17,31,16,194,0,15,
  193,255,202,17,16,194,0,31,17,31,194,17,0,204,17,193,
  255,193,241,194,17,16,202,17,31,193,240,1,209,17,0,1,
  194,17,193,255,193,240,194,17,195,241,0,200,17,194,0,1,
  196,241,197,17,16,31,17,16,203,0,208,0,1,198,17,196,
  0,195,17,0,198,17,193,241,194,17,31,193,241,1,196,17,
  16,196,17,16,1,198,17,193,241,194,17,16,1,195,17,16,
  194,0,1,197,17,194,31,17,195,31,16,194,0,196,17,16,
  0,1,195,17,193,255,203,17,16,203,17,16,1,208,255,193,
  241,0,1,195,17,16,197,17,0,199,17,193,241,194,0,1,
  198,17,31,194,255,193,240,17,31,16,203,0,208,0,1,193,
  241,196,17,193,241,194,0,199,17,193,241,201,17,1,196,17,
  16,196,17,193,240,1,202,17,1,194,17,193,255,16,194,0,
  15,193,241,202,17,16,194,0,17,31,194,17,16,0,1,203,
  17,193,255,193,241,194,17,16,202,17,31,193,240,1,209,17,
  0,1,194,17,31,193,240,195,17,194,241,0,200,17,194,0,
  1,193,241,200,17,16,31,17,16,196,0,1,198,0,194,0,
  1,203,0,1,218,17,193,241,210,17,193,241,208,17,194,31,
  195,17,31,205,17,193,255,217,17,193,255,31,206,255,193,241,
  210,17,194,241,210,17,16,0,1,198,0,0,1,17,16,201,
  0,194,1,243,17,194,31,195,17,31,224,17,31,193,241,205,
  17,31,193,241,214,17,31,193,241,196,17,193,241,204,17,193,
  241,201,17,31,199,17,16,197,0,0,1,17,193,241,195,0,
  1,196,0,1,220,17,193,241,227,17,31,210,17,31,217,17,
  203,255,193,241,193,255,31,194,255,193,241,209,17,195,241,214,
  17,195,0,16,0,0,196,17,16,0,250,17,195,31,229,17,
  193,241,205,17,31,193,241,215,17,193,241,231,17,16,0,197,
  17,194,0,1,0,16,1,0,221,17,193,241,255,17,209,17,
  196,255,193,241,193,255,31,201,255,193,241,234,17,16,1,0,
  17,0,196,17,16,194,0,1,0,16,194,1,255,17,255,17,
  236,17,1,0,194,16
}

RES_PCX_SKYLINE_BACK =
{
  10,5,1,4,0,0,0,0,87,1,60,0,0,0,0,0,
  0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,
  0,0,0,0,0,0,0,0,0,0,0,12,25,22,25,51,
  45,37,86,46,70,130,50,0,0,0,0,0,0,0,0,0,
  0,1,172,0,1,0,0,0,0,0,0,0,0,0,0,0,
  0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,
  0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,
  0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,
  255,0,204,0,12,255,0,225,0,255,0,204,0,12,255,0,
  225,0,255,0,204,0,12,255,0,225,0,255,0,204,0,12,
  255,0,225,0,255,0,204,0,193,204,193,192,255,0,195,0,
  12,220,0,255,0,204,0,193,204,193,192,255,0,195,0,12,
  220,0,255,0,204,0,193,204,193,192,255,0,195,0,12,220,
  0,255,0,204,0,193,204,193,192,255,0,195,0,12,220,0,
  255,0,203,0,12,194,204,255,0,195,0,12,220,0,255,0,
  202,0,194,12,194,204,12,255,0,194,0,12,220,0,255,0,
  202,0,194,12,194,204,12,255,0,194,0,12,220,0,230,0,
  12,226,0,197,204,193,192,222,0,194,204,193,192,200,0,12,
  214,0,193,204,193,192,219,0,230,0,193,204,193,192,225,0,
  197,204,193,192,221,0,12,194,204,193,192,200,0,12,214,0,
  193,204,193,192,219,0,229,0,12,194,204,225,0,197,204,193,
  192,218,0,12,198,204,193,192,199,0,12,214,0,193,204,193,
  192,219,0,229,0,195,204,193,192,224,0,197,204,193,192,218,
  0,12,198,204,193,192,199,0,12,213,0,194,204,193,192,219,
  0,228,0,197,204,193,192,223,0,197,204,193,192,196,0,195,
  12,211,0,12,198,204,193,192,198,0,12,194,204,211,0,12,
  195,204,219,0,228,0,12,196,204,224,0,197,204,193,192,194,
  0,197,204,193,192,210,0,12,198,204,193,192,198,0,195,204,
  193,192,210,0,196,204,219,0,208,0,194,204,193,192,208,0,
  199,204,193,192,222,0,197,204,193,192,194,0,197,204,193,192,
  210,0,12,198,204,193,192,195,0,12,200,204,207,0,12,196,
  204,219,0,207,0,12,194,204,193,192,208,0,199,204,193,192,
  222,0,197,204,193,192,194,0,197,204,193,192,210,0,12,198,
  204,193,192,195,0,12,200,204,207,0,197,204,219,0,207,0,
  195,204,193,192,207,0,12,200,204,222,0,197,204,193,192,194,
  0,197,204,193,192,210,0,12,198,204,193,192,195,0,12,200,
  204,206,0,198,204,193,192,218,0,206,0,12,195,204,193,192,
  207,0,12,200,204,222,0,197,204,193,192,194,0,197,204,193,
  192,210,0,12,198,204,193,192,195,0,12,200,204,206,0,198,
  204,219,0,206,0,196,204,193,192,207,0,12,200,204,222,0,
  197,204,193,192,194,0,197,204,193,192,210,0,12,198,204,193,
  192,195,0,12,200,204,206,0,198,204,193,192,218,0,205,0,
  12,196,204,193,192,207,0,12,200,204,221,0,199,204,194,0,
  197,204,193,192,210,0,12,198,204,193,192,195,0,12,200,204,
  206,0,198,204,219,0,205,0,197,204,193,192,207,0,12,200,
  204,221,0,199,204,194,0,197,204,193,192,210,0,12,198,204,
  193,192,195,0,12,200,204,206,0,198,204,193,192,218,0,204,
  0,12,197,204,193,192,207,0,12,200,204,221,0,199,204,194,
  0,197,204,193,192,210,0,12,198,204,193,192,195,0,12,200,
  204,206,0,198,204,193,192,218,0,202,0,200,204,193,192,207,
  0,12,200,204,196,0,204,204,205,0,199,204,194,0,197,204,
  193,192,210,0,12,198,204,193,192,195,0,12,200,204,197,0,
  12,197,204,195,0,198,204,193,192,218,0,202,0,200,204,193,
  192,207,0,12,200,204,196,0,12,202,204,193,192,205,0,199,
  204,194,0,197,204,193,192,201,0,193,192,200,0,12,198,204,
  193,192,195,0,12,200,204,197,0,12,197,204,195,0,198,204,
  193,192,218,0,202,0,200,204,193,192,204,0,193,192,194,0,
  12,200,204,196,0,204,204,205,0,199,204,194,0,197,204,193,
  192,201,0,193,192,200,0,12,198,204,193,192,195,0,12,200,
  204,197,0,12,197,204,195,0,198,204,193,192,218,0,202,0,
  200,204,193,192,204,0,193,192,194,0,12,200,204,196,0,204,
  204,205,0,199,204,194,0,197,204,193,192,200,0,195,204,199,
  0,12,198,204,193,192,195,0,12,200,204,197,0,12,197,204,
  195,0,198,204,193,192,218,0,202,0,200,204,193,192,204,0,
  193,192,194,0,12,200,204,196,0,204,204,205,0,199,204,194,
  0,197,204,193,192,200,0,195,204,193,192,198,0,12,198,204,
  193,192,195,0,12,200,204,197,0,12,197,204,195,0,198,204,
  193,192,218,0,202,0,200,204,193,192,204,0,193,192,194,0,
  12,200,204,196,0,204,204,205,0,199,204,194,0,197,204,193,
  192,199,0,198,204,197,0,12,198,204,193,192,195,0,12,200,
  204,197,0,12,197,204,195,0,198,204,193,192,218,0,202,0,
  200,204,193,192,203,0,12,193,192,194,0,12,200,204,196,0,
  204,204,196,0,195,204,198,0,199,204,194,0,197,204,193,192,
  199,0,198,204,197,0,12,198,204,193,192,195,0,12,200,204,
  197,0,12,197,204,195,0,198,204,193,192,218,0,202,0,200,
  204,193,192,203,0,12,193,204,194,0,12,200,204,196,0,204,
  204,195,0,197,204,193,192,196,0,199,204,194,0,197,204,193,
  192,199,0,198,204,197,0,12,198,204,193,192,195,0,12,200,
  204,197,0,12,197,204,195,0,198,204,193,192,218,0,202,0,
  200,204,193,192,200,0,12,196,204,193,192,0,12,200,204,196,
  0,204,204,195,0,197,204,193,192,196,0,199,204,194,0,197,
  204,193,192,199,0,198,204,197,0,12,198,204,193,192,195,0,
  12,200,204,197,0,12,197,204,195,0,198,204,193,192,218,0,
  202,0,200,204,193,192,200,0,12,196,204,193,192,0,12,200,
  204,196,0,204,204,195,0,197,204,193,192,196,0,199,204,194,
  0,197,204,193,192,199,0,198,204,197,0,12,198,204,193,192,
  195,0,12,200,204,197,0,12,197,204,193,192,194,0,198,204,
  193,192,218,0,202,0,200,204,193,192,200,0,12,196,204,193,
  192,0,12,200,204,196,0,204,204,195,0,197,204,193,192,196,
  0,199,204,194,0,197,204,193,192,199,0,198,204,197,0,12,
  198,204,193,192,195,0,12,200,204,197,0,12,198,204,194,0,
  198,204,193,192,218,0,202,0,200,204,193,192,200,0,12,196,
  204,193,192,0,12,200,204,196,0,204,204,195,0,197,204,193,
  192,196,0,199,204,194,0,197,204,193,192,199,0,198,204,197,
  0,12,198,204,193,192,195,0,12,200,204,197,0,12,197,204,
  12,193,192,0,198,204,193,192,218,0,202,0,200,204,193,192,
  200,0,12,196,204,193,192,0,12,200,204,196,0,204,204,195,
  0,197,204,193,192,196,0,199,204,194,0,197,204,193,192,199,
  0,198,204,197,0,12,198,204,193,192,195,0,12,200,204,197,
  0,12,197,204,0,193,204,0,198,204,193,192,218,0,202,0,
  200,204,193,192,200,0,12,196,204,193,192,0,12,200,204,196,
  0,204,204,195,0,197,204,193,192,196,0,199,204,194,0,197,
  204,193,192,199,0,198,204,197,0,12,198,204,193,192,195,0,
  12,200,204,197,0,12,197,204,0,12,193,192,198,204,193,192,
  218,0,202,0,200,204,193,192,200,0,12,196,204,193,192,0,
  12,200,204,196,0,204,204,195,0,197,204,193,192,196,0,199,
  204,194,0,197,204,193,192,199,0,198,204,197,0,12,198,204,
  193,192,195,0,12,200,204,197,0,12,206,204,193,192,198,0,
  193,192,12,0,193,192,12,0,193,192,205,0,202,0,200,204,
  193,192,200,0,12,196,204,193,192,0,12,200,204,196,0,204,
  204,195,0,197,204,193,192,196,0,199,204,194,0,197,204,193,
  192,199,0,198,204,197,0,12,198,204,193,192,195,0,12,200,
  204,197,0,12,206,204,193,192,197,0,12,193,192,193,204,12,
  193,192,193,204,12,193,192,205,0,202,0,200,204,193,192,200,
  0,12,196,204,193,192,0,12,200,204,196,0,204,204,195,0,
  197,204,193,192,196,0,199,204,194,0,197,204,193,192,199,0,
  198,204,197,0,12,198,204,193,192,195,0,12,200,204,197,0,
  12,206,204,193,192,196,0,200,204,193,192,205,0,202,0,200,
  204,193,192,200,0,12,196,204,193,192,0,12,200,204,196,0,
  204,204,195,0,197,204,193,192,196,0,199,204,194,0,197,204,
  193,192,199,0,198,204,197,0,12,198,204,193,192,195,0,12,
  200,204,197,0,12,206,204,193,192,194,0,12,201,204,193,192,
  205,0,202,0,200,204,193,192,200,0,12,196,204,193,192,0,
  12,200,204,196,0,204,204,195,0,197,204,193,192,196,0,199,
  204,194,0,197,204,193,192,199,0,198,204,197,0,12,198,204,
  193,192,195,0,12,200,204,197,0,12,206,204,193,192,0,203,
  204,193,192,205,0,202,0,201,204,0,12,198,0,12,196,204,
  193,192,0,12,200,204,196,0,204,204,195,0,198,204,193,192,
  195,0,199,204,194,0,197,204,193,192,199,0,198,204,197,0,
  12,198,204,193,192,195,0,12,200,204,197,0,12,206,204,193,
  192,12,203,204,193,192,205,0,201,0,12,200,204,193,192,194,
  204,194,0,12,0,12,0,12,196,204,193,192,0,12,200,204,
  196,0,204,204,195,0,199,204,195,0,199,204,194,0,197,204,
  193,192,199,0,198,204,197,0,12,198,204,193,192,195,0,12,
  200,204,197,0,12,206,204,193,192,12,203,204,193,192,205,0,
  201,0,201,204,193,192,0,12,193,204,193,192,12,0,12,0,
  197,204,193,192,0,12,200,204,196,0,204,204,195,0,199,204,
  195,0,199,204,194,0,197,204,194,192,196,204,194,192,198,204,
  197,0,12,198,204,193,192,195,0,12,200,204,197,0,12,206,
  204,193,192,12,203,204,193,192,205,0,200,0,12,201,204,193,
  192,0,12,0,12,196,204,12,196,204,193,192,0,12,216,204,
  195,0,198,204,193,192,195,0,199,204,194,0,211,204,197,0,
  12,198,204,193,192,195,0,12,200,204,197,0,12,206,204,193,
  192,12,203,204,193,192,205,0,200,0,202,204,193,192,0,12,
  194,0,12,0,12,0,12,196,204,193,192,0,12,200,204,193,
  192,194,0,12,204,204,195,0,199,204,195,0,199,204,194,0,
  198,204,193,192,197,0,199,204,197,0,12,198,204,193,192,195,
  0,12,200,204,197,0,12,206,204,193,192,12,203,204,193,192,
  205,0,199,0,12,202,204,193,192,0,12,194,0,12,0,12,
  0,12,196,204,193,192,0,12,200,204,195,0,12,204,204,195,
  0,199,204,195,0,199,204,194,0,198,204,198,0,12,198,204,
  197,0,12,198,204,193,192,195,0,12,200,204,197,0,12,206,
  204,193,192,12,203,204,193,192,205,0,199,0,12,202,204,193,
  192,0,12,194,0,12,0,12,0,12,196,204,193,192,0,12,
  200,204,195,0,205,204,195,0,198,204,193,192,195,0,199,204,
  194,0,197,204,193,192,199,0,198,204,197,0,12,198,204,193,
  192,195,0,12,200,204,197,0,12,206,204,193,192,12,203,204,
  193,192,205,0,199,0,12,202,204,193,192,0,12,194,0,12,
  0,12,0,12,196,204,193,192,0,201,204,194,0,12,205,204,
  195,0,199,204,195,0,199,204,194,0,197,204,193,192,199,0,
  198,204,197,0,12,198,204,193,192,195,0,12,200,204,197,0,
  12,206,204,193,192,12,203,204,193,192,205,0,199,0,12,202,
  204,193,192,0,12,194,0,12,0,12,0,12,196,204,193,192,
  0,201,204,194,0,12,205,204,195,0,199,204,195,0,199,204,
  194,0,197,204,193,192,199,0,198,204,197,0,12,198,204,193,
  192,195,0,12,200,204,197,0,12,206,204,193,192,12,203,204,
  193,192,205,0,199,0,12,202,204,193,192,0,12,194,0,12,
  0,12,0,12,196,204,193,192,12,201,204,194,0,12,205,204,
  195,0,199,204,195,0,199,204,194,0,197,204,193,192,199,0,
  198,204,197,0,12,198,204,193,192,195,0,12,200,204,197,0,
  12,206,204,193,192,12,203,204,193,192,205,0,199,0,12,202,
  204,194,192,193,204,0,193,192,193,204,193,192,193,204,193,192,
  12,196,204,193,192,12,201,204,194,0,12,205,204,195,0,199,
  204,193,192,194,0,199,204,194,0,197,204,193,192,199,0,198,
  204,197,0,12,198,204,193,192,195,0,12,200,204,197,0,12,
  206,204,193,192,12,203,204,193,192,0,193,192,203,0,198,0,
  255,204,255,204,220,204,193,192,203,0,196,0,193,192,12,255,
  204,255,204,221,204,193,192,202,0,196,0,255,204,255,204,224,
  204,193,192,201,0,195,0,12,255,204,255,204,225,204,193,192,
  200,0,0,12,255,204,255,204,231,204,195,192,194,0,255,204,
  255,204,237,204,12
}

RES_PCX_SKYLINE_TREES =
{
  10,5,1,4,0,0,0,0,71,1,13,0,0,0,0,0,
  0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,
  0,0,0,0,0,0,0,0,0,0,0,12,25,22,25,51,
  45,37,86,46,70,130,50,0,0,0,0,0,0,0,0,0,
  0,1,164,0,1,0,0,0,0,0,0,0,0,0,0,0,
  0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,
  0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,
  0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,
  212,0,9,153,255,0,197,0,153,237,0,9,153,218,0,212,
  0,194,153,144,255,0,195,0,9,194,153,215,0,153,144,211,
  0,194,153,144,217,0,211,0,9,195,153,0,9,144,245,0,
  153,144,200,0,195,153,144,213,0,9,194,153,210,0,9,195,
  153,0,9,144,214,0,211,0,9,195,153,0,194,153,218,0,
  9,217,0,9,194,153,200,0,196,153,205,0,9,144,198,0,
  195,153,144,0,153,207,0,9,195,153,0,194,153,214,0,196,
  0,153,144,199,0,9,197,0,199,153,144,202,0,9,153,205,
  0,9,208,0,9,144,0,144,197,0,195,153,144,0,153,197,
  0,194,9,153,144,204,0,9,195,153,144,196,0,195,153,144,
  9,153,144,206,0,199,153,144,202,0,9,153,201,0,195,0,
  9,194,153,0,153,0,153,195,0,153,144,153,194,0,9,200,
  153,144,198,0,9,144,0,194,153,144,204,0,153,208,0,194,
  153,9,153,197,0,195,153,144,9,153,144,195,0,144,0,153,
  9,205,0,197,153,195,0,9,199,153,203,0,153,144,9,200,
  153,144,198,0,9,144,0,194,153,144,200,0,195,0,9,198,
  153,144,0,144,153,9,153,144,0,9,195,153,9,197,153,198,
  0,197,153,144,204,0,153,208,0,194,153,9,153,144,153,194,
  0,9,199,153,195,0,144,0,9,144,196,0,153,144,199,0,
  197,153,195,0,201,153,199,0,153,0,9,194,153,9,195,153,
  9,197,153,198,0,197,153,144,200,0,195,0,199,153,144,9,
  153,194,9,153,144,194,0,153,9,0,144,194,153,144,194,153,
  198,0,197,153,144,203,0,9,153,207,0,9,198,153,144,0,
  201,153,0,144,153,144,153,144,0,153,0,9,194,153,199,0,
  194,9,144,194,153,195,0,195,153,144,197,153,144,197,0,9,
  197,153,0,153,9,0,144,194,153,144,194,153,198,0,197,153,
  144,200,0,194,0,9,197,153,144,153,194,0,196,153,144,195,
  0,9,0,9,0,144,0,153,144,197,0,9,197,153,204,0,
  194,153,195,0,9,203,0,9,194,153,194,144,0,153,144,0,
  195,153,144,197,153,144,9,153,0,9,0,9,197,153,200,0,
  9,0,153,144,195,0,9,194,144,194,9,194,153,9,153,144,
  197,0,9,197,153,194,0,9,0,9,0,144,0,153,144,197,
  0,9,197,153,201,0,195,0,194,9,0,153,9,0,144,0,
  194,9,153,0,144,9,195,0,9,0,9,0,9,0,144,198,
  0,194,153,9,153,9,206,0,9,195,0,9,194,153,202,0,
  153,144,153,144,194,9,194,0,9,194,144,194,9,194,153,9,
  153,144,9,153,0,9,0,198,153,200,0,9,0,144,0,153,
  196,0,144,0,144,9,0,9,153,198,0,197,153,144,194,0,
  9,0,9,0,9,0,144,198,0,194,153,9,153,9,202,0,
  196,0,153,0,144,9,0,194,153,144,194,9,0,144,195,0,
  194,9,194,153,144,153,0,144,198,0,9,144,153,0,9,194,
  144,202,0,197,153,0,195,9,153,198,0,9,195,0,153,0,
  144,9,197,0,144,0,144,9,0,9,153,194,0,153,0,194,
  9,197,153,144,200,0,9,194,144,194,153,144,195,0,144,0,
  144,0,144,9,198,0,9,153,144,153,194,144,194,0,194,9,
  194,153,144,153,0,144,198,0,9,144,153,0,9,194,144,200,
  0,196,0,194,9,200,153,9,144,194,0,144,201,153,198,0,
  9,0,144,9,0,196,144,0,144,197,0,9,195,153,194,0,
  196,9,0,195,153,144,0,153,144,153,0,144,9,144,153,197,
  0,144,0,144,0,144,9,195,0,194,153,9,196,153,194,144,
  195,153,0,195,153,194,144,195,153,9,153,144,194,0,144,195,
  153,9,144,9,199,0,153,9,144,0,153,9,144,201,153,198,
  0,9,0,144,9,0,196,144,0,144,195,0,194,0,9,218,
  153,195,144,9,0,9,0,144,194,9,201,153,144,199,0,205,
  153,194,144,9,194,153,144,195,153,144,195,153,9,144,153,144,
  217,153,144,204,153,9,196,0,144,194,9,205,153,195,144,9,
  0,9,0,144,194,9,200,153,144,255,153,255,153,230,153
}

RES_PCX_SPECTROX =
{
  10,5,1,4,0,0,0,0,176,0,21,0,0,0,0,0,
  0,0,0,255,0,255,239,239,239,32,17,13,31,15,32,61,
  29,64,123,80,76,127,122,96,140,74,52,121,58,128,188,74,
  155,204,119,180,245,160,151,250,214,184,254,243,192,255,255,255,
  0,1,90,0,1,0,0,0,0,0,0,0,0,0,0,0,
  0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,
  0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,
  0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,
  0,127,198,255,193,247,195,0,127,198,255,193,247,195,0,127,
  200,255,194,0,7,199,255,112,0,15,195,255,193,247,194,0,
  127,195,255,193,240,15,200,255,112,194,0,7,199,255,112,0,
  15,193,255,193,240,197,0,15,193,255,193,240,0,7,200,238,
  112,0,7,200,238,112,0,7,201,238,194,0,126,199,238,193,
  231,0,14,196,238,112,7,196,238,193,224,14,200,238,193,231,
  194,0,126,199,238,193,231,0,14,193,238,193,224,197,0,14,
  193,238,193,224,0,94,200,238,193,229,0,94,200,238,193,229,
  0,94,201,238,0,5,201,238,80,14,196,238,193,224,14,196,
  238,193,224,14,201,238,112,5,201,238,80,14,193,238,193,224,
  197,0,14,193,238,193,224,0,126,193,238,193,237,196,221,193,
  222,193,238,193,231,0,126,193,238,193,237,196,221,193,222,193,
  238,193,231,0,126,193,238,193,237,199,221,0,7,194,238,197,
  221,194,238,112,13,194,221,194,238,193,224,14,194,238,194,221,
  193,208,13,199,221,194,238,193,224,7,194,238,197,221,194,238,
  112,14,193,238,193,224,197,0,14,193,238,193,224,0,194,238,
  193,216,196,136,141,194,238,0,194,238,193,216,196,136,141,194,
  238,0,194,238,193,216,199,136,0,14,193,238,193,237,197,136,
  193,222,193,238,193,224,8,194,136,126,193,238,193,224,14,193,
  238,193,231,194,136,128,8,199,136,193,222,193,238,193,224,14,
  193,238,193,237,197,136,193,222,193,238,193,224,14,193,238,193,
  224,197,0,14,193,238,193,224,0,194,204,200,136,0,194,204,
  198,136,194,204,0,194,204,200,136,0,12,193,204,193,200,197,
  136,140,193,204,193,192,8,194,136,140,193,204,193,192,12,193,
  204,193,200,194,136,128,8,199,136,140,193,204,193,192,12,193,
  204,193,200,197,136,140,193,204,193,192,12,193,204,193,192,197,
  0,12,193,204,193,192,0,194,238,128,196,0,8,194,136,0,
  194,238,133,196,0,88,194,238,0,194,238,128,200,0,14,193,
  238,193,232,80,195,0,5,142,193,238,193,224,194,0,5,142,
  193,238,193,224,14,193,238,193,232,80,201,0,5,142,193,238,
  193,224,14,193,238,193,232,80,195,0,5,142,193,238,193,224,
  14,193,238,193,229,197,0,94,193,238,193,224,0,200,204,101,
  194,0,194,204,80,196,0,5,194,204,0,194,204,193,192,200,
  0,12,193,204,193,197,197,0,88,136,128,195,0,92,193,204,
  193,192,12,193,204,193,197,203,0,108,193,204,193,192,12,193,
  204,193,197,197,0,92,193,204,193,192,12,193,204,193,198,80,
  195,0,5,108,193,204,193,192,0,201,204,96,0,194,204,198,
  0,194,204,0,108,199,204,195,0,12,193,204,193,192,197,0,
  8,136,128,195,0,12,193,204,193,192,12,193,204,193,192,196,
  0,6,200,204,128,12,193,204,193,192,197,0,12,193,204,193,
  192,9,201,204,144,0,108,200,204,193,198,0,194,204,198,0,
  194,204,0,140,199,204,195,0,12,193,204,193,192,203,0,12,
  193,204,193,192,12,193,204,193,192,196,0,108,199,204,193,200,
  80,12,193,204,193,192,197,0,12,193,204,193,192,5,156,199,
  204,193,201,80,0,90,201,170,0,194,170,197,0,3,194,170,
  0,138,199,170,195,0,10,170,160,203,0,10,170,160,10,170,
  160,196,0,200,170,169,0,10,170,160,197,0,10,170,160,0,
  90,199,170,165,194,0,69,199,85,194,204,0,194,204,197,0,
  54,194,204,0,108,199,204,195,0,12,193,204,193,192,197,0,
  12,193,204,193,192,195,0,12,193,204,193,192,12,193,204,193,
  192,195,0,6,201,204,96,12,193,204,193,192,197,0,12,193,
  204,193,192,0,201,204,194,0,4,199,85,194,170,0,194,170,
  0,196,187,186,194,170,0,194,170,149,197,85,195,0,10,170,
  160,197,0,10,170,160,195,0,10,170,160,10,170,160,195,0,
  10,170,169,197,85,154,170,160,10,170,160,197,0,10,170,160,
  9,170,169,197,85,154,170,144,0,194,170,198,0,194,170,0,
  194,170,0,198,170,165,0,194,170,198,85,195,0,10,170,160,
  197,0,10,170,160,195,0,10,170,160,10,170,160,195,0,10,
  170,165,197,85,90,170,160,10,170,160,197,0,10,170,160,10,
  170,165,197,85,90,170,160,0,194,170,80,196,0,5,194,170,
  0,194,170,0,198,170,148,0,194,170,80,200,0,10,170,165,
  197,0,90,170,160,195,0,10,170,160,10,170,160,195,0,10,
  170,165,80,195,0,5,90,170,160,10,170,165,197,0,90,170,
  160,10,170,165,80,195,0,5,90,170,160,0,194,170,181,196,
  68,91,194,170,0,194,170,0,198,153,80,0,194,170,176,200,
  0,10,170,171,84,195,68,69,186,170,160,195,0,10,170,160,
  10,170,160,195,0,10,170,165,197,0,90,170,160,10,170,171,
  84,195,68,69,186,170,160,10,170,165,197,0,90,170,160,0,
  154,170,171,196,187,186,170,169,0,194,170,0,198,85,64,0,
  154,170,171,199,187,0,9,194,170,197,187,194,170,144,195,0,
  10,170,160,10,170,160,195,0,10,170,160,197,0,10,170,160,
  9,194,170,197,187,194,170,144,10,170,160,197,0,10,170,160,
  0,90,200,170,165,0,194,170,0,197,85,84,194,0,90,201,
  170,0,5,201,170,80,195,0,10,170,160,10,170,160,195,0,
  10,170,160,197,0,10,170,160,5,201,170,80,10,170,160,197,
  0,10,170,160,0,73,200,170,148,0,194,170,201,0,73,201,
  170,0,4,154,199,170,169,64,195,0,10,170,160,10,170,160,
  195,0,10,170,160,197,0,10,170,160,4,154,199,170,169,64,
  10,170,160,197,0,10,170,160,0,5,200,153,80,0,194,153,
  201,0,5,201,153,194,0,89,199,153,149,196,0,9,153,144,
  9,153,144,195,0,9,153,144,197,0,9,153,144,0,89,199,
  153,149,0,9,153,144,197,0,9,153,144,0,4,200,85,64,
  0,194,85,201,0,4,201,85,194,0,69,199,85,84,196,0,
  5,85,80,5,85,80,195,0,5,85,80,197,0,5,85,80,
  0,69,199,85,84,0,5,85,80,197,0,5,85,80,0,0,
  69,198,85,84,194,0,194,85,202,0,69,200,85,194,0,4,
  199,85,64,196,0,5,85,80,5,85,80,195,0,5,85,80,
  197,0,5,85,80,0,4,199,85,64,0,5,85,80,197,0,
  5,85,80,0
}

RES_PCX_TIME =
{
  10,5,1,4,0,0,0,0,116,0,10,0,0,0,0,0,
  0,0,0,255,0,255,239,239,239,32,17,13,31,15,32,61,
  29,64,123,80,76,127,122,96,140,74,52,121,58,128,188,74,
  155,204,119,180,245,160,151,250,214,184,254,243,192,255,255,255,
  0,1,60,0,1,0,0,0,0,0,0,0,0,0,0,0,
  0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,
  0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,
  0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,
  197,136,8,196,136,128,3,136,131,8,136,128,0,56,195,136,
  128,0,8,196,136,128,3,195,136,48,194,0,8,196,136,8,
  128,196,0,136,195,0,136,0,8,128,136,8,128,0,197,255,
  15,196,255,193,240,54,193,255,193,246,143,193,255,193,248,3,
  143,195,255,193,240,0,15,196,255,193,240,56,195,255,115,194,
  0,135,196,255,15,193,240,196,0,193,255,195,0,193,255,0,
  15,193,240,193,255,15,193,240,0,197,238,14,196,238,193,224,
  193,254,197,238,8,196,238,193,224,0,14,196,238,193,224,142,
  195,238,193,231,194,0,126,196,238,14,193,224,196,0,193,238,
  195,0,193,238,0,14,193,224,193,238,14,193,224,0,194,0,
  193,204,196,0,12,193,192,194,0,193,204,48,60,193,195,3,
  193,204,12,193,195,199,0,12,193,192,194,0,193,204,99,51,
  54,193,204,194,0,193,204,196,0,12,193,192,196,0,193,204,
  48,194,0,193,204,0,12,193,192,193,204,12,193,192,0,194,
  0,193,238,196,0,14,193,224,194,0,193,238,0,14,193,224,
  0,193,238,14,193,232,194,136,128,196,0,14,193,224,194,0,
  193,238,195,0,193,238,194,0,193,238,195,136,0,14,193,224,
  196,0,193,238,104,194,136,193,238,0,14,193,224,193,238,14,
  193,224,0,194,0,193,204,196,0,12,193,192,194,0,193,204,
  0,12,193,192,0,193,204,6,195,204,193,192,196,0,12,193,
  192,194,0,193,204,195,0,193,204,194,0,196,204,0,12,193,
  192,196,0,108,196,204,0,12,193,192,193,204,12,193,192,0,
  194,0,170,196,0,10,160,194,0,170,0,10,160,0,170,9,
  195,170,160,196,0,10,160,194,0,170,195,0,170,194,0,196,
  170,0,10,160,196,0,9,196,170,0,10,160,170,10,160,0,
  194,0,170,196,0,10,160,194,0,170,0,10,160,0,170,10,
  164,199,0,10,160,194,0,170,64,0,4,170,194,0,170,196,
  0,10,164,200,0,170,199,0,194,0,170,194,0,4,68,74,
  164,68,64,170,0,9,144,0,170,10,163,195,68,64,195,0,
  10,160,194,0,170,148,68,73,170,194,0,170,196,0,10,169,
  195,68,64,196,68,170,0,4,64,68,4,64,0,194,0,170,
  194,0,11,187,186,171,187,176,170,196,0,170,9,170,195,187,
  176,195,0,10,160,194,0,154,171,187,186,169,194,0,170,196,
  0,9,170,195,187,176,154,195,187,169,0,11,176,187,11,176,
  0,194,0,153,194,0,10,196,170,160,153,196,0,153,5,154,
  195,170,160,195,0,9,144,194,0,89,195,170,149,194,0,153,
  196,0,5,154,195,170,160,9,195,170,149,0,10,160,170,10,
  160,0
}

RES_PCX_LISSA_BG =
{
  10,5,1,4,0,0,0,0,239,0,135,0,0,0,0,0,
  0,0,0,0,0,0,12,19,31,26,29,33,24,38,62,51,
  57,65,37,58,94,64,51,83,121,58,128,188,74,155,0,255,
  0,0,255,0,0,255,0,0,255,0,0,255,0,0,255,0,
  0,1,120,0,1,0,0,0,0,0,0,0,0,0,0,0,
  0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,
  0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,
  0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,
  216,17,23,195,119,199,17,119,23,17,198,23,17,199,23,199,
  17,23,200,17,199,23,17,198,23,17,194,23,113,196,23,194,
  17,194,23,119,23,216,17,213,17,194,119,17,119,194,113,119,
  113,198,17,194,113,194,17,119,113,196,119,113,119,195,113,194,
  17,113,197,17,113,119,113,194,17,113,17,113,194,17,113,194,
  17,194,113,119,113,196,119,113,119,113,194,17,194,113,119,113,
  17,119,113,17,113,17,195,113,23,119,113,212,17,212,17,23,
  194,17,119,23,17,194,23,119,194,17,113,194,17,194,23,17,
  195,23,195,119,194,120,119,17,119,17,23,119,194,17,194,23,
  194,17,195,23,194,17,113,198,17,23,119,17,23,113,23,194,
  120,196,119,194,23,17,194,23,119,194,17,23,113,195,23,17,
  194,23,113,17,23,212,17,212,17,195,113,23,119,195,17,113,
  196,17,23,113,119,194,113,198,119,194,135,195,119,195,113,17,
  194,113,17,7,0,112,194,0,1,194,0,112,194,17,113,17,
  194,113,196,119,194,135,197,119,194,113,119,194,113,194,17,119,
  113,17,113,195,17,119,17,195,113,211,17,212,17,23,113,195,
  23,17,194,23,119,195,17,113,195,23,198,119,195,120,196,119,
  23,17,194,23,113,16,112,203,0,194,23,17,194,23,195,119,
  195,120,199,119,23,119,17,194,23,17,195,23,17,195,23,17,
  119,195,17,113,208,17,204,17,194,113,198,17,113,23,113,17,
  194,119,194,17,194,113,196,17,113,196,119,120,135,119,195,135,
  119,135,194,119,194,113,112,209,0,112,113,195,119,135,119,195,
  135,119,136,196,119,113,119,17,194,113,17,194,113,17,23,194,
  113,17,119,17,113,211,17,204,17,23,119,23,198,17,23,17,
  194,23,194,119,23,194,119,195,17,195,23,119,198,120,136,194,
  119,120,119,194,23,198,0,5,199,85,198,0,194,7,23,120,
  194,119,120,136,197,120,194,119,23,119,195,17,23,119,194,23,
  194,119,23,17,23,194,17,23,197,17,23,119,196,17,23,198,
  17,205,17,194,119,113,194,17,23,194,17,113,17,23,194,113,
  196,119,113,195,17,195,119,135,119,194,135,196,136,194,135,194,
  112,195,0,195,5,204,85,197,0,112,119,135,196,136,195,135,
  119,135,195,119,113,194,17,196,119,194,113,119,194,17,113,194,
  17,23,113,17,119,113,119,194,113,194,17,113,199,17,206,17,
  23,119,200,17,194,119,194,23,195,119,23,17,194,23,119,120,
  119,195,120,197,136,120,112,196,0,195,81,206,85,196,0,194,
  120,197,136,194,120,119,120,194,119,23,119,194,23,195,119,194,
  23,119,113,198,17,194,119,194,23,119,194,23,17,194,23,199,
  17,201,17,194,113,194,17,113,17,194,113,199,17,195,119,113,
  197,119,113,194,119,194,135,136,135,198,136,196,0,194,5,194,
  21,5,85,48,199,0,53,197,85,80,195,0,8,197,136,135,
  136,195,135,119,113,197,119,113,195,119,113,198,17,119,194,17,
  194,113,119,194,113,17,113,199,17,201,17,23,194,17,23,119,
  17,119,200,17,194,23,119,23,194,119,120,197,119,120,119,120,
  136,120,196,136,198,0,194,80,48,205,0,197,85,195,0,8,
  196,136,120,136,119,120,201,119,23,119,23,199,17,119,17,23,
  195,119,23,202,17,201,17,113,195,17,113,17,119,113,119,194,
  17,113,195,17,23,113,119,194,113,199,119,194,135,198,136,128,
  198,0,48,209,0,196,85,195,0,198,136,195,135,198,119,194,
  113,119,113,119,196,17,113,17,23,113,17,195,119,195,113,201,
  17,200,17,195,23,17,23,119,23,119,23,119,113,23,113,195,
  17,194,23,194,119,23,194,119,120,196,119,120,119,120,196,136,
  128,196,0,3,51,212,0,5,195,85,80,194,0,197,136,119,
  120,196,119,120,195,119,23,194,119,23,196,17,119,194,23,17,
  23,196,119,23,195,17,23,198,17,200,17,194,113,195,17,194,
  119,113,17,119,23,113,119,194,113,194,17,113,119,113,194,119,
  120,198,119,135,119,195,136,128,195,0,3,194,48,215,0,195,
  85,80,194,0,195,136,135,119,135,200,119,113,119,194,113,194,
  17,113,119,194,113,119,194,17,195,119,194,113,195,17,194,113,
  197,17,200,17,23,17,23,194,17,195,23,17,23,119,199,23,
  199,119,194,120,119,120,136,120,195,136,196,0,48,218,0,195,
  85,194,0,8,195,136,120,136,119,194,120,200,119,199,23,119,
  17,194,23,194,119,23,195,17,23,119,23,113,196,17,201,17,
  113,17,113,194,17,194,113,17,195,119,195,113,119,113,200,119,
  135,119,194,135,136,135,194,136,80,195,0,48,220,0,5,194,
  85,194,0,194,136,135,136,195,135,119,135,199,119,113,119,195,
  113,195,119,113,17,195,119,113,195,17,196,113,196,17,200,17,
  23,17,194,23,196,17,194,23,195,119,23,199,119,197,120,119,
  194,120,136,120,136,128,194,0,3,223,0,5,194,85,0,5,
  194,136,120,136,120,119,197,120,200,119,23,195,119,23,17,23,
  194,119,195,17,194,23,194,119,23,196,17,201,17,113,17,119,
  113,194,17,113,194,17,194,113,119,113,199,119,194,135,119,196,
  135,196,136,195,0,48,224,0,194,85,80,0,8,195,136,197,
  135,119,194,135,198,119,113,119,195,113,195,17,119,113,195,17,
  194,113,119,194,113,196,17,201,17,23,17,194,23,195,17,195,
  23,119,23,199,119,120,119,120,119,194,120,136,120,195,136,128,
  0,194,48,226,0,194,85,194,0,196,136,120,136,120,119,120,
  119,120,200,119,23,119,194,23,199,17,196,119,23,196,17,200,
  17,195,113,17,194,113,195,17,196,113,199,119,195,135,136,135,
  198,136,0,5,3,228,0,194,85,0,8,197,136,135,136,196,
  135,194,119,120,195,119,197,113,199,17,195,119,194,113,196,17,
  199,17,196,23,17,194,23,194,17,194,23,119,195,23,119,135,
  196,119,197,120,195,136,137,136,128,0,80,229,0,5,85,80,
  0,198,136,196,120,197,119,135,119,195,23,119,23,199,17,23,
  195,119,197,17,198,17,194,113,195,119,113,194,119,195,17,113,
  194,119,113,199,119,195,135,119,196,136,152,136,0,194,3,230,
  0,194,85,0,8,194,152,195,136,135,119,195,135,198,119,113,
  194,119,194,113,199,17,23,113,119,113,197,17,197,17,23,194,
  119,194,23,194,119,23,119,17,23,17,194,23,198,119,194,120,
  119,194,120,136,120,195,136,137,136,128,234,0,85,80,0,195,
  136,194,137,136,120,136,120,119,194,120,199,119,23,195,17,194,
  119,23,17,23,17,194,23,198,17,197,17,119,113,195,17,195,
  119,113,17,113,194,17,196,119,120,119,135,119,195,135,198,136,
  153,152,194,0,3,232,0,5,85,0,8,137,136,153,195,152,
  136,196,135,119,135,120,196,119,113,194,17,119,198,113,200,17,
  196,17,23,119,17,23,17,195,23,119,113,17,23,119,17,23,
  194,119,136,119,136,195,119,195,120,136,197,137,128,194,0,48,
  233,0,81,80,0,136,137,153,194,137,194,136,194,120,195,119,
  120,135,120,135,194,119,17,23,119,195,23,194,17,194,23,200,
  17,197,17,113,194,17,202,113,195,119,136,196,135,119,194,135,
  194,136,196,152,136,0,5,235,0,5,21,0,8,136,194,152,
  136,152,136,195,135,119,195,135,136,135,194,119,197,113,194,119,
  196,113,199,17,196,17,23,194,17,194,23,194,17,23,194,119,
  23,119,194,23,196,119,136,194,120,194,136,120,119,194,120,137,
  136,153,137,128,0,83,48,235,0,81,80,0,136,137,195,136,
  120,119,194,120,194,136,194,120,135,196,119,194,23,119,23,119,
  23,113,23,119,23,199,17,197,17,113,195,17,113,194,17,194,
  119,113,199,119,135,196,136,194,135,119,135,194,136,153,152,194,
  0,51,239,0,8,153,152,136,194,135,119,135,196,136,194,135,
  200,119,195,113,194,119,113,199,17,196,17,23,119,194,17,23,
  195,17,23,119,23,198,119,120,119,120,196,136,119,195,120,194,
  137,128,0,3,48,238,0,80,0,137,153,136,194,120,119,120,
  137,153,152,136,119,120,201,119,23,194,119,194,23,199,17,197,
  17,119,113,17,194,113,194,17,113,200,119,135,119,196,136,196,
  135,194,136,153,244,0,8,153,136,196,135,137,136,153,136,135,
  119,135,202,119,195,113,199,17,197,17,23,119,194,23,195,17,
  23,196,119,120,136,135,120,119,120,197,136,195,120,136,137,152,
  194,0,48,239,0,5,0,8,194,137,136,195,120,136,194,137,
  194,136,119,120,119,194,136,199,119,23,194,119,17,194,113,196,
  17,198,17,195,113,195,17,113,196,119,136,135,120,119,135,119,
  197,136,135,195,136,153,144,0,3,243,0,136,194,152,136,135,
  136,137,136,153,136,135,119,135,120,119,136,135,194,119,113,194,
  119,113,119,113,119,194,23,113,196,17,199,17,33,196,17,194,
  23,194,119,194,136,120,119,196,120,196,136,137,195,136,152,246,
  0,8,194,137,136,194,137,136,194,153,136,195,120,119,194,120,
  136,135,194,119,23,119,194,23,119,113,119,23,197,17,198,17,
  194,18,34,194,23,17,196,119,135,136,135,136,119,195,135,195,
  136,137,194,152,136,137,128,194,0,48,244,0,136,152,136,194,
  152,194,153,152,196,135,120,135,136,194,135,119,113,119,113,119,
  194,113,194,17,113,197,17,198,17,33,113,33,23,194,113,119,
  23,119,197,120,136,195,120,195,136,194,153,137,136,152,144,0,
  3,245,0,136,137,136,137,194,153,194,137,136,195,120,136,196,
  120,195,119,23,17,23,201,17,197,17,18,194,39,18,17,114,
  119,23,195,119,135,136,135,194,136,194,135,195,136,194,137,136,
  153,152,137,194,0,48,245,0,8,194,152,153,194,152,195,136,
  194,135,194,136,135,136,194,135,119,195,113,33,113,201,17,197,
  17,33,34,114,34,33,39,194,33,39,194,119,194,120,194,136,
  120,195,136,194,120,136,153,152,137,136,152,194,0,48,245,0,
  8,153,195,137,194,136,194,120,195,136,120,194,136,120,195,119,
  195,23,202,17,197,17,194,18,196,34,18,34,114,195,119,194,
  135,136,135,195,136,135,119,136,194,137,136,137,133,0,3,247,
  0,136,152,153,152,136,135,119,195,152,135,136,195,135,194,119,
  196,114,33,201,17,199,17,194,33,17,33,194,17,194,39,194,
  119,120,195,119,196,136,135,120,137,152,137,152,80,249,0,8,
  195,137,136,119,195,137,136,135,194,119,120,195,119,39,17,194,
  18,17,18,200,17,197,17,113,114,18,17,18,17,194,18,194,
  114,195,119,135,194,119,135,196,136,135,136,195,153,194,0,48,
  247,0,9,194,153,137,135,194,136,153,136,151,135,194,119,135,
  194,119,114,194,113,17,34,194,33,194,17,194,113,23,195,17,
  197,17,194,23,194,17,18,33,194,34,194,39,195,119,197,120,
  195,136,120,136,194,153,149,0,83,249,0,194,153,152,136,120,
  137,153,136,196,120,194,119,135,119,23,194,17,18,34,194,17,
  194,23,197,17,197,17,113,119,113,23,195,18,194,34,194,119,
  120,194,119,136,194,135,119,197,136,137,153,144,5,250,0,137,
  194,153,152,136,153,152,135,119,135,136,135,196,119,113,195,17,
  34,194,17,119,194,113,196,17,195,17,197,23,194,17,23,194,
  34,39,197,119,120,194,136,194,120,136,152,136,152,136,153,144,
  0,80,249,0,152,153,194,152,137,153,136,194,120,194,136,197,
  119,195,17,194,18,17,194,23,39,194,23,195,17,198,17,119,
  113,194,17,18,114,34,39,196,119,194,135,136,135,119,196,136,
  137,136,153,80,5,250,0,9,194,153,137,136,152,136,135,119,
  136,195,135,195,119,113,195,17,33,17,33,119,114,33,196,17,
  195,17,23,17,23,119,195,17,33,39,33,119,39,119,194,120,
  119,194,120,119,120,194,136,152,195,136,153,0,80,250,0,9,
  194,153,152,137,152,194,136,119,194,120,119,194,120,194,119,39,
  195,17,194,18,34,39,119,17,23,195,17,196,17,113,119,113,
  17,194,113,114,194,119,194,114,119,195,136,194,135,194,119,136,
  135,196,137,149,0,3,251,0,196,153,151,136,135,194,119,135,
  195,136,135,195,113,194,17,33,23,113,17,119,114,113,195,17,
  196,17,194,23,114,39,23,195,119,39,119,39,196,120,136,120,
  119,120,136,120,136,152,194,153,144,0,80,250,0,9,194,153,
  152,137,136,120,136,119,194,120,136,195,120,119,23,119,194,17,
  18,194,119,34,119,23,196,17,196,17,113,119,113,114,119,113,
  17,119,194,114,194,119,136,135,196,136,135,136,135,194,137,194,
  153,144,0,48,251,0,194,153,137,136,135,136,135,196,136,135,
  136,135,119,194,113,194,17,33,194,119,113,119,194,113,195,17,
  196,17,23,39,119,39,119,113,23,195,39,119,194,120,198,136,
  194,120,194,136,194,153,80,5,252,0,9,153,194,152,136,194,
  120,198,136,120,194,119,23,17,194,18,194,119,39,119,39,196,
  17,197,17,196,119,113,17,18,114,195,119,194,135,196,136,194,
  135,195,136,137,153,0,5,252,0,9,194,153,194,136,194,135,
  194,136,194,152,195,135,194,119,113,17,34,33,196,119,113,196,
  17,197,17,195,119,113,194,17,23,194,39,194,119,194,120,197,
  136,120,194,136,152,194,153,0,3,252,0,9,194,153,152,194,
  136,120,152,153,137,194,136,120,195,119,23,18,34,23,34,195,
  119,113,196,17,197,17,18,114,113,18,194,34,113,114,196,119,
  202,136,137,149,0,80,253,0,195,153,195,136,137,136,194,152,
  136,135,195,119,114,194,34,114,119,17,194,114,197,17,198,17,
  33,34,33,17,194,39,23,197,119,120,200,136,153,144,0,80,
  252,0,9,194,153,194,152,194,136,152,137,194,136,198,119,194,
  34,119,17,34,194,33,197,17,198,17,18,194,17,195,114,197,
  119,194,136,135,197,136,194,137,153,144,0,48,253,0,195,153,
  194,137,153,137,136,135,194,136,135,198,119,114,194,17,18,198,
  17,201,17,194,39,196,119,194,120,194,136,194,120,197,136,137,
  153,144,255,0,197,153,194,152,136,194,120,194,136,120,197,119,
  113,33,200,17,200,17,194,18,195,119,135,119,135,194,136,135,
  136,135,196,136,194,152,153,80,5,254,0,9,196,153,137,136,
  135,136,135,194,136,194,135,119,135,194,119,114,18,200,17,201,
  17,194,39,119,120,136,194,120,194,136,120,136,194,120,136,196,
  137,153,0,5,252,0,1,16,195,153,152,195,136,194,120,136,
  120,194,136,194,120,136,194,119,39,33,200,17,196,17,194,18,
  195,17,194,114,119,136,195,135,136,135,194,136,135,194,136,152,
  194,153,194,152,0,3,252,0,21,17,25,137,153,137,195,136,
  135,194,136,135,136,195,135,136,135,194,114,113,195,17,18,196,
  17,195,17,18,195,33,194,17,194,39,119,135,120,136,120,194,
  136,120,135,194,120,136,137,194,153,137,153,254,0,17,81,25,
  152,153,136,152,194,136,120,119,136,120,194,136,120,136,119,135,
  119,39,33,195,17,33,34,195,17,195,17,18,34,196,18,194,
  114,119,135,136,135,194,136,135,194,136,194,135,136,153,194,152,
  153,152,254,0,194,17,25,195,137,152,136,194,135,194,136,135,
  194,136,135,136,194,135,195,114,18,17,194,18,34,195,17,195,
  17,33,194,34,194,33,18,39,194,119,195,120,194,136,120,195,
  136,120,198,137,254,0,17,81,24,152,153,137,152,194,136,120,
  195,136,120,194,136,194,120,195,119,34,194,17,194,34,194,33,
  194,17,195,17,18,17,18,34,194,18,114,195,119,136,135,136,
  194,135,194,136,135,194,136,153,194,152,153,149,0,80,252,0,
  17,21,17,137,136,153,152,194,136,135,194,136,194,135,136,135,
  136,135,194,119,194,114,194,18,34,17,18,195,17,196,17,33,
  17,33,34,23,39,196,119,197,120,136,194,120,194,137,194,153,
  152,128,0,80,252,0,1,81,17,153,196,137,136,194,120,136,
  196,120,197,119,39,18,194,33,17,33,195,17,195,17,194,18,
  17,194,34,113,114,39,196,119,194,135,119,135,194,119,194,136,
  152,153,152,136,133,17,48,252,0,1,21,17,136,153,194,152,
  153,152,136,194,119,135,119,194,135,194,119,120,119,34,113,114,
  34,33,194,18,195,17,196,17,194,33,18,33,17,23,33,197,
  119,194,120,119,194,120,194,136,195,137,136,145,17,16,252,0,
  1,194,17,152,153,195,136,137,136,120,119,194,120,194,119,120,
  194,119,113,39,194,17,34,17,194,33,195,17,196,17,18,34,
  33,113,114,17,23,198,119,136,194,135,195,136,135,195,136,129,
  17,16,252,0,1,21,20,136,152,136,135,194,136,152,194,135,
  136,135,198,119,17,18,194,113,194,34,196,17,197,17,34,39,
  17,34,33,195,119,120,119,194,120,198,136,120,194,136,137,129,
  17,16,252,0,1,194,17,194,153,194,136,120,198,136,120,195,
  119,135,119,113,34,33,23,34,33,196,17,199,17,114,200,119,
  199,136,135,194,136,152,129,17,16,252,0,1,17,24,136,152,
  136,135,194,136,152,195,136,152,135,199,119,114,113,198,17,199,
  17,198,119,39,119,120,199,136,120,195,136,129,17,16,252,0,
  1,17,20,194,137,194,136,120,136,137,194,136,137,194,136,194,
  119,39,197,119,113,198,17,197,17,119,114,17,119,113,114,194,
  119,116,194,119,198,136,194,135,195,136,113,17,16,253,0,194,
  17,136,152,136,194,135,136,194,152,136,194,152,135,119,114,194,
  119,114,113,119,113,18,119,113,196,17,196,17,23,119,113,33,
  39,17,18,39,119,39,119,194,120,197,136,195,120,136,135,129,
  194,17,253,0,21,24,195,136,195,120,137,153,194,137,136,120,
  194,119,39,119,34,17,23,194,33,194,119,196,17,196,17,194,
  113,23,113,17,18,113,34,119,114,119,135,197,136,194,135,195,
  136,120,113,194,17,253,0,194,17,196,136,194,135,136,194,153,
  152,136,194,135,116,119,114,33,114,194,17,119,17,194,113,195,
  17,195,17,194,23,17,194,119,113,39,119,34,194,39,194,119,
  120,197,136,120,195,136,135,133,17,49,253,0,21,17,120,196,
  136,120,197,136,195,119,39,36,39,119,33,194,119,113,194,23,
  195,17,196,17,113,17,113,119,18,194,119,114,34,194,119,135,
  196,136,194,135,196,136,119,113,17,81,253,0,81,17,135,196,
  136,194,135,196,136,194,135,119,114,34,194,119,114,23,194,113,
  17,113,195,17,195,17,23,17,23,119,194,23,39,194,119,39,
  194,119,195,120,194,136,120,119,195,120,194,135,117,17,81,252,
  0,1,21,17,194,120,136,194,120,119,195,120,136,194,120,195,
  119,39,194,119,39,194,23,119,17,23,195,17,195,17,113,194,
  119,194,113,23,114,199,119,194,135,136,135,119,136,120,135,120,
  118,120,194,17,252,0,1,81,24,119,194,135,136,120,135,119,
  196,135,198,119,114,119,194,17,194,119,194,113,194,17,195,17,
  23,119,196,23,200,119,120,136,120,119,120,135,195,120,119,104,
  194,17,16,251,0,1,17,24,119,195,120,119,136,194,119,194,
  120,195,119,135,197,119,23,17,194,23,119,195,17,195,17,23,
  195,113,194,17,114,198,119,194,135,136,195,135,119,120,194,135,
  118,120,194,17,16,251,0,1,81,24,136,119,135,136,194,119,
  194,135,136,119,194,135,197,119,114,113,195,17,113,119,195,17,
  196,17,194,23,195,17,39,198,119,120,194,136,120,195,119,136,
  120,119,103,119,17,19,16,251,0,5,17,24,136,119,194,120,
  135,195,119,120,194,136,199,119,33,195,17,23,196,17,195,17,
  33,197,17,114,197,119,116,119,136,135,195,119,120,195,119,194,
  118,17,21,17,251,0,1,17,132,194,135,194,119,120,196,119,
  136,135,116,197,119,114,113,17,18,198,17,200,17,194,23,119,
  39,119,71,34,66,71,197,119,194,135,194,119,194,103,81,21,
  17,251,0,1,17,24,194,120,119,120,119,120,197,119,194,66,
  39,71,119,39,119,23,194,17,18,197,17,201,17,196,114,194,
  34,194,36,195,119,135,120,119,135,194,119,194,118,129,194,17,
  251,0,1,17,132,196,119,194,135,194,119,135,119,116,36,34,
  194,39,119,114,194,113,200,17,198,17,23,194,17,194,39,119,
  196,34,39,194,119,135,119,136,119,120,194,119,70,103,129,17,
  49,251,0,1,17,134,194,71,119,120,194,136,120,136,195,119,
  195,114,196,119,194,17,23,198,17,198,17,113,119,113,17,119,
  114,18,195,34,197,119,136,119,120,194,116,68,102,129,17,81,
  16,250,0,1,24,132,194,116,119,120,135,194,136,135,202,119,
  17,119,194,113,197,17,197,17,23,39,33,23,194,119,33,194,
  39,34,39,119,194,71,119,135,120,136,135,119,195,71,117,17,
  81,16,250,0,194,17,134,195,71,119,136,195,120,197,119,114,
  197,119,17,194,39,197,17,197,17,194,114,34,33,194,17,18,
  196,114,196,116,198,119,116,118,120,17,19,16,250,0,17,24,
  195,100,197,119,118,194,119,39,119,39,34,18,23,119,23,119,
  194,34,114,113,196,17,197,17,39,34,39,34,33,17,23,119,
  195,39,119,71,119,194,71,198,119,71,104,17,21,16,250,0,
  17,24,194,102,194,71,119,194,71,194,119,116,194,119,114,194,
  34,33,17,119,113,194,119,34,39,33,196,17,196,17,18,196,
  114,33,18,197,119,116,71,119,116,196,119,194,116,119,118,104,
  81,21,17,250,0,17,24,102,194,100,195,116,197,119,71,39,
  194,34,18,17,194,23,39,196,114,196,17,196,17,33,34,113,
  39,34,33,17,194,39,195,119,68,119,194,71,194,119,195,71,
  70,71,102,104,129,17,49,249,0,1,17,136,102,70,68,66,
  68,66,68,116,68,116,114,68,66,34,194,33,194,17,113,39,
  119,33,114,194,33,195,17,196,17,194,18,39,114,18,34,195,
  114,195,119,36,119,116,194,119,114,119,116,36,68,100,118,102,
  129,17,81,16,248,0,1,24,134,102,100,70,68,36,116,34,
  119,39,71,39,194,36,39,23,34,194,17,23,39,114,119,34,
  18,196,17,196,17,33,34,33,17,194,33,39,119,34,195,119,
  71,196,119,39,119,68,66,196,70,133,17,19,16,248,0,1,
  17,134,195,102,98,194,71,194,34,114,68,116,68,66,195,113,
  194,17,113,18,113,17,34,39,33,195,17,195,17,18,17,18,
  34,195,17,113,119,113,194,119,114,36,194,119,194,116,119,116,
  68,119,116,71,116,68,136,17,81,17,248,0,17,24,134,102,
  100,70,100,68,116,34,194,39,34,194,36,34,39,114,195,17,
  194,23,119,114,34,17,23,195,17,198,17,34,33,194,17,23,
  197,119,39,194,119,71,68,119,39,71,194,119,194,71,68,104,
  81,21,17,248,0,17,24,194,102,70,68,100,68,66,34,71,
  194,114,66,194,34,119,113,195,17,113,17,113,194,119,113,196,
  17,195,17,197,18,194,17,18,196,119,114,34,119,114,195,116,
  39,36,116,119,68,116,68,104,129,17,81,16,246,0,1,17,
  136,102,194,100,70,102,68,194,36,68,119,39,195,34,39,195,
  17,194,23,17,23,18,119,194,18,195,17,197,17,33,34,33,
  18,195,23,195,119,39,36,39,119,68,66,71,34,116,68,116,
  68,71,70,104,129,21,194,17,246,0,1,17,136,194,102,100,
  68,102,100,66,34,66,119,114,195,34,114,33,195,17,119,18,
  17,34,194,33,196,17,197,17,194,18,194,17,23,113,195,119,
  195,114,34,119,116,195,36,71,119,116,36,116,194,102,133,17,
  83,17,16,245,0,194,17,134,102,100,68,36,70,102,68,194,
  36,39,36,194,34,66,39,18,194,17,23,119,194,17,194,18,
  197,17,197,17,33,39,23,17,23,17,194,23,39,119,39,114,
  34,71,34,114,66,196,68,119,68,194,102,136,194,17,49,16,
  245,0,17,24,134,102,70,66,194,68,102,68,98,66,194,34,
  66,194,34,119,113,33,17,119,194,113,194,17,194,33,196,17,
  199,17,194,113,17,113,194,17,195,119,34,36,195,34,196,68,
  66,36,70,100,70,104,81,195,17,244,0,1,17,24,195,70,
  68,34,195,70,68,66,34,39,116,194,34,39,119,17,194,23,
  17,23,200,17,197,17,194,23,196,119,23,194,119,39,194,34,
  194,66,68,66,195,68,66,34,68,194,70,68,72,133,17,19,
  17,244,0,1,17,136,100,102,100,68,66,34,195,100,66,68,
  114,119,66,194,34,196,119,113,201,17,197,17,113,195,119,113,
  114,194,113,114,194,34,194,36,194,68,36,195,68,194,36,68,
  102,100,70,72,136,194,17,49,16,243,0,17,24,136,195,70,
  194,68,194,36,195,68,36,68,71,36,194,34,39,195,23,202,
  17,197,17,23,194,119,23,17,194,39,23,195,34,66,68,66,
  194,68,194,66,195,68,194,70,68,102,100,136,194,17,49,16,
  242,0,1,17,24,132,100,102,194,100,195,68,194,66,194,68,
  66,68,194,66,34,33,195,113,33,201,17,198,17,195,113,18,
  33,113,34,18,34,197,36,68,195,36,195,68,194,102,70,68,
  136,129,17,19,17,242,0,194,17,136,134,68,70,194,102,194,
  70,68,195,36,68,196,36,195,34,18,17,18,201,17,198,17,
  194,23,119,194,18,17,196,34,66,68,66,68,34,195,66,195,
  68,70,194,100,68,72,129,194,17,49,16,240,0,1,194,17,
  136,100,68,194,100,194,102,100,196,66,36,66,68,194,66,34,
  33,34,33,34,194,33,194,17,33,197,17,199,17,113,196,17,
  194,18,194,34,194,68,36,34,196,36,196,68,70,195,68,136,
  196,17,240,0,194,17,24,134,70,68,194,70,68,194,102,68,
  195,36,34,194,36,68,66,194,34,18,34,194,18,34,33,34,
  18,197,17,198,17,195,33,195,17,33,196,34,68,66,36,34,
  66,34,197,68,66,195,68,136,129,17,19,17,239,0,1,194,
  17,136,132,100,68,66,68,70,68,102,68,66,34,66,36,34,
  68,66,194,34,33,194,34,33,34,33,34,194,18,33,196,17,
  197,17,18,34,194,18,195,17,18,196,34,36,68,66,36,34,
  36,197,68,195,36,68,72,136,194,17,49,16,238,0,194,17,
  24,136,70,68,195,36,68,194,70,194,68,34,36,34,194,68,
  199,34,18,194,34,17,194,33,196,17,197,17,34,33,17,194,
  33,194,17,33,200,34,66,34,196,68,196,66,68,72,136,196,
  17,237,0,1,194,17,24,136,68,196,66,70,68,102,68,66,
  34,66,202,34,195,33,199,17,196,17,18,34,194,17,18,195,
  17,18,34,18,198,34,36,34,36,196,68,34,195,36,70,136,
  129,17,19,49,16,236,0,17,81,17,136,134,68,194,36,34,
  36,70,102,100,68,34,36,201,34,18,194,34,194,18,199,17,
  197,17,33,195,17,33,194,17,194,34,33,199,34,66,196,68,
  194,66,34,66,194,68,104,136,194,17,51,17,235,0,1,21,
  17,24,136,100,68,194,66,34,66,196,68,194,66,200,34,195,
  33,194,34,33,199,17,196,17,18,194,17,194,18,194,17,18,
  194,34,18,34,194,18,196,34,68,194,36,194,68,36,34,194,
  36,70,68,102,136,129,17,83,49,16,234,0,194,81,17,136,
  134,195,68,36,34,194,36,194,68,194,36,66,196,34,194,18,
  34,18,34,18,33,18,34,18,199,17,197,17,33,194,17,202,
  33,195,34,68,196,66,34,194,66,194,68,195,100,194,136,17,
  21,194,17,233,0,194,21,17,24,136,132,100,68,100,68,195,
  66,34,195,66,68,66,194,34,197,33,194,34,196,33,199,17,
  196,17,18,34,17,18,17,195,18,34,33,17,18,34,17,18,
  194,34,68,34,68,195,34,195,36,68,195,70,72,136,129,194,
  17,49,16,231,0,17,194,81,17,194,136,102,194,70,194,68,
  194,36,195,34,36,66,36,66,194,34,17,18,34,195,18,194,
  17,194,18,200,17,197,17,34,33,195,17,195,34,33,17,33,
  194,17,196,34,36,34,66,34,195,66,198,68,194,136,194,17,
  19,17,230,0,1,21,85,17,24,136,132,102,195,100,68,196,
  66,34,66,36,196,34,33,194,17,34,198,33,200,17,197,17,
  18,194,34,194,18,194,34,18,34,17,18,17,194,18,198,34,
  194,36,34,194,36,68,36,195,68,72,136,129,195,17,16,229,
  0,17,85,81,17,194,136,68,194,70,68,36,68,36,34,194,
  36,199,34,18,195,17,194,34,18,17,18,17,194,18,198,17,
  198,17,194,33,195,34,33,194,34,195,17,33,194,34,33,199,
  34,195,66,34,196,68,194,136,17,194,19,17,228,0,1,194,
  85,17,24,136,132,195,68,66,34,195,66,198,34,33,194,34,
  194,33,199,17,18,33,34,33,197,17,199,17,196,18,17,194,
  18,194,17,194,18,34,195,18,34,66,196,34,197,36,195,68,
  72,136,129,17,81,17,16,226,0,1,21,85,81,17,194,136,
  196,68,196,36,197,34,66,34,195,18,34,18,199,17,18,195,
  34,197,17,200,17,195,33,17,194,33,195,17,196,33,199,34,
  195,66,68,66,195,68,72,194,136,17,21,19,17,16,224,0,
  1,17,194,85,17,24,194,136,195,68,66,68,196,66,194,34,
  36,195,34,197,33,199,17,195,34,194,33,196,17,201,17,18,
  17,194,18,195,17,195,18,34,18,199,34,36,34,36,34,194,
  36,68,36,68,194,136,129,17,194,49,17,223,0,1,17,194,
  85,194,17,194,136,132,68,36,68,36,34,36,34,36,200,34,
  18,34,194,18,199,17,196,34,18,196,17,201,17,33,17,34,
  33,194,17,33,194,17,194,33,34,33,199,34,194,66,34,196,
  66,68,72,194,136,195,17,49,17,221,0,1,17,194,85,81,
  17,24,194,136,68,197,66,34,194,66,198,34,33,34,195,33,
  195,17,34,33,195,17,194,33,34,194,33,196,17,200,17,18,
  17,194,18,196,17,194,18,195,34,18,199,34,197,36,34,194,
  36,68,40,136,129,194,17,19,194,17,219,0,1,17,21,194,
  85,17,21,194,136,36,68,36,34,197,36,200,34,18,195,34,
  18,17,18,194,34,195,17,194,18,194,34,18,196,17,201,17,
  33,17,33,194,17,194,33,17,195,34,195,33,34,33,200,34,
  66,34,194,66,68,66,194,136,81,195,17,49,17,217,0,1,
  17,21,194,85,194,17,194,136,130,68,195,66,34,66,199,34,
  33,34,195,33,195,34,33,17,195,34,33,195,17,196,33,196,
  17,200,17,18,17,18,194,17,195,18,17,18,34,199,18,199,
  34,194,36,34,36,68,36,72,194,136,196,17,49,17,16,214,
  0,1,17,195,85,194,17,24,194,136,68,36,68,34,194,36,
  200,34,199,18,34,17,194,18,194,34,18,195,17,18,34,18,
  33,196,17,200,17,194,33,195,17,194,34,33,17,34,18,33,
  34,194,33,194,17,33,34,33,194,34,36,198,34,66,34,68,
  194,136,129,195,17,19,194,49,16,212,0,1,17,195,85,81,
  194,17,194,136,132,66,34,66,200,34,33,34,194,33,194,17,
  33,34,194,33,34,194,17,195,34,194,33,195,17,194,33,197,
  17,200,17,195,18,17,18,34,18,34,18,34,33,18,33,195,
  17,194,18,194,34,18,194,34,36,196,34,36,34,36,68,72,
  194,136,129,196,17,19,51,17,209,0,194,17,21,195,85,81,
  194,17,195,136,194,68,34,36,196,34,36,195,34,18,194,34,
  18,196,17,34,194,18,17,18,196,34,18,195,17,18,198,17,
  201,17,33,195,17,33,17,34,33,34,194,17,33,195,17,18,
  33,34,194,33,199,34,194,66,195,68,72,194,136,129,198,17,
  49,194,17,204,0,195,17,196,85,195,17,195,136,195,68,195,
  66,198,34,194,33,34,33,34,196,17,33,17,18,33,17,195,
  34,195,33,201,17,201,17,18,194,17,18,34,17,34,200,17,
  194,18,34,18,194,34,36,197,34,36,34,36,68,36,68,195,
  136,198,17,194,81,49,195,17,199,0,195,17,197,85,195,17,
  24,194,136,132,68,36,68,34,36,201,34,18,34,18,199,17,
  34,17,18,195,34,18,202,17,201,17,194,33,194,17,33,17,
  194,33,199,17,195,34,33,197,34,33,194,34,194,66,68,66,
  194,68,72,195,136,196,17,197,21,85,49,199,17,53,197,85,
  81,195,17,24,195,136,194,68,66,68,195,66,34,33,197,34,
  33,195,34,33,198,17,34,194,17,194,33,34,194,33,17,33,
  199,17,206,17,18,34,200,17,194,34,194,18,195,34,18,17,
  194,18,34,36,34,195,36,194,68,72,194,136,120,113,196,17,
  195,81,206,85,196,17,194,120,194,136,195,68,194,36,34,36,
  194,34,18,34,194,18,195,34,194,18,34,33,198,17,194,34,
  194,18,34,194,18,17,194,18,199,17,205,17,194,34,33,194,
  17,18,194,17,33,17,18,194,33,196,34,33,195,17,195,34,
  66,34,194,66,195,68,72,194,135,194,113,195,17,195,21,204,
  85,197,17,113,119,135,136,195,68,195,66,34,66,195,34,33,
  194,17,196,34,194,33,34,194,17,33,194,17,18,33,17,34,
  33,34,194,33,194,17,33,199,17,204,17,18,34,18,198,17,
  18,17,194,18,194,34,18,194,34,195,17,195,18,34,198,36,
  68,34,119,120,119,23,7,198,17,21,199,85,198,17,195,23,
  120,119,114,36,68,197,36,194,34,18,34,195,17,18,34,194,
  18,194,34,18,17,18,194,17,18,197,17,18,34,196,17,18,
  198,17,204,17,194,33,198,17,33,18,33,17,194,34,194,17,
  194,33,196,17,33,196,34,36,66,34,195,66,34,135,194,119,
  113,112,113,209,17,194,113,195,119,130,34,195,66,34,68,196,
  34,33,34,17,194,33,17,194,33,17,18,194,33,17,34,17,
  33,211,17,212,17,18,33,195,18,17,194,18,34,195,17,33,
  195,18,198,34,195,36,194,34,39,119,23,17,23,7,113,17,
  113,203,17,194,23,17,194,23,119,194,34,195,36,199,34,18,
  34,17,194,18,17,195,18,17,195,18,17,34,195,17,33,208,
  17,212,17,195,33,18,34,195,17,33,196,17,18,33,34,194,
  33,198,34,194,66,194,34,39,195,113,17,112,113,17,23,17,
  113,197,17,113,194,17,113,17,194,113,119,195,34,194,66,197,
  34,194,33,34,194,33,194,17,34,33,17,33,195,17,34,17,
  195,33,211,17,212,17,18,194,17,34,18,17,194,18,34,194,
  17,33,194,17,194,18,17,195,18,195,34,194,36,34,17,34,
  17,18,119,194,17,194,23,194,17,195,23,194,17,113,198,17,
  23,114,17,18,33,18,194,36,196,34,194,18,17,194,18,34,
  194,17,18,33,195,18,17,194,18,33,17,18,212,17,213,17,
  194,34,17,34,194,33,34,33,198,17,194,33,194,17,34,33,
  196,34,33,34,195,33,194,17,113,197,17,113,119,113,194,17,
  113,17,113,194,17,113,194,17,194,33,34,33,196,34,33,34,
  33,194,17,194,33,34,33,17,34,33,17,33,17,195,33,18,
  34,33,212,17,216,17,18,195,34,199,17,34,18,17,198,18,
  17,199,18,199,17,23,200,17,199,18,17,198,18,17,194,18,
  33,196,18,194,17,194,18,34,18,216,17
}

RES_PCX_DOGGY =
{
  10,5,1,4,0,0,0,0,151,0,28,0,0,0,0,0,
  0,0,0,20,52,100,255,255,255,36,159,222,188,74,155,255,
  0,255,255,0,255,255,0,255,255,0,255,255,0,255,255,0,
  255,255,0,255,255,0,255,255,0,255,255,0,255,255,0,255,
  0,1,76,0,1,0,0,0,0,0,0,0,0,0,0,0,
  0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,
  0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,
  0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,
  207,0,68,210,0,64,210,0,68,210,0,68,195,0,195,0,
  4,64,201,0,4,51,64,208,0,68,52,209,0,4,51,64,
  198,0,64,201,0,4,51,64,194,0,195,0,67,52,200,0,
  4,67,34,52,64,206,0,4,51,35,68,208,0,67,34,52,
  197,0,4,52,201,0,67,34,52,194,0,194,0,4,50,35,
  64,199,0,67,50,17,35,52,196,0,68,64,200,0,67,34,
  18,51,64,196,0,4,64,200,0,4,50,17,35,64,196,0,
  67,35,64,199,0,4,50,17,35,64,0,194,0,67,33,18,
  52,198,0,4,50,33,17,34,35,64,194,0,4,51,52,199,
  0,4,50,17,18,34,52,196,0,67,52,200,0,67,33,17,
  18,52,195,0,4,50,18,52,199,0,67,33,17,18,52,0,
  0,4,50,17,18,52,198,0,67,33,195,17,18,52,194,0,
  67,34,35,64,198,0,67,34,195,17,35,64,194,0,4,50,
  35,64,198,0,4,50,194,17,18,35,64,194,0,67,33,17,
  35,64,198,0,67,33,17,18,35,64,0,4,50,17,18,52,
  198,0,67,33,195,17,35,64,0,4,50,17,18,52,197,0,
  4,50,196,17,35,64,194,0,67,33,18,52,198,0,4,50,
  195,17,18,52,194,0,67,33,17,35,64,196,0,4,68,50,
  195,17,18,52,0,4,50,194,17,35,198,68,67,33,194,17,
  18,52,194,0,4,50,17,18,52,198,0,67,33,194,17,18,
  52,195,0,67,33,18,52,197,0,68,67,33,195,17,18,52,
  194,0,67,33,17,35,197,68,67,51,50,195,17,18,52,194,
  0,67,33,17,35,198,51,50,194,17,18,35,64,194,0,4,
  50,17,18,52,198,68,50,33,17,18,35,64,195,0,67,33,
  18,52,197,68,51,50,33,194,17,34,35,64,194,0,67,33,
  17,35,197,51,50,34,33,194,17,34,35,64,194,0,4,50,
  17,18,198,34,33,194,17,35,52,195,0,4,50,194,17,35,
  198,51,33,194,17,35,52,196,0,67,33,18,198,51,34,33,
  194,17,18,51,52,195,0,4,50,17,18,197,34,33,195,17,
  34,51,52,0,195,0,67,34,202,17,35,64,196,0,67,33,
  17,199,34,194,17,18,52,64,196,0,4,50,17,198,34,196,
  17,35,68,64,195,0,4,50,203,17,35,68,64,0,195,0,
  4,51,33,200,17,18,52,197,0,4,50,202,17,18,52,197,
  0,4,50,202,17,18,52,198,0,67,34,33,200,17,18,52,
  195,0,195,0,4,50,201,17,18,52,198,0,67,34,201,17,
  35,64,198,0,67,34,201,17,18,52,198,0,4,50,201,17,
  35,64,195,0,195,0,4,50,201,17,18,52,198,0,4,50,
  201,17,35,64,198,0,67,33,201,17,35,64,198,0,4,50,
  201,17,35,64,195,0,195,0,67,33,201,17,18,52,198,0,
  67,33,201,17,35,64,198,0,67,33,201,17,35,64,198,0,
  4,50,201,17,35,64,195,0,195,0,67,33,195,17,33,197,
  17,18,52,198,0,67,33,201,17,35,64,198,0,4,50,201,
  17,35,64,198,0,4,50,195,17,34,196,17,18,52,196,0,
  195,0,67,33,195,17,34,33,196,17,18,52,198,0,4,50,
  200,17,18,52,199,0,4,50,195,17,34,196,17,18,52,199,
  0,4,50,195,17,35,34,195,17,18,52,196,0,194,0,4,
  67,33,194,17,18,51,50,33,195,17,35,64,197,0,4,68,
  50,194,17,194,34,196,17,18,52,199,0,4,50,194,17,18,
  51,34,33,194,17,35,64,199,0,67,33,17,18,17,35,51,
  33,194,17,18,52,196,0,194,0,67,50,33,18,17,18,52,
  67,33,18,33,17,35,64,197,0,67,51,50,194,17,35,51,
  34,195,17,35,64,199,0,67,33,194,17,18,52,51,50,194,
  17,35,64,198,0,4,50,194,17,34,17,35,67,33,18,33,
  18,51,64,195,0,0,4,50,33,17,18,17,18,52,67,33,
  18,50,17,35,64,196,0,4,50,194,34,17,18,52,68,51,
  34,194,17,35,64,198,0,4,50,195,17,18,52,68,50,17,
  33,18,52,198,0,67,33,17,18,50,17,35,67,33,18,50,
  33,34,52,195,0,0,67,33,17,18,34,17,35,64,67,33,
  18,50,17,35,64,196,0,67,33,195,17,18,52,0,68,51,
  33,17,35,64,198,0,67,33,195,17,35,64,4,50,17,34,
  17,35,64,196,0,4,50,17,18,35,33,18,52,67,33,18,
  51,33,17,35,64,194,0,0,67,33,18,35,33,17,35,64,
  67,33,18,51,33,18,52,196,0,67,33,194,17,18,35,64,
  194,0,67,33,17,35,64,198,0,67,33,17,18,34,52,0,
  4,50,17,35,34,18,52,196,0,4,50,17,35,50,17,35,
  64,67,33,35,68,50,33,35,64,194,0,4,50,17,35,51,
  33,17,35,68,50,17,35,67,33,18,52,195,0,4,50,17,
  34,17,18,52,195,0,67,33,17,35,64,198,0,67,33,17,
  18,51,64,0,4,50,17,35,50,18,52,196,0,4,50,18,
  52,50,17,35,64,67,33,18,52,67,33,18,52,194,0,4,
  50,17,35,67,33,17,35,67,33,17,35,67,33,17,35,64,
  194,0,4,50,18,51,33,18,52,194,0,4,50,194,17,35,
  64,198,0,67,33,34,18,52,194,0,4,50,17,35,67,33,
  35,64,195,0,4,50,18,52,67,33,18,52,67,33,35,64,
  4,50,18,52,194,0,4,50,18,52,4,50,33,18,50,17,
  34,52,4,50,17,34,52,194,0,4,50,18,52,50,17,35,
  64,194,0,67,34,17,18,52,198,0,67,33,18,17,35,64,
  0,4,50,17,35,67,33,35,64,195,0,4,50,18,52,4,
  50,17,35,67,33,35,64,0,67,33,35,64,0,67,33,18,
  52,0,67,50,17,35,34,51,64,0,67,33,17,35,64,0,
  4,50,18,52,67,33,18,52,194,0,4,51,33,17,35,64,
  197,0,67,33,18,34,52,194,0,4,50,17,18,52,50,52,
  196,0,4,50,18,52,0,67,33,18,51,33,18,52,0,4,
  50,52,194,0,4,50,35,64,0,4,67,34,52,51,68,194,
  0,4,50,34,52,195,0,67,35,64,4,50,35,64,195,0,
  68,50,34,52,198,0,4,50,35,51,64,195,0,67,34,35,
  64,67,64,197,0,67,35,64,0,4,50,35,68,50,35,64,
  194,0,67,64,194,0,0,67,52,195,0,4,51,64,68,196,
  0,67,51,64,195,0,4,52,194,0,67,52,197,0,67,51,
  64,199,0,67,52,68,196,0,4,51,52,0,4,198,0,4,
  52,195,0,67,52,0,67,52,195,0,4,195,0,0,4,64,
  196,0,68,198,0,4,68,197,0,64,194,0,4,64,197,0,
  4,68,200,0,4,64,198,0,68,64,201,0,64,195,0,4,
  64,0,4,64,199,0
}

RES_PCX_HELI =
{
  10,5,1,4,0,0,0,0,12,0,5,0,0,0,0,0,
  0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,
  0,0,0,0,0,0,0,0,0,0,0,12,25,22,25,51,
  45,37,86,46,70,130,50,0,0,0,0,0,0,0,0,0,
  0,1,8,0,1,0,0,0,0,0,0,0,0,0,0,0,
  0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,
  0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,
  0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,
  195,0,12,196,0,195,0,12,196,0,195,0,193,204,193,192,
  195,0,194,0,194,204,12,195,0,197,204,193,192,194,0,195,
  0,193,192,12,195,0
}

RES_PCX_SAND =
{
  10,5,1,4,0,0,0,0,239,0,81,0,0,0,0,0,
  0,0,0,38,34,90,92,84,114,61,69,128,80,109,150,123,
  114,132,124,197,214,161,141,134,192,173,148,227,209,156,238,232,
  167,189,226,234,226,243,205,255,255,255,255,0,0,0,255,0,
  0,1,120,0,1,0,0,0,0,0,0,0,0,0,0,0,
  0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,
  0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,
  0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,
  242,0,6,182,255,0,197,0,240,0,107,195,221,193,214,255,
  0,196,0,238,0,107,198,221,193,214,255,0,195,0,235,0,
  6,189,196,221,193,219,107,176,109,193,221,96,255,0,194,0,
  233,0,107,197,221,182,194,0,11,176,0,109,193,221,96,255,
  0,0,231,0,6,197,221,182,196,0,11,176,194,0,109,193,
  221,96,255,0,231,0,107,194,221,193,219,96,198,0,107,176,
  194,0,6,194,221,96,254,0,231,0,194,187,193,221,200,0,
  187,96,195,0,6,193,221,193,214,254,0,231,0,195,187,96,
  199,0,187,96,196,0,6,193,221,193,214,253,0,231,0,187,
  96,107,189,96,198,0,187,198,0,6,193,221,193,214,252,0,
  231,0,187,96,0,107,189,96,197,0,187,199,0,109,193,221,
  96,251,0,230,0,6,187,194,0,6,187,193,214,197,0,182,
  200,0,109,193,221,96,250,0,230,0,6,187,195,0,6,187,
  193,214,196,0,107,201,0,109,193,221,96,249,0,230,0,11,
  187,196,0,6,187,193,214,195,0,182,201,0,6,194,221,96,
  248,0,230,0,11,187,197,0,107,189,96,194,0,107,202,0,
  6,193,221,193,214,248,0,230,0,11,187,198,0,107,189,96,
  0,102,203,0,109,193,221,248,0,230,0,11,187,199,0,107,
  193,221,96,70,200,0,107,195,221,182,248,0,230,0,11,187,
  199,0,6,187,193,214,4,198,0,107,194,221,196,187,248,0,
  230,0,11,182,200,0,6,187,193,214,196,0,107,194,221,193,
  219,187,102,0,11,187,8,194,170,193,201,128,243,0,34,32,
  228,0,11,182,201,0,6,189,193,214,0,107,194,221,193,219,
  187,102,195,0,11,187,194,169,172,193,204,193,201,128,241,0,
  34,195,34,227,0,107,176,202,0,107,189,194,221,193,219,187,
  102,197,0,11,182,138,194,154,194,204,152,239,0,2,194,34,
  197,34,225,0,107,182,201,0,6,0,107,189,193,219,182,168,
  112,197,0,107,184,152,153,194,169,170,193,201,128,236,0,2,
  196,34,199,34,223,0,187,176,201,0,11,100,43,189,136,120,
  138,169,128,195,0,66,187,184,121,137,196,154,169,128,233,0,
  2,198,34,201,34,32,220,0,187,176,201,0,11,98,91,193,
  219,119,135,136,137,151,194,0,66,36,187,101,135,136,153,154,
  153,169,194,170,152,230,0,201,34,204,34,218,0,187,176,201,
  0,70,69,107,193,214,194,119,87,136,153,112,4,34,38,187,
  135,87,121,194,137,169,196,154,152,226,0,2,203,34,207,34,
  215,0,187,176,200,0,4,36,87,187,193,215,194,117,85,120,
  194,136,114,34,43,187,117,194,119,153,152,137,152,195,169,170,
  152,222,0,2,206,34,211,34,211,0,187,176,199,0,4,194,
  34,120,187,193,215,194,87,85,88,120,135,34,37,123,182,87,
  88,153,135,121,120,194,137,138,195,154,152,217,0,2,210,34,
  215,34,32,206,0,187,96,199,0,194,34,37,135,189,181,117,
  85,87,194,135,34,37,119,182,178,85,138,172,193,201,194,135,
  136,195,152,196,169,128,211,0,215,34,220,34,32,201,0,187,
  64,198,0,195,34,88,118,189,103,194,85,194,120,114,34,87,
  119,107,101,40,194,154,193,204,152,120,152,119,195,137,138,195,
  154,152,205,0,220,34,229,34,38,187,201,34,37,119,123,189,
  69,195,82,114,34,37,119,117,182,98,41,194,169,170,193,201,
  135,121,135,119,120,195,152,153,194,169,152,232,34,229,34,43,
  187,197,34,37,120,82,37,194,119,91,189,194,85,37,195,34,
  87,119,85,102,66,121,153,194,154,172,152,119,121,135,119,136,
  195,137,138,152,154,152,231,34,229,34,123,187,194,119,114,34,
  37,87,194,135,194,119,117,123,193,219,85,194,82,194,34,87,
  194,117,86,100,39,152,153,137,194,169,153,135,119,152,169,119,
  120,136,194,152,153,137,169,130,230,34,219,34,37,195,34,37,
  34,37,87,194,119,123,187,119,195,34,87,197,119,114,75,193,
  214,194,37,194,34,87,119,87,85,38,66,120,137,136,194,152,
  154,153,152,119,121,154,169,135,119,136,195,137,136,169,130,229,
  34,217,34,195,82,85,37,85,82,194,85,119,121,154,166,187,
  85,82,37,199,117,34,107,193,212,194,82,85,194,119,194,117,
  34,36,39,136,152,135,153,194,137,169,153,194,135,169,195,170,
  152,119,195,152,194,153,130,194,34,37,198,85,196,82,215,34,
  215,34,197,37,196,85,195,121,153,167,84,107,181,195,37,85,
  194,87,82,195,34,189,193,213,39,120,136,135,119,87,34,194,
  66,120,152,135,152,120,152,194,136,170,194,120,121,195,154,194,
  170,135,194,137,136,121,153,152,119,195,85,197,87,195,37,34,
  82,213,34,215,34,195,82,194,85,195,117,87,195,151,154,117,
  34,38,187,197,82,195,34,37,135,189,193,215,194,135,133,194,
  117,82,195,34,136,194,135,194,119,135,194,137,136,195,135,153,
  152,137,153,169,168,119,153,194,152,151,137,194,153,151,119,197,
  117,85,194,82,85,194,34,37,211,34,211,34,196,37,85,37,
  197,87,196,121,170,151,82,37,34,189,181,198,34,87,194,120,
  189,184,194,120,88,87,37,34,36,34,39,136,120,195,119,194,
  120,194,154,153,136,194,120,135,119,120,194,153,152,120,121,194,
  137,152,194,120,153,196,119,194,87,198,85,82,85,34,82,208,
  34,210,34,36,102,194,82,195,85,194,117,195,119,151,194,153,
  194,154,117,85,194,82,43,193,219,82,195,34,194,117,194,119,
  86,189,103,194,119,114,194,34,37,34,39,136,194,135,196,119,
  135,137,169,170,153,194,135,120,195,119,120,153,195,135,120,153,
  151,135,137,194,120,194,119,194,117,119,117,197,85,34,85,209,
  34,209,34,37,195,68,102,85,194,87,196,119,194,121,122,154,
  170,169,82,34,82,194,34,189,181,194,34,37,87,85,82,34,
  43,193,219,197,34,37,34,39,194,136,120,119,194,120,195,119,
  136,194,120,121,152,136,119,170,152,196,119,194,120,119,154,195,
  152,195,135,195,119,195,87,200,85,34,85,37,82,203,34,204,
  34,85,194,34,82,37,83,51,52,99,68,101,194,117,196,119,
  194,151,153,194,170,151,34,82,195,37,36,193,219,82,37,194,
  85,82,34,37,43,193,214,198,34,39,194,136,135,119,117,194,
  135,136,193,202,152,119,135,153,135,153,194,119,138,170,169,135,
  196,119,120,169,153,194,137,136,194,120,196,119,194,117,201,85,
  34,82,194,34,82,201,34,202,34,85,34,85,34,194,85,82,
  34,17,20,70,49,55,194,87,194,119,195,121,153,195,154,114,
  82,197,34,77,181,85,37,196,34,75,180,37,196,34,39,136,
  195,120,117,88,120,136,154,193,204,193,201,135,119,137,152,120,
  151,194,119,137,170,168,138,170,152,120,153,194,170,195,152,194,
  135,196,119,194,87,198,85,82,37,34,85,37,82,37,202,34,
  203,34,85,34,196,85,33,195,17,19,53,194,119,194,151,119,
  194,151,153,194,169,167,195,34,82,34,82,34,36,187,82,197,
  34,107,98,34,37,194,34,120,136,195,135,133,82,87,136,137,
  169,172,193,204,152,119,120,153,151,137,135,194,119,194,137,194,
  154,195,170,194,169,195,153,195,120,195,119,194,117,119,87,117,
  87,85,119,85,82,85,34,82,37,194,82,201,34,204,34,197,
  85,197,37,195,119,195,121,195,153,194,154,114,37,194,85,37,
  194,34,82,34,75,101,195,34,37,85,182,66,82,34,39,196,
  120,119,117,194,37,40,194,153,154,122,170,193,202,152,119,120,
  153,120,152,194,119,120,154,197,170,194,154,153,194,152,196,135,
  195,119,87,85,117,87,117,199,85,82,37,194,85,200,34,203,
  34,85,37,85,82,196,85,194,82,85,196,119,196,151,194,153,
  169,85,117,195,82,195,34,82,36,182,197,82,100,194,34,120,
  197,135,133,195,82,194,137,153,194,135,137,170,169,135,119,120,
  151,137,135,119,194,137,153,169,194,170,196,169,195,137,194,120,
  199,119,117,87,85,119,198,85,82,85,201,34,202,34,85,37,
  85,82,199,85,117,194,87,194,119,196,121,194,153,151,87,195,
  37,34,37,194,82,34,199,37,66,37,137,197,119,117,194,37,
  34,40,152,136,149,119,194,120,194,154,152,194,119,194,120,151,
  154,194,152,194,170,169,154,170,154,153,196,152,195,135,199,119,
  194,117,201,85,82,200,34,201,34,37,82,201,85,117,85,194,
  117,198,119,194,151,149,117,194,85,82,194,85,82,194,34,196,
  82,196,34,120,153,169,194,153,119,85,195,82,34,40,136,135,
  85,87,119,135,137,169,193,201,135,194,119,135,119,121,195,169,
  170,194,169,196,153,194,137,136,194,120,102,71,196,119,87,117,
  87,194,85,117,197,85,82,85,201,34,200,34,37,82,199,85,
  87,85,194,87,85,87,85,197,119,194,121,119,194,87,37,85,
  197,37,195,34,37,196,34,120,196,154,119,117,85,37,194,34,
  194,120,135,85,87,119,85,194,120,154,156,169,135,119,120,152,
  119,120,194,154,100,194,154,195,153,195,152,135,102,195,68,196,
  119,117,87,117,87,199,85,82,85,82,200,34,201,34,196,85,
  37,202,85,197,119,194,151,149,194,117,114,117,85,194,82,83,
  98,195,34,194,82,120,153,169,194,136,195,135,114,195,82,34,
  135,136,135,117,85,195,117,87,119,120,194,169,196,153,170,129,
  170,166,67,57,194,169,194,153,195,137,134,68,54,67,51,53,
  194,117,195,119,85,194,117,200,85,201,34,198,34,82,34,85,
  82,85,82,198,85,87,194,85,197,87,197,119,194,87,194,119,
  87,195,85,51,70,37,34,39,194,153,194,154,152,194,136,194,
  120,119,194,37,194,34,194,120,135,194,85,119,194,87,117,85,
  195,119,138,169,195,153,154,147,26,151,17,24,195,153,196,152,
  147,19,100,65,17,34,39,87,85,117,87,117,201,85,202,34,
  199,34,85,82,85,82,199,85,194,117,85,119,195,117,198,119,
  151,194,119,195,117,85,83,17,52,101,121,194,137,152,137,136,
  169,194,117,135,119,169,194,119,120,194,136,194,119,87,119,117,
  197,135,152,120,121,195,170,194,169,147,25,152,194,119,137,196,
  153,194,137,131,49,195,17,18,85,117,119,87,117,198,85,37,
  82,85,82,202,34,198,34,37,82,206,85,87,195,119,82,195,
  119,87,117,119,194,87,194,119,194,117,17,19,53,205,153,152,
  136,154,153,197,154,152,120,119,121,194,137,153,198,154,145,25,
  198,153,136,194,152,136,135,130,196,82,194,87,85,117,198,85,
  34,194,85,194,82,202,34,199,34,85,37,85,37,85,37,203,
  85,117,114,194,34,85,202,119,117,85,151,153,151,206,153,154,
  198,153,194,169,153,170,153,197,169,147,25,49,200,153,194,137,
  136,120,119,194,37,119,194,117,85,119,85,119,87,197,85,82,
  85,37,82,85,201,34,198,34,82,206,85,82,85,194,82,34,
  39,196,87,195,119,194,121,119,195,121,153,199,121,194,153,121,
  153,121,196,153,154,194,153,194,169,194,153,169,196,153,195,154,
  195,153,147,25,17,196,153,152,153,136,195,152,135,198,119,87,
  201,85,34,198,85,200,34,199,34,205,85,82,85,194,82,34,
  37,198,119,194,117,196,119,196,151,119,206,151,194,153,121,194,
  153,194,137,194,153,154,194,169,198,153,145,194,25,145,153,198,
  137,136,195,120,197,119,117,87,194,117,85,117,195,85,82,37,
  194,85,37,82,85,201,34,200,34,85,82,204,85,37,34,37,
  85,194,87,70,198,87,203,119,198,121,196,119,195,121,153,121,
  136,194,152,153,152,195,153,154,170,154,153,195,152,17,24,147,
  136,198,152,196,135,197,119,194,87,117,85,194,87,194,85,82,
  37,194,85,37,82,195,85,200,34,201,34,37,82,85,194,34,
  200,85,82,37,195,85,83,52,101,195,117,194,119,194,117,203,
  119,87,200,119,196,151,153,198,137,198,153,194,137,129,25,49,
  153,136,195,137,195,136,120,196,119,117,194,85,194,117,196,85,
  37,196,85,34,85,37,85,202,34,201,34,82,85,195,82,37,
  194,34,37,196,85,37,196,85,33,17,37,196,85,87,195,119,
  87,205,119,87,197,119,195,121,120,136,200,152,153,102,72,194,
  152,145,19,49,136,197,135,130,196,119,87,85,194,119,194,87,
  198,85,194,82,37,194,85,37,194,85,202,34,203,34,195,85,
  196,82,37,82,198,85,82,194,34,197,85,195,117,197,119,23,
  119,117,194,119,87,195,119,197,117,119,87,195,119,196,120,136,
  195,137,134,73,134,194,67,194,137,136,17,24,197,120,114,194,
  34,117,119,199,117,195,85,82,85,34,199,85,82,202,34,204,
  34,85,34,85,37,209,85,82,195,85,87,119,195,87,23,194,
  87,81,194,119,195,87,196,119,194,87,197,119,197,135,194,136,
  100,68,194,51,49,102,56,135,129,119,129,23,135,196,119,194,
  34,82,87,82,195,87,195,85,194,82,85,82,198,85,37,82,
  203,34,205,34,85,34,82,37,82,37,85,82,85,34,82,197,
  85,82,85,194,37,85,87,85,119,197,85,19,194,117,113,119,
  197,117,119,117,119,195,117,195,119,87,198,120,118,67,49,102,
  65,22,194,68,71,113,119,17,119,117,85,196,117,194,34,114,
  117,114,199,85,82,194,34,85,37,82,85,34,82,204,34,207,
  34,37,82,37,82,37,85,197,37,194,85,194,82,34,37,196,
  85,119,87,196,85,81,53,85,49,119,194,87,85,195,87,85,
  87,194,119,87,85,87,198,119,87,132,67,51,22,68,52,49,
  51,49,21,23,83,17,195,119,196,87,85,34,194,37,198,85,
  37,197,85,82,208,34,210,34,37,194,82,200,85,82,34,82,
  199,85,82,85,21,81,21,81,53,202,85,194,119,195,117,85,
  198,117,49,201,17,49,21,198,117,195,85,34,37,195,85,194,
  82,200,85,194,82,85,205,34,216,34,196,37,194,34,194,37,
  199,85,34,37,83,85,21,19,204,85,196,87,85,198,87,83,
  195,35,195,51,35,33,83,51,55,197,87,196,85,37,196,34,
  195,37,85,82,37,194,85,210,34,222,34,82,34,82,85,195,
  82,194,85,82,85,33,53,81,17,216,85,194,34,82,194,34,
  194,50,37,194,82,37,200,85,195,82,34,195,82,195,85,37,
  82,37,211,34,225,34,197,37,196,85,194,17,21,196,37,85,
  194,37,206,85,37,199,85,34,37,85,82,37,34,82,201,85,
  194,37,197,85,34,82,213,34,226,34,196,85,197,82,17,18,
  195,85,195,82,85,201,82,194,85,202,82,197,85,196,34,82,
  194,85,82,34,200,85,194,82,85,37,82,212,34,225,34,194,
  37,85,199,37,18,85,195,37,195,85,195,37,199,85,199,37,
  197,85,37,85,37,85,195,82,37,195,85,195,37,85,198,37,
  85,194,37,215,34,227,34,82,200,34,196,82,198,85,196,82,
  85,198,82,195,85,201,82,194,85,37,85,197,82,195,85,200,
  82,85,214,34,235,34,85,82,196,37,85,198,37,82,194,34,
  199,37,196,85,194,37,85,198,37,34,203,37,221,34,234,34,
  37,34,82,195,34,200,82,34,37,82,201,34,195,82,194,85,
  195,34,198,82,196,34,196,82,222,34,235,34,82,195,34,194,
  37,199,34,194,37,82,195,34,198,37,194,85,195,37,34,37,
  237,34,234,34,82,204,34,37,195,34,82,194,34,194,82,195,
  85,194,82,197,34,194,82,195,34,37,232,34,248,34,37,209,
  34,37,34,82,196,34,82,230,34,255,34,209,34,82,231,34,
  255,34,249,34,255,34,249,34,255,34,249,34,255,34,249,34,
  255,34,249,34,255,34,249,34,255,34,249,34,255,34,249,34
}

RES_PCX_SAND_FG =
{
  10,5,1,4,0,0,0,0,239,0,63,0,0,0,0,0,
  0,0,0,0,0,0,26,42,57,19,51,89,42,66,78,59,
  105,95,72,100,112,94,130,116,105,131,132,146,159,141,176,184,
  160,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,
  0,1,120,0,1,0,0,0,0,0,0,0,0,0,0,0,
  0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,
  0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,
  0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,
  32,255,0,248,0,16,255,0,248,0,18,255,0,248,0,17,
  255,0,248,0,17,32,255,0,247,0,17,18,255,0,247,0,
  194,17,32,255,0,246,0,194,17,16,255,0,246,0,194,17,
  18,255,0,246,0,195,17,16,255,0,245,0,195,17,16,255,
  0,245,0,196,17,255,0,245,0,196,17,16,255,0,244,0,
  196,17,16,255,0,244,0,197,17,255,0,244,0,197,17,16,
  255,0,243,0,198,17,255,0,243,0,198,17,16,255,0,242,
  0,198,17,16,255,0,242,0,199,17,255,0,242,0,199,17,
  16,255,0,241,0,200,17,255,0,241,0,200,17,16,197,0,
  33,255,0,234,0,200,17,18,196,0,2,16,255,0,234,0,
  201,17,16,195,0,33,64,255,0,234,0,202,17,194,0,2,
  17,255,0,235,0,202,17,16,0,1,18,255,0,235,0,202,
  17,18,0,33,16,255,0,232,0,1,194,17,203,17,32,17,
  32,255,0,231,0,33,195,17,205,17,255,0,230,0,198,17,
  205,17,255,0,229,0,1,198,17,205,17,194,0,1,16,255,
  0,224,0,1,199,17,205,17,32,0,33,32,255,0,223,0,
  1,200,17,206,17,32,17,194,0,1,0,1,255,0,219,0,
  201,17,208,17,194,0,33,0,33,255,0,212,0,2,197,0,
  2,201,17,208,17,194,0,17,2,16,255,0,212,0,1,32,
  196,0,33,201,17,208,17,32,2,194,17,32,255,0,212,0,
  1,18,195,0,1,202,17,211,17,32,255,0,213,0,2,17,
  194,0,1,203,17,211,17,255,0,215,0,17,32,0,33,203,
  17,211,17,18,255,0,212,0,32,0,33,206,17,212,17,18,
  64,255,0,210,0,18,0,1,206,17,214,17,32,255,0,209,
  0,17,32,1,206,17,215,17,18,255,0,206,0,32,0,33,
  16,33,206,17,217,17,32,255,0,202,0,16,2,16,0,1,
  208,17,218,17,18,255,0,200,0,194,16,1,16,0,33,208,
  17,220,17,32,255,0,198,0,1,18,1,16,33,209,17,221,
  17,32,255,0,198,0,213,17,222,17,36,255,0,197,0,213,
  17,223,17,18,64,255,0,194,0,2,213,17,224,17,18,255,
  0,0,33,214,17,225,17,32,254,0,33,215,17,225,17,18,
  253,0,33,216,17,226,17,32,251,0,33,217,17,227,17,32,
  248,0,4,33,218,17,228,17,32,246,0,2,220,17,228,17,
  18,245,0,2,221,17,229,17,36,243,0,66,222,17,231,17,
  36,239,0,4,33,223,17,233,17,18,64,233,0,66,227,17,
  239,17,36,225,0,4,33,229,17,255,17,249,17,255,17,249,
  17,255,17,249,17,255,17,249,17
}

RES_PCX_KELP_BG =
{
  10,5,1,4,0,0,0,0,239,0,40,0,0,0,0,0,
  0,0,0,19,51,89,72,100,112,45,88,130,68,132,159,105,
  131,132,124,197,214,146,159,141,176,184,160,210,212,172,221,231,
  179,189,226,234,206,241,208,255,255,255,255,0,0,0,255,0,
  0,1,120,0,1,0,0,0,0,0,0,0,0,0,0,0,
  0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,
  0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,
  0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,
  194,64,255,0,247,0,194,64,194,4,64,255,0,244,0,68,
  64,4,0,64,255,0,244,0,4,64,4,0,64,4,255,0,
  240,0,4,64,0,4,64,4,0,64,68,197,0,4,255,0,
  235,0,68,0,0,64,4,0,194,64,0,64,195,0,68,255,
  0,233,0,4,64,4,64,0,64,4,0,68,64,0,4,194,
  0,68,64,255,0,234,0,64,0,64,0,64,4,64,68,64,
  0,4,194,0,194,64,255,0,234,0,68,0,64,0,68,0,
  64,68,64,0,194,68,195,64,194,0,4,255,0,227,0,4,
  0,4,0,68,0,68,0,68,0,64,68,0,4,0,64,194,
  68,64,194,0,4,64,255,0,220,0,4,195,0,64,4,68,
  0,4,0,68,0,68,64,68,0,64,68,195,0,64,4,68,
  195,0,4,255,0,220,0,194,4,64,4,194,64,0,194,68,
  4,0,68,0,68,194,68,0,64,68,195,0,68,0,68,194,
  0,194,4,255,0,220,0,4,0,64,0,68,64,0,194,68,
  4,0,64,0,4,194,68,0,64,68,195,0,4,0,68,194,
  0,194,4,0,64,255,0,218,0,4,0,68,0,4,64,0,
  68,194,4,195,64,4,194,68,0,194,64,4,194,0,4,0,
  68,194,0,194,4,0,64,255,0,212,0,68,197,0,4,0,
  4,194,0,64,0,68,4,0,195,64,4,194,68,4,194,64,
  0,64,0,4,0,68,4,64,194,4,0,64,68,64,255,0,
  210,0,4,64,196,0,4,0,4,68,0,64,0,64,4,0,
  68,64,68,4,195,68,0,64,0,68,0,68,0,68,0,64,
  194,4,195,64,255,0,212,0,64,0,68,194,0,195,4,64,
  4,64,4,64,68,64,4,64,4,68,195,68,194,64,194,4,
  0,64,0,68,0,68,4,0,64,68,64,0,4,255,0,210,
  0,64,0,4,64,0,64,194,4,64,4,68,4,64,194,68,
  4,64,194,68,68,67,196,68,4,194,64,4,68,0,195,4,
  64,68,194,0,68,194,0,64,255,0,207,0,68,194,0,194,
  64,68,194,4,64,4,68,4,64,197,68,67,52,51,199,68,
  4,195,68,194,4,64,68,194,0,64,0,4,64,255,0,205,
  0,195,4,194,0,68,64,68,194,4,195,68,4,198,68,67,
  51,52,67,52,203,68,64,68,194,0,68,0,4,64,255,0,
  205,0,4,68,4,194,0,68,64,204,68,51,68,51,194,68,
  194,52,202,68,64,68,64,0,4,0,4,194,64,194,0,4,
  255,0,202,0,68,4,64,0,68,64,203,68,194,52,68,52,
  67,68,51,52,203,68,4,64,0,4,64,4,194,64,194,0,
  4,255,0,202,0,4,194,64,0,68,4,203,68,51,52,67,
  52,67,68,51,195,68,67,52,201,68,194,0,64,4,0,64,
  4,0,68,255,0,201,0,194,4,194,68,4,201,68,51,195,
  68,67,52,67,194,51,52,67,52,68,51,67,52,67,200,68,
  194,0,64,4,0,64,194,4,64,255,0,200,0,64,4,0,
  68,194,4,199,68,67,68,51,67,52,68,51,68,51,52,51,
  52,67,52,67,52,68,194,52,199,68,52,64,4,64,4,0,
  64,194,4,255,0,201,0,64,4,0,68,4,201,68,194,52,
  68,51,68,51,68,51,52,195,51,52,67,194,68,51,68,52,
  67,195,68,52,67,52,68,52,194,68,194,64,194,4,64,255,
  0,197,0,68,4,0,64,195,68,52,195,68,52,194,68,67,
  68,52,67,52,68,67,68,195,51,197,51,67,51,68,51,68,
  52,67,52,194,68,194,51,52,68,51,194,68,194,64,194,4,
  64,211,0,3,48,239,0,68,4,0,64,194,68,67,52,194,
  68,67,52,194,68,51,68,52,67,52,67,51,67,195,51,197,
  51,67,51,68,194,67,194,68,51,67,68,51,52,195,68,194,
  52,68,64,0,68,64,0,195,64,202,0,48,194,0,195,3,
  48,219,0,48,207,0,195,64,0,68,64,0,194,68,194,52,
  195,68,52,68,194,67,52,68,194,67,68,67,51,67,195,51,
  197,51,67,51,195,67,68,51,67,51,68,67,195,68,67,68,
  52,68,64,0,68,194,64,68,64,68,200,0,3,51,48,194,
  0,3,194,51,220,0,48,206,0,4,64,68,194,64,68,64,
  0,194,68,52,67,195,68,67,68,67,51,67,52,196,67,51,
  67,195,51,201,51,67,52,194,67,194,52,67,194,52,68,51,
  194,52,194,68,64,196,68,52,68,194,0,48,3,48,196,0,
  51,196,0,51,219,0,194,3,48,202,0,51,0,48,0,4,
  68,52,195,68,64,195,68,52,51,52,68,52,51,68,52,51,
  67,68,51,67,199,51,201,51,67,52,68,67,68,194,51,194,
  52,67,51,194,52,68,64,4,64,195,68,51,52,68,0,194,
  51,197,0,194,3,48,194,0,194,3,216,0,48,0,3,51,
  203,0,3,51,48,4,68,51,52,194,68,67,196,68,52,194,
  51,68,52,194,51,52,67,194,68,51,67,199,51,204,51,67,
  68,67,51,194,52,196,51,194,68,4,64,195,68,67,52,195,
  68,51,3,196,0,194,51,48,194,3,194,51,3,199,0,3,
  48,206,0,51,0,3,203,0,194,3,52,195,68,51,195,68,
  67,195,68,67,195,51,194,52,194,51,68,194,67,202,51,209,
  51,52,196,51,67,68,4,64,68,64,194,68,52,68,67,68,
  194,51,0,194,48,0,194,51,0,3,194,51,48,3,198,0,
  194,3,204,0,48,194,0,3,48,3,195,0,3,0,3,51,
  48,0,194,48,3,51,52,67,194,68,52,68,67,68,67,194,
  68,194,67,195,51,52,208,51,213,51,52,67,52,68,48,68,
  4,68,67,51,52,67,68,194,51,68,48,51,0,3,48,3,
  194,48,195,3,48,197,0,3,48,201,0,48,194,0,51,195,
  0,48,51,194,3,0,48,51,0,51,0,3,48,52,67,51,
  52,67,68,194,51,194,68,52,67,52,68,51,68,212,51,216,
  51,67,52,51,0,51,67,194,51,67,68,195,51,52,51,0,
  194,48,195,51,195,3,48,194,3,194,0,48,0,51,201,0,
  51,194,0,3,0,194,3,48,51,195,3,194,51,195,48,3,
  52,195,51,52,194,67,194,51,67,194,51,52,51,67,214,51,
  219,51,52,51,67,51,67,51,52,51,67,194,52,51,68,48,
  196,51,195,3,194,51,3,0,194,48,0,51,48,3,197,0,
  3,0,51,194,0,51,48,194,3,194,51,195,3,195,51,48,
  52,67,194,52,51,67,52,194,51,67,51,67,52,218,51,218,
  51,52,51,67,195,51,52,195,51,194,52,51,68,202,51,3,
  0,51,48,195,51,3,197,0,194,3,194,51,48,51,48,194,
  3,201,51,52,67,194,52,195,51,52,196,51,67,52,217,51,
  242,51,3,194,51,48,195,51,194,3,48,194,0,51,194,3,
  194,51,48,195,51,3,240,51,255,51,249,51,255,51,249,51,
  255,51,249,51
}

RES_PCX_KELP1 =
{
  10,5,1,4,0,0,0,0,16,0,68,0,0,0,0,0,
  0,0,0,0,0,0,26,42,57,19,51,89,42,66,78,59,
  105,95,72,100,112,94,130,116,105,131,132,146,159,141,176,184,
  160,45,88,130,0,0,0,0,0,0,0,0,0,0,0,0,
  0,1,10,0,1,0,0,0,0,0,0,0,0,0,0,0,
  0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,
  0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,
  0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,
  197,0,176,196,0,195,0,48,3,176,196,0,195,0,59,11,
  197,0,194,0,194,48,176,197,0,0,3,176,59,187,176,196,
  0,0,3,176,187,51,197,0,194,0,194,48,198,0,194,0,
  59,48,198,0,194,0,59,0,179,197,0,3,0,11,0,179,
  197,0,3,176,59,11,48,197,0,0,59,194,179,198,0,0,
  51,48,199,0,0,3,176,3,176,197,0,0,3,176,11,48,
  197,0,3,48,176,179,198,0,0,194,59,176,198,0,0,59,
  179,48,198,0,0,3,176,199,0,194,0,176,0,48,197,0,
  51,0,176,0,51,197,0,59,0,48,0,179,197,0,51,194,
  176,11,51,197,0,3,187,48,3,48,197,0,0,3,194,51,
  198,0,194,0,51,48,198,0,194,0,3,199,0,3,48,3,
  194,0,176,196,0,3,59,0,48,3,179,196,0,0,59,0,
  48,11,51,196,0,0,3,51,48,3,51,196,0,194,0,3,
  194,51,197,0,195,0,3,48,197,0,195,0,3,198,0,194,
  0,51,3,198,0,194,0,51,176,48,3,48,195,0,194,0,
  3,194,48,11,48,195,0,194,0,3,51,48,179,196,0,196,
  0,194,51,196,0,194,0,3,48,3,197,0,194,0,3,176,
  3,197,0,194,0,3,179,3,0,51,195,0,194,0,3,59,
  3,0,179,195,0,195,0,51,3,11,179,195,0,195,0,3,
  51,3,48,195,0,196,0,3,51,196,0,195,0,51,3,197,
  0,195,0,59,3,0,3,48,194,0,195,0,59,3,48,3,
  48,194,0,195,0,3,51,48,11,48,194,0,196,0,51,48,
  179,48,194,0,196,0,3,194,51,48,194,0,195,0,3,0,
  51,48,195,0,195,0,51,0,48,196,0,195,0,51,194,48,
  196,0,195,0,51,187,48,0,51,194,0,195,0,3,59,176,
  3,179,194,0,196,0,51,48,11,48,194,0,197,0,48,51,
  48,194,0,197,0,194,51,195,0,197,0,51,0,3,48,0,
  195,0,51,48,3,0,59,48,0,195,0,3,187,194,3,51,
  194,0,196,0,195,51,195,0,197,0,3,48,195,0,198,0,
  48,195,0,198,0,48,195,0,198,0,48,195,0,198,0,51,
  195,0
}

RES_PCX_KELP2 =
{
  10,5,1,4,0,0,0,0,19,0,72,0,0,0,0,0,
  0,0,0,0,0,0,26,42,57,19,51,89,42,66,78,59,
  105,95,72,100,112,94,130,116,105,131,132,146,159,141,176,184,
  160,45,88,130,0,0,0,0,0,0,0,0,0,0,0,0,
  0,1,10,0,1,0,0,0,0,0,0,0,0,0,0,0,
  0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,
  0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,
  0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,
  199,0,3,48,0,197,0,51,0,11,48,0,197,0,51,187,
  59,48,0,198,0,59,187,194,0,198,0,11,179,194,0,199,
  0,176,0,3,199,0,176,0,51,197,0,48,0,179,11,51,
  197,0,51,0,187,11,0,197,0,3,194,59,51,0,198,0,
  59,179,48,0,199,0,59,48,0,199,0,59,48,0,199,0,
  11,48,0,196,0,3,48,0,3,48,0,196,0,3,179,0,
  3,194,0,197,0,59,0,3,0,3,197,0,51,0,194,11,
  51,197,0,3,176,11,179,51,197,0,3,194,59,51,0,198,
  0,194,51,194,0,198,0,59,48,194,0,196,0,48,0,187,
  195,0,196,0,51,0,179,195,0,196,0,3,0,48,3,48,
  0,196,0,3,194,48,11,48,0,196,0,3,194,176,179,48,
  0,196,0,3,179,187,179,194,0,196,0,3,51,179,195,0,
  197,0,51,48,195,0,197,0,3,48,195,0,197,0,3,196,
  0,194,0,3,179,0,48,196,0,194,0,3,59,0,48,196,
  0,194,0,3,51,0,48,196,0,195,0,3,51,48,196,0,
  196,0,51,197,0,196,0,3,197,0,196,0,194,3,196,0,
  194,0,51,0,48,179,48,195,0,194,0,59,0,48,51,196,
  0,194,0,3,176,194,51,196,0,194,0,3,51,48,197,0,
  195,0,3,0,51,196,0,195,0,3,0,179,196,0,0,3,
  48,194,3,179,196,0,0,3,176,3,11,51,196,0,0,3,
  187,194,3,48,196,0,194,0,51,3,51,197,0,194,0,3,
  51,198,0,195,0,194,3,48,196,0,0,51,0,3,11,48,
  196,0,0,51,0,51,11,48,196,0,0,59,0,194,51,197,
  0,0,51,176,51,48,197,0,0,195,51,198,0,194,0,51,
  48,3,197,0,195,0,48,3,48,196,0,195,0,48,51,48,
  196,0,3,48,0,59,179,48,196,0,3,179,0,187,51,197,
  0,0,59,0,51,48,197,0,0,51,194,48,198,0,0,3,
  51,48,198,0,51,0,3,48,198,0,59,48,3,0,51,48,
  196,0,3,51,3,11,179,197,0,0,3,194,51,48,197,0,
  194,0,51,199,0,194,0,48,199,0,194,0,48,199,0,194,
  0,48,199,0,0,3,48,199,0
}

RES_PCX_KELP3 =
{
  10,5,1,4,0,0,0,0,16,0,65,0,0,0,0,0,
  0,0,0,0,0,0,26,42,57,19,51,89,42,66,78,59,
  105,95,72,100,112,94,130,116,105,131,132,146,159,141,176,184,
  160,45,88,130,0,0,0,0,0,0,0,0,0,0,0,0,
  0,1,10,0,1,0,0,0,0,0,0,0,0,0,0,0,
  0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,
  0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,
  0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,
  198,0,48,195,0,198,0,179,195,0,198,0,179,195,0,198,
  0,48,195,0,197,0,3,48,195,0,195,0,3,176,3,48,
  195,0,195,0,3,176,11,0,3,194,0,196,0,59,11,48,
  179,194,0,196,0,3,179,187,48,194,0,198,0,51,48,194,
  0,196,0,179,0,179,195,0,196,0,59,0,179,195,0,196,
  0,3,194,176,51,194,0,197,0,187,59,48,194,0,197,0,
  51,187,48,194,0,198,0,179,195,0,196,0,48,0,176,195,
  0,195,0,3,48,0,176,3,48,0,195,0,3,176,0,48,
  11,48,0,195,0,3,59,0,176,179,48,0,196,0,51,0,
  59,179,194,0,196,0,3,194,51,195,0,197,0,51,48,195,
  0,197,0,3,196,0,195,0,176,0,3,0,51,194,0,194,
  0,3,179,0,48,11,51,194,0,194,0,3,59,0,48,11,
  48,194,0,194,0,3,51,0,194,51,195,0,195,0,3,194,
  51,196,0,196,0,51,197,0,196,0,3,197,0,196,0,194,
  3,48,195,0,194,0,51,0,48,179,48,195,0,194,0,59,
  0,48,51,196,0,194,0,3,176,194,51,196,0,194,0,3,
  51,48,197,0,195,0,3,0,51,196,0,195,0,3,0,179,
  196,0,0,3,48,194,3,179,196,0,0,3,176,3,11,51,
  196,0,0,3,187,194,3,48,196,0,194,0,51,3,51,197,
  0,194,0,3,51,198,0,195,0,194,3,48,196,0,0,51,
  0,3,11,48,196,0,0,51,0,51,11,48,196,0,0,59,
  0,194,51,197,0,0,51,176,51,48,197,0,0,195,51,198,
  0,194,0,51,48,3,197,0,195,0,48,3,48,196,0,195,
  0,48,51,48,196,0,3,48,0,59,179,48,196,0,3,179,
  0,187,51,197,0,0,59,0,51,48,197,0,0,51,194,48,
  198,0,0,3,51,48,198,0,51,0,3,48,198,0,59,48,
  3,0,51,48,196,0,3,51,3,11,179,197,0,0,3,194,
  51,48,197,0,194,0,51,199,0,194,0,48,199,0,194,0,
  48,199,0,194,0,48,199,0,0,3,48,199,0
}

RES_PCX_KELP_FG_LEFT_1 =
{
  10,5,1,4,0,0,0,0,16,0,88,0,0,0,0,0,
  0,0,0,0,0,0,19,51,89,72,100,112,45,88,130,68,
  132,159,105,131,132,124,197,214,146,159,141,176,184,160,210,212,
  172,221,231,179,206,241,208,255,255,255,0,0,0,0,0,0,
  0,1,10,0,1,0,0,0,0,0,0,0,0,0,0,0,
  0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,
  0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,
  0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,
  1,199,0,16,0,1,17,198,0,16,0,0,17,197,0,1,
  16,0,0,1,16,196,0,1,16,0,0,1,16,196,0,1,
  16,0,0,1,16,196,0,17,16,0,0,1,17,196,0,17,
  16,0,0,1,17,195,0,1,17,16,0,0,1,17,195,0,
  1,17,16,0,194,0,17,195,0,194,17,16,0,0,1,17,
  195,0,194,17,194,0,194,0,17,16,0,1,194,17,194,0,
  194,0,17,16,0,195,17,194,0,194,0,17,16,0,194,17,
  16,194,0,194,0,17,16,0,195,17,194,0,194,0,17,16,
  1,194,17,16,194,0,194,0,17,16,1,194,17,16,194,0,
  194,0,17,16,1,194,17,16,194,0,194,0,17,16,1,194,
  17,16,194,0,194,0,17,0,1,194,17,195,0,194,0,17,
  0,1,194,17,195,0,0,1,17,0,1,194,17,195,0,194,
  0,16,0,1,194,17,195,0,0,1,16,0,1,194,17,195,
  0,0,1,16,0,1,194,17,195,0,0,17,194,0,1,194,
  17,16,194,0,0,17,194,0,1,194,17,16,194,0,0,17,
  195,0,194,17,16,194,0,1,17,195,0,194,17,195,0,1,
  16,0,16,0,194,17,195,0,17,16,0,16,0,194,17,195,
  0,17,194,0,17,0,1,17,195,0,17,194,0,17,0,1,
  17,16,194,0,17,16,0,17,194,0,17,16,194,0,17,16,
  0,17,194,0,17,16,194,0,17,16,0,17,16,0,194,17,
  16,0,17,16,0,17,16,0,1,17,16,0,17,16,0,17,
  16,0,1,17,16,0,17,16,0,17,16,0,1,17,194,0,
  1,16,0,1,16,0,1,17,194,0,1,17,0,1,16,0,
  1,17,194,0,1,17,0,1,17,194,0,17,194,0,1,17,
  0,1,17,194,0,17,194,0,0,17,194,0,17,16,0,17,
  194,0,0,17,16,0,17,16,0,17,194,0,0,1,16,0,
  17,16,0,17,194,0,0,1,16,0,1,17,0,17,194,0,
  0,1,16,0,1,17,0,16,194,0,194,0,17,0,1,17,
  0,16,194,0,194,0,17,194,0,17,0,16,194,0,194,0,
  17,16,0,17,0,16,194,0,194,0,1,16,0,17,194,16,
  194,0,194,0,1,16,0,194,17,16,194,0,194,0,1,17,
  0,1,17,16,194,0,195,0,17,0,1,17,16,194,0,195,
  0,17,194,0,17,16,194,0,195,0,17,194,0,17,195,0,
  195,0,1,194,0,17,195,0,195,0,1,16,0,17,195,0,
  195,0,1,16,0,17,16,194,0,195,0,1,16,0,17,195,
  0,195,0,1,16,0,17,195,0,195,0,1,16,0,17,195,
  0,195,0,1,16,1,16,195,0,195,0,1,17,1,196,0,
  195,0,1,194,17,196,0,195,0,1,17,16,196,0,195,0,
  1,17,16,196,0,195,0,1,17,16,196,0,195,0,1,17,
  16,196,0,195,0,1,17,197,0,195,0,1,17,16,196,0,
  195,0,1,17,197,0,195,0,1,17,197,0,195,0,1,17,
  197,0,195,0,1,17,197,0,195,0,1,17,197,0,195,0,
  1,17,197,0,195,0,1,17,197,0,195,0,1,17,197,0,
  195,0,1,17,197,0,195,0,1,17,197,0,195,0,17,16,
  197,0,195,0,17,16,197,0,195,0,1,16,197,0,196,0,
  16,197,0,196,0,16,197,0,196,0,16,197,0,196,0,16,
  197,0
}

RES_PCX_KELP_FG_LEFT_2 =
{
  10,5,1,4,0,0,0,0,15,0,78,0,0,0,0,0,
  0,0,0,0,0,0,19,51,89,72,100,112,45,88,130,68,
  132,159,105,131,132,124,197,214,146,159,141,176,184,160,210,212,
  172,221,231,179,206,241,208,255,255,255,0,0,0,0,0,0,
  0,1,8,0,1,0,0,0,0,0,0,0,0,0,0,0,
  0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,
  0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,
  0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,
  198,0,16,0,198,0,16,0,198,0,17,0,198,0,17,0,
  198,0,17,0,198,0,17,16,198,0,17,16,198,0,17,16,
  197,0,1,17,16,197,0,1,17,16,197,0,1,194,17,197,
  0,1,194,17,197,0,1,194,17,197,0,195,17,197,0,195,
  17,197,0,195,17,196,0,1,195,17,196,0,1,194,17,16,
  196,0,195,17,16,196,0,195,17,16,196,0,195,17,0,196,
  0,195,17,0,196,0,194,17,16,0,196,0,194,17,16,0,
  195,0,1,194,17,194,0,195,0,1,194,17,194,0,195,0,
  1,194,17,194,0,195,0,1,17,16,194,0,195,0,1,17,
  16,194,0,196,0,17,16,194,0,196,0,17,16,194,0,196,
  0,17,195,0,196,0,17,195,0,196,0,17,195,0,196,0,
  17,195,0,196,0,17,195,0,196,0,17,195,0,196,0,17,
  195,0,196,0,1,195,0,196,0,1,16,194,0,196,0,1,
  16,194,0,196,0,1,16,194,0,196,0,1,17,194,0,196,
  0,1,17,194,0,196,0,1,17,194,0,196,0,1,17,194,
  0,196,0,1,17,194,0,196,0,1,17,194,0,196,0,1,
  17,194,0,196,0,1,16,194,0,196,0,17,16,194,0,196,
  0,17,16,194,0,196,0,17,195,0,196,0,17,195,0,196,
  0,17,195,0,195,0,1,17,195,0,195,0,1,16,195,0,
  195,0,17,16,195,0,195,0,17,16,195,0,195,0,17,16,
  195,0,194,0,1,17,196,0,194,0,1,17,196,0,194,0,
  17,16,196,0,0,1,17,16,196,0,0,1,17,16,196,0,
  0,194,17,16,196,0,0,194,17,16,196,0,1,194,17,16,
  196,0,195,17,197,0,195,17,197,0,194,17,16,197,0,194,
  17,16,197,0,194,17,16,197,0,1,17,16,197,0,1,17,
  16,197,0,1,17,16,197,0,1,17,16,197,0,1,17,198,
  0,0,1,198,0
}

RES_PCX_KELP_FG_LEFT_3 =
{
  10,5,1,4,0,0,0,0,17,0,62,0,0,0,0,0,
  0,0,0,0,0,0,19,51,89,72,100,112,45,88,130,68,
  132,159,105,131,132,124,197,214,146,159,141,176,184,160,210,212,
  172,221,231,179,206,241,208,255,255,255,0,0,0,0,0,0,
  0,1,10,0,1,0,0,0,0,0,0,0,0,0,0,0,
  0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,
  0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,
  0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,
  200,0,16,0,200,0,16,0,200,0,16,0,200,0,16,0,
  200,0,17,0,200,0,17,0,200,0,17,0,200,0,17,0,
  200,0,17,0,200,0,17,0,199,0,1,17,0,199,0,1,
  17,0,199,0,1,17,0,199,0,194,17,0,199,0,194,17,
  0,198,0,1,17,16,0,198,0,1,17,16,0,198,0,194,
  17,16,0,198,0,194,17,194,0,197,0,1,194,17,194,0,
  197,0,1,17,16,194,0,197,0,1,17,16,194,0,197,0,
  194,17,195,0,197,0,194,17,195,0,197,0,17,16,195,0,
  197,0,17,16,195,0,196,0,1,17,196,0,196,0,1,17,
  196,0,196,0,1,16,196,0,196,0,1,16,196,0,196,0,
  1,16,196,0,196,0,1,16,196,0,196,0,1,197,0,196,
  0,1,16,196,0,196,0,17,197,0,196,0,17,197,0,196,
  0,17,197,0,196,0,17,197,0,195,0,1,17,197,0,195,
  0,1,16,197,0,195,0,1,16,197,0,195,0,17,198,0,
  195,0,17,198,0,195,0,16,198,0,194,0,1,199,0,194,
  0,17,199,0,194,0,16,199,0,0,1,16,199,0,0,17,
  200,0,0,17,200,0,1,16,200,0,1,16,200,0,1,17,
  200,0,194,1,200,0,194,1,200,0,194,1,200,0,194,1,
  200,0,17,1,16,199,0,17,1,16,199,0,16,0,16,199,
  0,16,0,16,199,0,16,0,16,199,0,16,0,16,199,0
}

RES_PCX_KELP_FG_LEFT_4 =
{
  10,5,1,4,0,0,0,0,11,0,38,0,0,0,0,0,
  0,0,0,0,0,0,19,51,89,72,100,112,45,88,130,68,
  132,159,105,131,132,124,197,214,146,159,141,176,184,160,210,212,
  172,221,231,179,206,241,208,255,255,255,0,0,0,0,0,0,
  0,1,6,0,1,0,0,0,0,0,0,0,0,0,0,0,
  0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,
  0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,
  0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,
  196,0,1,0,196,0,1,16,196,0,1,16,196,0,1,16,
  196,0,1,16,196,0,1,16,196,0,1,17,196,0,1,17,
  196,0,194,17,196,0,194,17,195,0,1,194,17,195,0,1,
  194,17,195,0,195,17,195,0,194,17,16,195,0,194,17,16,
  194,0,1,194,17,16,194,0,1,194,17,0,194,0,1,17,
  16,0,194,0,194,17,16,0,194,0,194,17,194,0,0,1,
  17,16,194,0,0,1,17,195,0,0,194,17,195,0,0,17,
  16,195,0,0,17,196,0,1,17,196,0,1,16,196,0,1,
  16,196,0,17,197,0,17,197,0,17,197,0,17,197,0,17,
  197,0,17,197,0,16,197,0,16,197,0,16,197,0,16,197,
  0,16,197,0
}

RES_PCX_KELP_FG_RIGHT_1 =
{
  10,5,1,4,0,0,0,0,9,0,32,0,0,0,0,0,
  0,0,0,0,0,0,19,51,89,72,100,112,45,88,130,68,
  132,159,105,131,132,124,197,214,146,159,141,176,184,160,210,212,
  172,221,231,179,206,241,208,255,255,255,0,0,0,0,0,0,
  0,1,6,0,1,0,0,0,0,0,0,0,0,0,0,0,
  0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,
  0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,
  0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,
  1,197,0,1,197,0,1,197,0,17,197,0,17,197,0,17,
  197,0,17,16,196,0,17,16,196,0,194,17,196,0,194,17,
  196,0,194,17,196,0,194,17,16,195,0,1,17,16,195,0,
  1,194,17,195,0,0,194,17,195,0,0,194,17,16,194,0,
  0,1,17,16,194,0,0,1,17,16,194,0,194,0,194,17,
  194,0,194,0,194,17,194,0,194,0,1,17,194,0,195,0,
  17,194,0,195,0,1,194,0,195,0,1,16,0,195,0,1,
  16,0,196,0,17,0,196,0,17,0,196,0,17,0,196,0,
  17,0,196,0,17,0,196,0,17,0,196,0,17,0,196,0,
  16,0
}

RES_PCX_KELP_FG_RIGHT_2 =
{
  10,5,1,4,0,0,0,0,13,0,51,0,0,0,0,0,
  0,0,0,0,0,0,19,51,89,72,100,112,45,88,130,68,
  132,159,105,131,132,124,197,214,146,159,141,176,184,160,210,212,
  172,221,231,179,206,241,208,255,255,255,0,0,0,0,0,0,
  0,1,8,0,1,0,0,0,0,0,0,0,0,0,0,0,
  0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,
  0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,
  0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,
  16,199,0,16,199,0,17,199,0,17,199,0,16,199,0,17,
  199,0,17,199,0,17,199,0,17,16,198,0,194,17,198,0,
  194,17,198,0,194,17,16,197,0,194,17,16,197,0,1,194,
  17,197,0,1,194,17,197,0,1,194,17,197,0,0,194,17,
  16,196,0,0,194,17,16,196,0,0,1,17,16,196,0,0,
  1,17,16,196,0,194,0,17,16,196,0,194,0,1,16,196,
  0,194,0,1,16,196,0,194,0,1,16,196,0,195,0,17,
  196,0,195,0,17,196,0,195,0,1,196,0,195,0,1,196,
  0,195,0,1,16,195,0,195,0,1,16,195,0,195,0,1,
  16,195,0,195,0,1,16,195,0,195,0,1,16,195,0,196,
  0,16,195,0,196,0,17,195,0,196,0,17,195,0,196,0,
  17,195,0,196,0,17,195,0,196,0,17,195,0,196,0,17,
  16,194,0,196,0,1,16,194,0,196,0,1,16,194,0,196,
  0,1,16,194,0,197,0,17,194,0,197,0,17,194,0,197,
  0,1,194,0,197,0,1,16,0,198,0,16,0,198,0,17,
  0,198,0,1,0,198,0,1,0,198,0,1,0
}

RES_PCX_KELP_FG_RIGHT_3 =
{
  10,5,1,4,0,0,0,0,12,0,75,0,0,0,0,0,
  0,0,0,0,0,0,19,51,89,72,100,112,45,88,130,68,
  132,159,105,131,132,124,197,214,146,159,141,176,184,160,210,212,
  172,221,231,179,206,241,208,255,255,255,0,0,0,0,0,0,
  0,1,8,0,1,0,0,0,0,0,0,0,0,0,0,0,
  0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,
  0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,
  0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,
  194,0,16,197,0,0,1,16,197,0,0,1,16,197,0,0,
  17,16,197,0,0,17,198,0,1,17,198,0,1,17,198,0,
  1,17,198,0,1,17,198,0,194,17,16,197,0,1,17,16,
  197,0,194,17,16,197,0,194,17,16,197,0,195,17,197,0,
  195,17,197,0,1,194,17,197,0,1,194,17,16,196,0,0,
  194,17,16,196,0,0,195,17,196,0,0,1,194,17,196,0,
  0,1,194,17,196,0,194,0,194,17,16,195,0,194,0,194,
  17,16,195,0,194,0,194,17,16,195,0,194,0,1,17,16,
  195,0,194,0,1,17,16,195,0,195,0,194,17,195,0,195,
  0,194,17,195,0,195,0,194,17,195,0,195,0,1,17,195,
  0,195,0,1,16,195,0,195,0,1,16,195,0,195,0,1,
  16,195,0,195,0,17,196,0,195,0,17,196,0,195,0,17,
  196,0,195,0,17,196,0,195,0,17,196,0,195,0,17,196,
  0,195,0,16,196,0,195,0,16,196,0,195,0,16,196,0,
  195,0,16,196,0,195,0,16,196,0,195,0,16,196,0,195,
  0,16,196,0,195,0,16,196,0,195,0,16,196,0,195,0,
  16,196,0,195,0,1,196,0,195,0,1,196,0,195,0,1,
  196,0,196,0,16,195,0,196,0,16,195,0,196,0,16,195,
  0,196,0,1,195,0,196,0,1,195,0,196,0,1,195,0,
  197,0,16,194,0,197,0,16,194,0,197,0,16,194,0,197,
  0,16,194,0,197,0,1,194,0,197,0,1,194,0,197,0,
  1,194,0,197,0,1,194,0,197,0,1,16,0,197,0,1,
  16,0,197,0,1,16,0,197,0,1,194,0,197,0,17,194,
  0,197,0,16,194,0,196,0,1,16,194,0,196,0,1,16,
  194,0,197,0,16,194,0,197,0,16,194,0
}

RES_PCX_KELP_FG_RIGHT_4 =
{
  10,5,1,4,0,0,0,0,11,0,92,0,0,0,0,0,
  0,0,0,0,0,0,19,51,89,72,100,112,45,88,130,68,
  132,159,105,131,132,124,197,214,146,159,141,176,184,160,210,212,
  172,221,231,179,206,241,208,255,255,255,0,0,0,0,0,0,
  0,1,6,0,1,0,0,0,0,0,0,0,0,0,0,0,
  0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,
  0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,
  0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,
  0,1,16,195,0,0,17,196,0,0,17,196,0,1,17,196,
  0,1,17,196,0,1,16,196,0,17,16,196,0,17,16,196,
  0,17,16,196,0,194,17,196,0,194,17,196,0,194,17,196,
  0,194,17,196,0,194,17,16,195,0,194,17,16,195,0,1,
  17,16,195,0,1,194,17,195,0,1,194,17,195,0,0,194,
  17,195,0,0,194,17,16,194,0,0,194,17,16,194,0,0,
  1,17,16,194,0,0,1,194,17,194,0,194,0,194,17,194,
  0,194,0,194,17,194,0,194,0,194,17,194,0,194,0,1,
  17,194,0,194,0,1,17,194,0,194,0,1,17,16,0,194,
  0,1,17,16,0,195,0,17,16,0,194,0,1,17,16,0,
  194,0,1,17,16,0,194,0,1,17,16,0,194,0,1,17,
  16,0,194,0,1,17,16,0,194,0,1,17,16,0,194,0,
  1,17,194,0,194,0,1,17,0,1,194,0,1,16,0,17,
  194,0,17,16,0,17,194,0,17,16,0,17,194,0,17,194,
  0,17,194,0,17,194,0,17,194,0,16,194,0,17,194,0,
  16,0,1,17,0,1,16,0,1,17,0,1,16,0,194,17,
  0,1,16,0,17,16,194,0,16,0,17,16,194,0,16,1,
  17,16,194,0,16,1,17,0,194,0,16,1,17,0,194,0,
  16,1,16,0,194,0,16,1,16,0,194,0,16,1,194,0,
  194,0,16,1,194,0,194,0,16,17,194,0,194,0,1,16,
  194,0,194,0,1,16,194,0,194,0,1,16,194,0,194,0,
  1,16,194,0,194,0,1,16,194,0,194,0,1,16,194,0,
  195,0,16,194,0,195,0,16,194,0,195,0,16,194,0,195,
  0,16,194,0,195,0,16,194,0,195,0,1,194,0,195,0,
  1,194,0,195,0,1,194,0,195,0,1,194,0,195,0,1,
  194,0,195,0,1,194,0,195,0,1,194,0,195,0,1,194,
  0,195,0,1,194,0,195,0,1,194,0,195,0,1,194,0,
  195,0,1,194,0,195,0,1,194,0,195,0,1,194,0,195,
  0,1,194,0,195,0,1,194,0,195,0,1,194,0,195,0,
  17,194,0,195,0,17,194,0,195,0,17,194,0,195,0,17,
  16,0,195,0,1,16,0,195,0,1,16,0,196,0,16,0
}

RES_PCX_KELP_FG_RIGHT_5 =
{
  10,5,1,4,0,0,0,0,10,0,76,0,0,0,0,0,
  0,0,0,0,0,0,19,51,89,72,100,112,45,88,130,68,
  132,159,105,131,132,124,197,214,146,159,141,176,184,160,210,212,
  172,221,231,179,206,241,208,255,255,255,0,0,0,0,0,0,
  0,1,6,0,1,0,0,0,0,0,0,0,0,0,0,0,
  0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,
  0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,
  0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,
  195,0,16,194,0,194,0,1,16,194,0,194,0,17,195,0,
  194,0,17,195,0,194,0,17,195,0,0,1,16,195,0,0,
  1,16,195,0,0,17,196,0,0,17,196,0,0,17,196,0,
  0,17,196,0,0,1,16,195,0,0,1,16,195,0,194,0,
  17,195,0,194,0,17,195,0,194,0,17,195,0,194,0,1,
  16,194,0,194,0,1,16,194,0,194,0,1,17,194,0,194,
  0,1,17,194,0,195,0,17,16,0,195,0,17,16,0,195,
  0,194,17,0,195,0,194,17,0,195,0,1,17,0,195,0,
  194,17,0,195,0,1,17,16,195,0,1,17,16,195,0,1,
  17,16,195,0,1,17,16,195,0,1,17,16,195,0,194,17,
  16,195,0,194,17,0,195,0,194,17,0,195,0,194,17,0,
  195,0,194,17,0,194,0,1,17,16,0,194,0,1,17,16,
  0,194,0,1,17,16,0,194,0,1,17,194,0,194,0,194,
  17,194,0,194,0,194,17,194,0,194,0,17,16,194,0,194,
  0,17,16,194,0,194,0,17,195,0,194,0,17,195,0,194,
  0,17,195,0,0,1,16,195,0,0,1,16,195,0,0,1,
  16,195,0,0,1,196,0,0,1,196,0,0,1,196,0,0,
  1,196,0,0,17,196,0,0,17,196,0,0,17,196,0,0,
  17,196,0,0,17,196,0,0,17,196,0,0,16,196,0,1,
  16,196,0,1,16,196,0,1,16,196,0,1,16,196,0,17,
  16,196,0,17,16,196,0,17,197,0,17,197,0,17,197,0,
  17,197,0,17,197,0,17,197,0,17,197,0,17,197,0,17,
  197,0,17,197,0
}

RES_PCX_KELP_FG_RIGHT_6 =
{
  10,5,1,4,0,0,0,0,6,0,35,0,0,0,0,0,
  0,0,0,0,0,0,19,51,89,72,100,112,45,88,130,68,
  132,159,105,131,132,124,197,214,146,159,141,176,184,160,210,212,
  172,221,231,179,206,241,208,255,255,255,0,0,0,0,0,0,
  0,1,4,0,1,0,0,0,0,0,0,0,0,0,0,0,
  0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,
  0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,
  0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,
  194,0,1,0,194,0,1,16,194,0,1,16,194,0,17,16,
  194,0,17,16,0,1,17,0,0,1,17,0,0,1,17,0,
  0,194,17,0,0,194,17,0,1,194,17,0,1,194,17,0,
  1,194,17,0,1,17,16,0,1,17,16,0,194,17,16,0,
  194,17,194,0,194,17,194,0,194,17,194,0,194,17,194,0,
  1,16,194,0,1,17,194,0,1,16,194,0,17,16,194,0,
  17,16,194,0,17,16,194,0,17,16,194,0,17,195,0,1,
  195,0,1,195,0,1,16,194,0,17,195,0,17,16,194,0,
  17,16,194,0,1,16,194,0,0,16,194,0
}

RES_PCX_SIDE_ROCKS_LEFT =
{
  10,5,1,4,0,0,0,0,46,0,32,0,0,0,0,0,
  0,0,0,0,0,0,50,25,63,38,34,90,64,50,82,77,
  87,95,92,84,114,112,112,116,123,114,132,161,141,134,192,173,
  148,61,69,128,68,132,159,124,197,214,189,226,234,255,255,255,
  0,1,24,0,1,0,0,0,0,0,0,0,0,0,0,0,
  0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,
  0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,
  0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,
  195,0,7,119,211,0,194,0,5,194,85,112,210,0,0,34,
  37,68,69,85,119,149,208,0,36,195,68,69,71,85,122,80,
  194,0,39,119,154,80,201,0,68,36,85,151,194,68,69,87,
  160,0,4,69,85,121,165,201,0,194,68,84,69,151,69,68,
  85,121,0,36,194,85,87,122,201,0,36,66,36,68,85,84,
  36,68,87,121,68,195,85,121,144,200,0,194,68,34,36,68,
  84,66,84,69,119,195,68,85,87,144,200,0,195,68,34,195,
  68,69,68,66,68,66,68,69,85,119,0,119,154,80,196,0,
  36,68,66,36,194,68,66,194,34,68,36,195,68,85,119,34,
  39,85,165,196,0,69,66,34,85,87,34,36,69,114,34,68,
  36,34,69,84,66,36,194,85,87,165,195,0,68,66,37,68,
  85,87,194,68,85,34,36,195,34,36,68,69,85,69,85,119,
  165,194,0,68,34,36,194,68,69,146,194,68,114,66,194,68,
  66,195,68,69,84,68,85,87,165,0,68,34,36,195,68,73,
  36,197,68,66,198,68,87,117,119,80,68,69,84,34,195,68,
  66,196,68,119,194,121,165,195,68,194,69,87,85,112,194,68,
  69,114,34,36,68,71,36,66,34,119,117,85,117,122,82,36,
  195,68,85,117,112,195,34,71,36,194,34,68,36,66,34,196,
  85,87,165,34,36,68,36,69,85,112,34,36,66,68,114,194,
  66,68,66,68,66,36,195,68,85,119,68,66,36,194,68,69,
  80,195,34,36,69,194,34,36,66,194,68,37,34,36,68,69,
  87,66,195,34,36,85,0,196,34,66,194,34,36,66,36,68,
  34,87,114,36,68,85,119,34,196,35,0,194,50,194,34,36,
  50,199,34,39,37,68,69,87,195,50,35,48,0,196,34,197,
  35,199,34,69,34,196,51,194,0,196,34,206,50,48,0,51,
  48,194,0,210,35,198,0,195,50,51,194,50,203,51,48,198,
  0,204,51,196,56,48,199,0,201,51,195,99,196,136,200,0,
  198,51,196,54,102,195,0,8,136,200,0,197,51,196,99,207,
  0,199,54,194,102,207,0,195,99,96,212,0,194,54,214,0,
  96,215,0
}

RES_PCX_SIDE_ROCKS_RIGHT =
{
  10,5,1,4,0,0,0,0,53,0,32,0,0,0,0,0,
  0,0,0,0,0,0,26,42,57,19,51,89,42,66,78,59,
  105,95,72,100,112,94,130,116,105,131,132,146,159,141,176,184,
  160,45,88,130,68,132,159,124,197,214,189,226,234,255,255,255,
  0,1,28,0,1,0,0,0,0,0,0,0,0,0,0,0,
  0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,
  0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,
  0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,
  206,0,117,68,32,203,0,205,0,9,194,84,169,117,85,80,
  200,0,205,0,167,68,74,117,194,69,85,200,0,204,0,9,
  116,66,167,85,82,66,84,80,199,0,204,0,114,36,41,117,
  85,68,195,36,34,198,0,202,0,10,117,116,34,71,85,84,
  194,68,195,66,32,197,0,202,0,167,85,69,68,34,197,68,
  36,41,117,197,0,198,0,151,87,0,9,117,194,84,34,66,
  36,68,90,151,194,85,87,85,196,84,0,197,0,10,116,85,
  64,117,85,194,68,194,36,66,90,151,117,195,85,68,196,69,
  0,197,0,167,85,84,69,85,68,69,195,66,69,169,85,195,
  84,195,34,68,66,34,0,196,0,9,117,85,194,68,36,69,
  194,68,36,34,74,117,85,195,68,36,197,34,0,196,0,7,
  85,84,69,66,34,68,69,66,194,34,71,117,84,194,69,194,
  66,36,68,34,194,66,0,194,0,9,117,69,68,69,68,36,
  196,34,68,36,71,85,68,84,68,36,34,66,36,66,36,34,
  0,194,0,167,85,194,84,68,194,66,34,36,66,195,34,36,
  69,82,68,82,194,66,34,68,195,66,0,0,10,117,69,36,
  85,194,36,34,89,117,196,68,36,66,34,194,36,194,34,194,
  68,71,82,34,0,0,167,85,84,66,69,82,34,37,167,194,
  85,169,121,119,84,194,68,194,66,69,169,119,117,85,84,66,
  0,0,149,85,194,68,36,34,66,122,117,85,194,69,194,85,
  84,36,34,36,87,122,151,194,85,69,84,36,0,7,194,84,
  196,66,36,149,85,195,84,68,69,82,66,194,34,85,116,195,
  84,82,34,66,0,117,69,68,196,36,39,85,84,196,68,36,
  194,34,117,82,36,196,68,66,34,36,0,194,84,68,66,68,
  194,34,37,84,196,68,194,66,34,39,84,68,34,197,36,34,
  36,0,66,198,34,37,195,68,196,36,34,37,68,36,194,34,
  196,66,36,34,0,2,198,50,197,34,194,66,195,34,195,66,
  34,195,36,68,34,66,0,0,202,35,34,194,35,202,34,68,
  34,66,0,199,0,2,195,51,199,50,194,2,196,50,52,68,
  50,0,202,0,197,56,51,48,0,197,32,196,35,0,207,0,
  196,131,195,51,194,0,2,194,50,0,209,0,8,195,54,197,
  51,35,0,210,0,3,196,99,195,51,50,0,212,0,197,54,
  194,51,0,213,0,6,102,196,99,0,214,0,6,195,102,54,
  0,216,0,6,194,102,0,218,0,102,0
}

RES_PCX_THE =
{
  10,5,1,4,0,0,0,0,22,0,7,0,0,0,0,0,
  0,0,0,0,0,0,26,42,57,19,51,89,42,66,78,59,
  105,95,72,100,112,94,130,116,105,131,132,146,159,141,176,184,
  160,45,88,130,68,132,159,124,197,214,189,226,234,255,255,255,
  0,1,12,0,1,0,0,0,0,0,0,0,0,0,0,0,
  0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,
  0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,
  0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,
  193,224,195,0,193,224,199,0,193,224,195,0,193,224,199,0,
  193,238,193,224,194,0,194,238,193,237,0,13,194,238,0,193,
  224,195,0,193,224,0,60,193,208,193,220,48,14,0,193,224,
  195,0,193,224,0,3,193,224,193,227,0,14,0,193,227,195,
  0,193,224,194,0,194,224,3,193,224,0,193,220,48,194,0,
  193,224,194,0,193,224,193,208,61,194,0,13,194,238,194,224,
  194,0,193,224,13,194,238,193,224
}

RES_PCX_END =
{
  10,5,1,4,0,0,0,0,87,0,19,0,0,0,0,0,
  0,0,0,0,0,0,26,42,57,19,51,89,42,66,78,59,
  105,95,72,100,112,94,130,116,105,131,132,146,159,141,176,184,
  160,45,88,130,68,132,159,124,197,214,189,226,234,255,255,255,
  0,1,44,0,1,0,0,0,0,0,0,0,0,0,0,0,
  0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,
  0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,
  0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,
  0,190,204,255,0,203,255,193,254,193,219,194,0,204,255,193,
  235,0,12,193,239,204,255,0,204,255,193,254,193,192,0,204,
  255,193,254,193,192,190,205,255,0,205,255,193,235,0,205,255,
  193,235,193,223,205,255,0,205,255,193,253,0,205,255,193,253,
  193,239,193,255,176,215,0,190,193,255,193,254,204,0,189,193,
  255,193,254,194,255,205,0,194,187,201,0,11,194,255,0,194,
  187,201,0,12,194,255,194,255,205,0,194,221,202,0,194,255,
  0,194,221,202,0,194,255,194,238,205,0,194,238,202,0,194,
  238,0,194,238,202,0,194,238,194,255,190,201,255,195,0,194,
  255,202,0,194,255,0,194,255,202,0,194,255,194,238,189,201,
  238,195,0,194,238,202,0,194,238,0,194,238,202,0,194,238,
  194,238,189,201,238,195,0,194,238,202,0,194,238,0,194,238,
  202,0,194,238,194,255,190,201,255,195,0,194,255,202,0,194,
  255,0,194,255,202,0,194,255,194,238,205,0,194,238,202,0,
  194,238,0,194,238,202,0,194,238,194,238,205,0,194,238,202,
  0,194,238,0,194,238,202,0,194,238,194,238,176,204,0,194,
  238,202,0,194,238,0,194,238,176,200,0,11,194,238,193,222,
  193,238,193,203,204,0,194,238,202,0,194,238,0,194,238,193,
  203,200,0,188,194,238,193,206,208,238,202,0,194,238,0,205,
  238,193,237,189,208,238,202,0,194,238,0,205,238,193,236,12,
  193,222,207,238,202,0,194,238,0,205,238,193,208,0,193,205,
  207,238,202,0,194,238,0,204,238,193,237,176
}
RES_HMAP_WIDTH=128
RES_HMAP_HEIGHT=128
RES_HMAP={
115,110,84,58,33,11,6,17,24,15,24,26,20,34,45,45,39,41,46,39,37,28,15,14,20,37,62,80,90,99,83,93,99,86,68,63,67,77,60,38,30,19,27,15,27,45,77,80,34,27,26,38,33,19,22,25,28,33,9,7,24,36,68,107,148,161,127,102,70,43,32,16,13,7,9,28,25,15,35,41,77,137,175,167,141,121,99,76,68,86,90,76,84,104,115,116,134,146,151,118,67,30,13,8,19,41,54,76,47,44,20,15,36,34,41,46,63,78,54,27,24,30,24,20,43,68,76,110,
96,121,140,98,63,50,39,47,34,19,25,19,11,19,20,20,25,29,20,11,20,17,7,14,29,45,74,110,127,134,121,129,126,112,104,90,84,80,44,22,23,20,18,13,14,29,59,84,45,35,16,22,19,3,8,11,11,16,7,15,36,56,96,143,175,151,115,80,33,15,12,0,1,5,11,33,38,32,41,52,98,129,135,149,137,134,126,96,86,108,121,110,107,122,129,127,126,132,158,153,118,71,41,34,40,58,68,81,54,37,16,13,26,38,54,46,59,60,34,36,40,33,15,8,28,76,96,112,
58,80,145,119,86,84,66,54,25,16,24,19,15,5,0,0,1,14,14,7,23,21,17,41,56,70,92,129,171,171,153,162,151,110,80,76,84,76,58,45,41,40,26,5,0,8,38,87,93,59,20,17,24,6,10,13,20,22,15,25,46,89,135,149,132,84,62,51,14,5,3,0,0,0,8,40,50,50,70,99,124,118,90,93,108,108,115,104,105,127,143,140,134,121,118,122,118,116,135,153,149,116,73,67,73,89,110,87,47,22,13,13,18,51,83,77,80,59,41,35,22,26,19,11,50,101,96,90,
52,66,107,115,101,71,33,30,9,7,14,11,16,5,2,5,11,20,18,10,17,13,20,43,52,95,112,138,171,138,112,129,116,59,32,37,64,76,67,49,39,34,32,24,2,4,36,93,129,92,58,41,44,36,33,43,55,30,27,35,45,96,129,119,93,47,34,37,21,11,6,0,2,13,36,46,30,56,90,87,80,107,87,60,74,83,77,80,101,140,146,130,115,105,110,113,105,104,107,101,118,130,115,107,115,130,119,74,43,16,7,12,5,38,93,92,93,66,45,34,30,49,54,56,96,64,62,73,
46,60,74,92,96,62,26,26,11,11,6,3,12,5,17,41,34,33,30,19,16,11,14,16,28,87,126,127,132,104,89,110,89,36,13,15,49,74,45,26,24,14,4,23,39,27,38,89,146,143,124,105,84,67,66,80,59,25,35,63,63,95,96,89,62,35,23,11,11,15,17,15,23,30,36,22,25,68,116,110,110,112,84,51,54,68,52,52,76,115,107,80,64,70,90,96,78,84,80,62,83,110,130,141,146,138,104,77,66,40,27,25,8,39,87,96,92,73,63,55,46,76,118,121,80,40,55,56,
51,63,62,78,107,87,51,38,30,20,6,6,21,15,26,36,39,60,66,51,23,9,10,19,41,95,122,112,112,107,78,77,67,32,16,17,41,63,43,23,24,27,7,8,44,77,89,127,177,180,154,141,115,76,71,108,86,49,58,84,81,92,89,80,54,45,19,2,0,1,12,19,14,21,36,36,59,99,135,138,141,118,73,46,56,60,39,43,54,76,60,36,25,33,68,95,77,86,86,62,70,89,110,135,141,127,99,92,96,86,83,68,46,77,98,113,92,67,70,55,43,62,87,95,84,67,60,50,
24,56,68,95,140,130,101,73,49,34,25,27,28,8,4,9,18,34,50,73,50,27,35,56,87,98,93,92,101,92,45,49,63,43,33,45,50,37,27,21,13,13,34,32,41,78,119,148,161,162,135,115,104,89,76,84,87,68,56,70,98,112,121,104,81,45,2,0,0,0,6,26,21,27,46,62,77,119,167,169,161,130,92,66,74,52,33,40,51,60,46,36,35,51,76,86,80,95,113,101,96,104,119,119,112,121,107,112,112,102,118,124,113,98,96,102,64,50,76,76,71,55,41,46,41,36,38,38,
27,51,78,112,135,138,127,115,95,80,67,44,8,0,0,0,1,6,7,32,58,68,67,68,98,95,87,96,110,102,64,59,76,56,33,40,34,19,2,5,14,5,22,45,45,58,81,101,113,141,138,112,86,83,87,74,74,87,90,93,121,145,138,108,74,19,0,0,0,0,14,52,54,40,47,50,55,92,164,190,167,124,102,93,84,56,46,55,64,73,64,59,62,78,68,45,60,84,102,122,141,141,126,96,95,118,113,121,107,81,77,84,78,52,60,73,39,37,80,99,62,32,24,21,18,22,36,40,
40,54,99,118,130,153,148,132,118,105,78,33,0,0,0,0,2,0,0,1,28,56,44,55,95,89,76,93,119,135,105,83,73,80,77,63,32,4,0,0,0,2,18,41,44,44,47,66,77,102,140,124,66,49,63,78,73,86,110,116,137,156,146,112,73,30,7,4,8,20,34,40,43,46,56,62,77,104,151,169,118,58,39,52,58,51,58,74,64,49,49,52,50,51,62,39,41,62,83,118,135,149,134,104,104,104,95,96,56,24,17,29,43,24,29,50,20,17,54,92,60,25,22,28,38,50,58,56,
50,73,129,158,166,169,129,90,71,78,87,46,7,1,0,0,4,0,0,0,3,41,50,64,93,84,55,41,76,137,112,93,68,73,107,84,19,0,0,0,0,0,6,28,40,36,40,44,46,84,141,113,64,44,41,63,92,80,81,105,129,158,153,121,84,47,24,27,54,49,26,15,13,11,15,33,66,110,141,119,80,47,24,29,45,43,43,63,67,43,36,44,38,37,70,74,66,70,95,134,130,149,158,132,113,92,74,63,17,1,0,1,24,15,16,34,15,13,40,66,73,68,44,37,51,44,44,67,
90,122,148,162,167,138,87,62,67,86,107,99,50,14,0,0,0,0,0,0,6,45,34,44,87,87,44,26,56,124,115,90,77,70,92,68,20,0,0,0,0,0,0,16,27,28,38,54,80,122,126,92,70,60,44,45,87,110,90,101,138,174,151,121,102,87,67,67,50,13,7,9,2,0,0,15,55,102,132,86,40,41,45,45,45,37,30,44,60,45,47,64,73,78,101,129,132,129,140,156,148,162,184,166,129,102,86,55,7,0,0,0,2,0,6,23,36,28,34,43,59,81,58,54,60,44,43,70,
89,126,122,108,134,148,104,93,96,84,92,124,107,38,9,11,1,0,0,1,29,34,16,41,87,102,70,43,58,132,141,89,87,98,93,60,32,3,0,0,0,0,0,15,45,70,73,81,112,110,89,86,78,58,64,60,78,127,124,124,166,185,148,126,112,90,49,30,6,0,2,1,5,1,4,38,80,104,119,93,49,36,34,19,5,6,16,38,46,27,37,52,58,101,116,146,184,187,179,179,166,179,197,193,159,107,95,55,6,1,3,9,0,0,1,13,40,24,20,33,50,86,95,89,93,87,71,71,
95,121,110,81,86,116,104,89,51,34,60,93,121,81,46,41,12,0,0,7,46,45,47,66,81,119,115,87,89,149,162,99,83,101,77,47,11,0,0,0,0,1,7,25,54,76,86,102,119,112,101,108,104,63,62,84,101,121,158,180,187,169,126,84,67,58,33,11,0,0,2,0,10,36,49,87,105,116,101,78,55,27,9,0,0,0,3,24,34,27,44,50,47,90,102,137,182,175,175,166,154,161,180,179,149,121,110,60,22,12,10,22,0,0,0,3,33,41,33,40,60,86,99,122,145,135,122,105,
126,127,107,107,78,74,74,54,29,29,37,51,99,96,80,55,19,10,1,8,55,78,60,36,51,118,159,149,153,189,172,110,90,98,89,52,6,0,0,0,4,8,19,38,60,90,113,132,149,154,154,146,146,121,93,101,110,132,171,187,167,151,105,51,43,50,37,9,0,3,8,8,34,83,98,98,105,116,89,59,24,7,3,0,0,0,11,19,30,30,43,47,60,98,110,154,149,124,137,129,124,137,140,145,135,124,118,80,44,12,15,32,2,3,4,7,46,74,50,51,78,71,63,86,119,126,118,132,
145,148,115,107,95,74,47,29,27,25,24,52,81,68,96,68,17,20,19,19,41,68,49,20,34,95,140,151,179,212,182,140,134,130,107,49,15,1,0,8,15,29,50,71,76,76,107,146,179,200,213,202,192,180,153,122,102,116,130,138,151,148,115,70,39,23,35,24,9,22,29,46,90,93,84,101,105,81,58,55,19,2,4,0,0,14,15,16,25,29,27,28,40,76,112,129,80,68,80,83,118,112,98,112,134,137,122,93,55,29,51,52,15,26,32,25,51,73,64,70,71,52,44,37,47,80,101,119,
164,171,140,115,105,87,47,25,26,44,62,60,45,43,86,83,36,32,40,30,24,40,47,21,34,84,107,119,158,210,205,180,158,129,73,24,9,13,14,22,26,64,110,102,87,60,70,124,177,193,179,159,156,166,154,118,89,83,98,108,124,146,124,68,19,6,24,30,46,67,62,99,135,118,98,113,141,96,70,66,26,7,11,4,10,37,47,54,52,39,22,26,36,54,99,83,37,30,41,50,93,89,76,86,113,156,129,98,90,83,102,81,49,47,37,27,40,41,64,86,70,40,20,16,9,33,104,138,
127,124,126,130,118,104,92,64,66,78,41,18,21,38,73,77,52,41,25,11,9,14,18,21,45,71,70,98,137,171,180,180,151,104,59,27,8,20,59,46,45,83,113,93,77,87,96,137,161,124,84,67,80,110,112,96,89,83,95,101,96,108,134,99,39,13,27,47,78,110,112,132,134,138,149,158,172,134,104,70,35,19,20,15,29,60,49,45,67,62,28,32,47,49,73,52,17,5,10,24,41,71,84,89,113,158,141,126,130,134,135,73,32,27,20,21,34,26,39,74,67,15,0,6,3,19,87,110,
89,74,64,77,92,98,112,119,110,67,25,19,28,39,52,64,66,59,34,12,7,12,13,20,46,36,40,87,130,140,127,145,138,99,77,56,30,40,98,87,58,49,74,101,90,89,122,162,113,52,32,26,45,73,54,43,58,81,89,93,95,93,118,148,105,50,45,80,119,134,129,126,112,126,177,217,187,118,78,45,27,28,14,10,40,56,19,10,37,93,77,60,77,74,63,44,16,0,0,5,21,56,95,99,124,146,156,179,151,138,127,40,9,15,18,8,4,23,49,73,52,5,0,2,10,33,68,74,
55,34,15,20,35,55,92,127,115,80,52,33,26,33,41,50,67,74,73,50,38,41,38,40,45,33,50,80,102,124,112,119,113,87,83,62,66,84,112,80,36,19,37,89,115,110,134,146,68,22,15,16,33,29,17,19,43,77,89,74,66,80,101,141,172,141,115,129,132,105,105,115,121,137,177,208,153,70,43,29,24,27,4,7,49,58,22,9,25,77,132,116,121,104,71,54,27,8,2,1,2,27,77,86,113,134,151,190,164,138,108,38,4,5,23,5,0,7,47,73,47,3,0,1,23,47,44,51,
27,11,1,4,17,39,66,74,66,68,59,37,35,44,39,36,34,16,36,46,44,43,33,49,68,77,95,104,95,87,104,105,80,70,77,49,46,56,62,56,30,24,38,86,122,134,121,108,60,21,19,32,28,14,13,23,47,59,68,81,62,58,80,112,161,205,190,153,98,67,81,112,138,158,166,172,115,49,27,28,35,22,5,20,46,44,25,11,12,37,116,156,158,105,73,49,18,8,3,0,0,22,49,46,89,108,130,166,193,156,98,49,16,7,9,2,0,2,30,68,37,0,0,1,20,26,27,35,
24,10,2,1,7,19,30,36,41,58,63,55,46,39,32,24,3,0,7,10,35,50,29,51,63,84,124,154,145,107,101,92,66,62,70,45,34,38,49,67,59,52,70,110,129,107,80,86,71,43,45,41,13,4,9,25,52,46,30,64,95,83,78,93,134,189,187,140,90,68,90,93,130,169,162,156,108,55,23,19,39,23,14,21,27,39,23,2,13,44,93,138,169,96,58,56,13,0,0,1,1,15,14,15,59,80,119,151,182,179,116,47,21,32,12,0,0,10,38,83,37,0,0,1,17,16,11,23,
33,13,0,0,2,17,29,36,43,47,50,37,32,29,20,11,0,0,0,0,18,68,71,80,59,80,115,134,138,126,113,87,58,55,64,56,51,54,54,64,66,78,98,126,105,54,46,62,62,50,39,19,3,1,14,27,54,54,33,38,77,112,98,99,130,162,164,135,108,98,89,92,143,175,174,174,118,70,21,5,24,35,19,33,50,51,20,0,12,71,104,145,179,122,77,46,2,0,0,0,0,2,4,11,34,68,116,121,148,172,138,73,20,36,39,1,0,14,64,102,54,8,1,4,28,20,15,36,
41,29,11,2,4,10,12,17,11,9,20,27,33,37,16,5,0,0,0,0,20,64,102,113,86,105,127,110,84,78,95,81,43,38,47,70,84,78,59,43,55,90,77,93,87,36,37,47,50,39,35,35,35,32,37,43,49,40,45,39,45,98,126,104,115,153,126,76,77,101,121,145,161,169,182,169,108,77,25,5,20,35,45,66,54,39,24,8,18,74,116,146,162,145,96,27,0,0,0,0,0,0,0,2,18,60,87,95,135,141,119,104,51,33,64,33,6,13,70,118,84,41,26,25,40,34,41,66,
77,62,28,0,0,1,4,10,4,3,11,19,37,37,13,3,0,0,0,4,16,39,76,105,132,151,154,116,77,60,78,66,28,24,34,70,92,80,62,45,58,98,86,76,67,51,38,30,58,70,63,66,38,27,20,25,44,39,40,35,36,74,126,112,110,122,105,78,73,115,140,137,143,151,177,158,112,89,37,15,7,6,50,89,59,49,47,30,28,64,119,154,161,135,63,11,0,0,0,0,1,3,0,4,26,60,81,89,119,129,96,98,86,50,54,73,56,47,74,127,110,52,34,41,40,35,54,78,
87,81,21,0,0,1,4,19,10,13,27,34,45,25,8,0,0,7,1,0,0,15,63,101,143,185,180,130,95,81,80,44,18,17,22,39,38,41,44,56,70,92,118,107,67,64,56,50,77,96,70,38,7,6,1,0,20,54,46,28,39,73,116,118,104,99,92,104,122,134,99,99,124,141,180,145,86,52,44,25,3,4,40,99,90,62,50,45,35,58,134,184,179,126,52,12,0,0,0,0,0,5,15,38,55,77,92,87,98,112,107,102,105,68,44,44,73,84,93,116,141,101,58,46,39,41,54,64,
46,71,49,19,6,0,6,27,23,37,50,80,76,44,26,9,5,0,0,0,1,28,74,92,129,184,190,154,119,90,54,12,4,9,7,9,6,16,39,52,87,96,134,148,101,96,102,107,92,55,33,11,4,6,0,0,5,27,64,59,54,90,124,104,87,89,98,113,132,112,89,112,122,145,161,116,49,18,35,30,13,14,41,98,86,44,44,58,52,77,135,171,172,116,59,8,0,0,2,4,0,4,9,23,51,89,98,95,95,99,105,99,95,96,73,56,68,87,99,101,124,137,105,66,40,47,52,45,
27,40,78,62,17,10,20,45,60,70,56,78,59,44,39,27,39,4,0,0,10,43,46,56,121,180,184,177,124,71,18,0,0,1,8,4,4,19,58,84,116,129,156,146,96,98,89,102,59,15,6,2,1,11,2,0,2,7,58,84,77,93,113,101,73,76,93,104,108,108,115,138,137,140,122,63,36,19,25,32,9,9,46,108,81,36,44,68,68,77,107,141,158,99,58,11,0,2,13,27,13,7,9,11,23,58,70,68,76,90,89,87,102,124,98,99,112,90,84,105,119,140,126,90,50,54,63,40,
28,28,78,112,78,60,60,70,104,99,68,71,27,19,25,37,77,22,0,6,33,55,38,52,126,154,141,149,127,70,10,0,0,0,5,7,15,37,80,124,143,161,171,137,95,87,67,78,33,2,1,0,0,5,15,11,9,21,47,74,81,74,80,107,93,83,95,102,115,132,151,159,137,127,86,20,6,14,27,39,28,36,80,115,70,32,34,70,71,76,108,137,153,99,67,27,7,13,12,52,45,12,4,1,5,35,54,35,35,52,55,52,101,124,116,121,127,101,70,96,140,148,116,104,76,68,77,55,
40,51,87,102,86,78,77,74,99,95,78,50,9,7,15,30,96,59,29,40,56,70,64,76,115,132,112,113,116,87,33,4,0,0,0,0,7,9,23,87,141,164,175,151,126,105,90,80,26,6,1,0,0,0,11,43,62,67,56,77,87,77,73,96,129,118,122,137,153,167,158,130,124,115,49,8,1,9,21,45,59,54,86,96,60,40,40,81,107,118,141,135,146,130,95,64,44,34,21,46,37,6,0,1,11,35,38,27,28,34,38,44,70,84,113,124,105,108,95,105,156,151,108,105,96,83,80,67,
34,84,93,70,54,81,70,52,81,81,59,20,2,4,9,20,77,101,98,73,70,59,66,102,95,90,98,74,86,107,73,13,0,0,0,0,0,0,1,44,129,172,187,179,158,149,137,93,47,30,5,0,1,6,30,98,115,95,80,89,76,71,101,115,135,154,151,151,156,151,137,99,101,89,20,4,7,6,5,33,59,50,87,87,56,34,43,87,127,127,151,141,124,140,126,99,73,47,24,22,14,10,9,15,15,9,11,11,9,23,20,33,51,71,110,135,112,108,134,153,166,148,121,104,87,81,71,54,
47,95,105,60,49,77,41,33,62,63,43,6,0,1,0,3,41,102,127,115,101,71,62,108,95,66,73,36,37,76,77,40,15,1,0,0,0,1,9,41,110,184,210,205,189,177,153,124,98,62,18,4,19,44,78,112,118,112,84,77,60,54,96,140,159,172,167,154,151,145,126,83,87,67,9,0,4,5,4,22,52,68,90,89,62,24,19,58,118,132,145,148,129,138,130,108,78,40,30,19,9,9,8,2,0,0,1,1,2,26,16,25,52,84,138,169,154,141,154,159,159,154,118,78,74,86,84,66,
81,99,92,47,39,52,34,30,34,43,44,8,0,0,0,0,22,80,130,158,132,105,80,99,93,70,49,23,29,36,51,43,27,20,0,0,0,6,39,80,116,167,212,228,205,179,162,151,127,83,39,27,52,93,122,124,116,110,68,68,67,56,90,138,193,212,192,175,169,151,108,81,87,55,6,0,0,2,17,28,36,63,77,56,66,52,27,46,96,138,154,158,146,130,89,74,63,27,19,13,12,10,0,0,0,0,4,7,15,45,44,63,92,129,175,184,190,174,137,134,153,138,93,68,83,112,105,84,
98,93,66,23,20,44,43,19,12,32,50,21,1,0,4,18,44,74,129,162,115,96,108,86,70,63,44,30,15,8,40,37,14,25,9,3,7,21,55,102,115,146,198,225,202,185,175,151,143,108,78,84,118,154,166,140,122,110,89,93,99,93,122,167,213,213,195,167,143,138,124,108,98,49,4,0,0,0,8,39,36,43,45,24,27,70,54,47,77,124,145,138,112,95,49,34,30,6,1,0,7,12,2,1,1,4,23,36,54,92,112,122,127,161,203,202,193,145,107,122,132,110,83,67,86,126,110,89,
92,78,43,6,8,33,27,5,6,33,68,35,8,1,6,24,40,67,124,175,132,96,110,67,38,47,51,32,17,21,50,39,19,27,17,15,23,32,58,87,112,156,185,193,182,184,175,182,187,159,149,158,174,190,161,122,115,118,105,98,84,90,129,177,184,164,174,171,154,151,154,137,108,46,4,0,0,0,2,32,49,39,22,17,11,47,83,74,89,119,116,78,51,56,45,24,21,11,2,0,6,8,1,4,10,27,59,71,90,112,113,122,146,184,218,208,167,112,96,104,95,78,73,68,84,130,130,89,
81,73,30,6,11,19,27,11,9,34,78,46,17,15,13,7,22,55,110,164,135,122,108,54,44,67,60,54,62,63,60,49,32,35,28,27,33,49,66,74,112,143,164,161,134,158,174,198,217,202,187,164,169,161,127,96,87,54,43,46,46,73,129,177,158,138,151,172,167,137,130,153,115,40,10,0,0,0,1,13,45,54,24,17,12,30,74,87,102,115,76,41,36,47,45,24,11,11,18,15,22,14,2,1,3,23,36,50,81,95,93,116,161,205,222,189,122,83,78,76,62,55,68,81,108,140,122,78,
90,81,43,21,19,4,9,19,19,28,66,76,44,24,9,12,20,38,107,151,119,118,116,89,96,89,63,70,89,92,84,63,41,29,20,24,43,60,66,90,107,121,153,141,105,115,149,172,171,158,161,140,134,134,110,86,49,22,27,44,66,105,149,166,130,115,134,156,164,141,134,161,129,50,27,2,0,0,0,7,26,63,47,15,13,33,66,89,102,102,76,59,51,41,35,26,17,22,45,40,32,27,12,1,5,12,9,29,76,108,124,146,182,205,207,151,71,49,59,60,39,43,39,55,98,116,105,84,
107,96,73,40,19,6,2,14,19,14,27,70,74,29,11,16,26,45,116,151,130,134,145,132,112,66,51,59,76,95,83,67,51,26,8,8,29,68,107,130,135,122,93,89,90,83,102,145,132,105,118,108,118,112,93,77,56,47,54,68,81,89,112,98,66,77,113,113,140,159,148,130,126,90,54,29,3,0,0,8,22,52,54,23,34,64,93,121,143,129,102,68,41,37,44,45,50,66,49,19,14,22,22,6,7,9,6,26,78,105,113,141,190,195,184,138,51,33,52,45,23,28,24,46,87,90,93,107,
122,96,60,49,8,2,4,9,6,1,5,33,74,56,22,20,56,92,134,149,146,149,146,122,92,59,58,73,64,41,43,58,55,28,18,18,39,90,138,122,99,90,52,37,54,58,56,89,98,86,77,66,92,87,62,62,44,30,37,46,40,45,66,47,37,70,86,59,92,132,129,102,90,101,76,55,27,1,0,11,40,54,58,67,74,81,119,138,149,151,127,98,83,78,77,76,78,68,32,9,2,8,19,20,15,18,7,13,52,95,98,132,197,198,162,126,68,45,51,33,20,20,29,78,93,83,92,118,
113,68,37,50,4,0,0,0,1,0,1,13,56,74,51,49,84,137,151,153,154,161,156,129,104,84,83,70,35,16,18,29,45,15,7,23,51,90,135,112,63,56,43,18,30,41,39,52,47,56,37,27,60,64,30,22,0,0,11,22,32,36,35,21,30,62,56,36,64,96,119,105,64,63,64,62,47,11,5,14,43,62,84,96,81,95,143,151,141,135,113,126,122,98,86,87,71,58,47,19,2,0,12,32,35,28,21,16,30,81,112,137,195,213,172,115,86,62,58,45,26,11,45,84,55,64,74,105,
96,55,37,52,6,0,0,3,8,2,1,2,26,51,46,59,92,134,149,149,143,149,158,141,137,132,102,55,28,18,3,0,23,25,4,14,45,67,102,124,98,71,36,13,27,29,35,38,28,18,18,18,32,51,27,6,0,0,1,8,38,36,19,14,28,58,59,41,59,83,99,86,56,35,44,81,58,18,13,20,43,86,113,107,105,132,182,182,158,122,92,108,90,90,86,73,68,51,52,40,11,2,8,33,47,29,21,30,43,63,108,148,190,213,187,134,80,50,67,70,28,22,71,77,54,62,55,90,
98,73,56,52,12,0,0,0,4,0,0,0,2,12,11,29,67,110,121,126,134,140,167,172,148,110,86,64,35,14,0,0,4,30,22,23,38,51,63,95,124,105,54,32,37,21,27,23,17,0,0,6,12,27,32,8,2,1,1,22,64,58,33,23,29,59,63,58,64,98,102,55,41,35,45,80,44,21,27,43,78,108,113,121,135,146,175,169,130,84,60,59,52,73,51,25,38,45,49,62,44,23,20,36,43,34,16,19,43,64,83,145,207,225,192,143,73,28,35,62,60,66,99,83,78,66,62,99,
122,86,71,52,4,0,0,0,1,0,0,0,0,0,0,3,30,90,87,83,130,158,197,180,115,73,58,59,38,9,2,2,7,16,16,14,29,35,44,49,73,102,93,80,58,30,26,11,8,4,0,0,1,1,19,15,8,9,17,37,74,76,43,15,30,52,50,66,74,118,104,41,29,40,56,59,25,22,39,68,104,95,95,99,108,134,124,99,84,46,23,17,27,33,25,11,15,27,46,74,86,54,38,47,36,38,46,46,51,77,89,126,203,242,198,135,89,38,21,32,55,90,104,92,96,89,102,132,
124,78,64,66,23,0,0,0,3,0,0,0,0,0,0,0,12,64,63,59,113,154,198,158,98,78,77,66,43,15,1,0,8,11,8,22,23,5,8,19,21,27,62,95,81,55,39,19,10,15,10,6,0,0,8,5,5,5,6,23,50,43,35,35,51,49,45,77,96,126,101,44,40,40,49,32,11,6,17,46,86,104,110,95,87,135,108,73,50,6,0,2,9,19,28,15,10,4,9,37,89,83,59,66,39,32,60,81,80,95,127,149,207,242,207,132,76,56,29,17,29,78,86,59,68,81,116,143,
121,98,59,40,44,32,11,3,3,4,5,0,0,0,0,0,9,33,45,63,96,122,185,159,104,101,74,55,36,27,16,1,0,7,8,23,13,0,0,0,2,2,12,59,95,81,47,27,24,30,35,19,5,8,5,0,0,0,1,21,27,23,28,44,60,55,55,89,116,129,115,78,54,34,33,15,1,0,2,34,87,137,151,134,121,146,113,70,35,5,1,0,3,8,11,5,8,5,6,29,60,63,66,87,64,50,52,56,78,89,134,175,198,202,200,156,90,58,43,19,27,76,52,25,30,34,74,105,
115,118,73,23,9,27,38,13,8,13,5,1,4,0,0,0,11,21,34,58,74,108,169,177,135,122,99,67,34,26,35,8,0,0,5,18,13,0,0,0,0,1,6,29,68,89,50,24,26,20,15,7,3,7,1,0,1,1,13,28,20,8,7,21,44,46,66,83,108,115,118,108,55,35,27,10,2,2,9,40,66,112,156,164,177,166,122,73,30,4,0,0,1,11,13,10,11,14,25,24,18,19,35,62,76,77,52,41,73,93,127,177,179,167,179,153,121,95,55,28,40,78,40,19,4,1,43,81,
89,90,70,40,15,21,32,29,19,2,0,0,0,2,4,5,17,24,35,46,66,115,169,190,174,158,121,52,15,12,38,18,0,0,0,13,5,0,0,0,0,3,15,11,12,41,49,35,27,4,4,0,0,1,0,0,2,13,43,59,49,19,11,26,28,20,41,71,95,95,107,108,80,51,25,0,0,0,14,27,35,87,146,145,177,154,124,76,19,0,0,0,9,36,23,8,7,23,22,2,0,4,18,29,36,37,36,44,74,115,140,172,162,148,151,124,93,101,78,44,55,76,39,9,0,0,22,76,
55,76,60,54,44,23,15,30,32,1,0,0,0,0,0,15,22,18,36,60,71,119,167,192,203,169,101,34,8,16,35,8,0,0,3,4,0,0,0,0,0,2,0,0,0,7,34,52,27,4,8,0,0,3,0,0,2,9,33,80,96,70,47,44,25,25,58,105,121,116,118,102,73,47,22,2,0,0,15,27,40,93,134,159,175,124,118,83,22,6,1,5,36,43,18,6,13,28,7,0,0,2,11,5,9,29,35,41,64,81,112,145,115,86,93,95,56,66,86,70,70,64,17,3,1,3,28,55,
14,55,74,78,67,45,28,34,54,16,0,0,0,0,0,10,28,28,44,80,89,116,153,185,210,162,102,50,25,41,46,1,0,0,7,0,0,0,0,0,0,0,0,0,0,4,21,45,36,26,34,15,15,11,2,5,0,0,22,77,70,60,77,67,55,73,112,143,137,130,127,92,59,44,13,0,1,2,12,45,73,112,141,198,192,141,137,89,37,27,27,52,71,46,21,18,49,52,4,0,0,0,7,0,0,15,32,27,26,37,77,118,87,59,77,107,70,54,87,93,96,78,32,17,11,19,19,13,
3,24,76,76,73,70,68,63,67,46,26,16,10,5,6,32,62,67,76,101,108,118,138,174,202,169,127,89,71,89,63,15,2,5,14,0,0,0,0,0,0,0,0,0,0,17,27,45,54,62,58,20,11,8,15,19,0,1,19,77,67,41,66,95,102,110,138,145,121,98,98,83,73,58,8,0,0,1,22,73,107,143,184,222,210,169,145,101,71,76,93,92,45,18,5,7,54,59,19,0,0,5,3,0,0,0,9,11,17,38,80,98,68,56,81,113,126,107,118,115,119,110,63,24,9,6,0,3,
2,10,41,66,81,96,96,98,80,70,87,68,45,46,44,77,87,98,124,127,113,115,124,159,187,184,174,149,134,101,43,25,17,8,5,0,0,0,0,0,0,6,8,4,15,47,78,76,62,52,26,1,0,2,14,24,14,15,32,70,86,67,74,108,135,143,138,116,92,67,68,87,102,76,24,13,19,41,84,107,132,171,198,202,179,153,134,118,113,110,95,45,7,3,0,1,39,33,27,13,9,16,0,0,0,0,5,7,11,24,59,63,38,38,77,101,153,151,119,99,122,126,70,19,3,0,0,0,
4,8,23,64,108,145,149,118,104,105,101,98,107,105,95,101,83,73,105,110,87,92,115,146,161,172,180,174,145,77,35,38,36,30,13,0,0,0,0,1,8,16,21,32,55,71,78,77,50,30,18,2,0,1,16,38,38,38,52,78,92,102,101,116,153,167,121,101,96,74,66,89,119,104,68,66,90,115,105,105,140,177,198,190,161,143,119,104,95,87,68,26,4,5,2,5,26,13,21,30,27,27,0,0,0,0,4,2,0,8,47,50,24,24,78,121,143,127,101,101,135,138,76,15,1,2,0,1,
19,22,35,62,105,158,182,151,124,116,102,92,108,116,89,71,34,27,58,93,87,90,105,126,137,148,153,162,158,110,83,68,45,49,50,29,12,5,9,26,25,20,27,46,52,29,22,33,38,20,11,12,18,28,50,60,50,50,76,110,113,112,126,141,161,171,143,124,119,119,116,116,129,124,110,110,124,129,108,116,159,182,192,189,174,153,134,124,115,102,70,27,9,18,13,2,6,5,13,51,80,41,0,0,0,1,4,0,1,27,68,63,37,35,93,138,134,149,137,141,169,145,71,20,6,15,28,29,
26,23,24,45,98,153,195,193,151,116,93,80,87,101,83,45,12,12,39,86,87,68,86,119,145,156,162,164,161,153,129,90,67,58,58,59,35,20,26,34,45,37,27,23,24,11,15,25,29,23,11,13,32,49,46,52,66,73,98,127,105,116,159,175,171,172,167,158,143,129,116,107,98,99,98,78,78,102,110,119,156,203,205,185,177,167,158,149,129,104,70,27,19,33,8,0,0,0,4,41,107,68,17,5,5,9,2,0,8,30,58,68,60,60,108,141,135,159,156,159,175,130,80,49,45,40,34,32,
19,23,33,56,101,121,148,175,172,141,116,104,92,83,83,46,8,4,23,56,66,60,92,134,154,156,167,182,177,162,153,151,135,107,86,74,56,38,20,23,67,77,34,25,10,12,26,19,28,35,32,20,19,23,26,22,39,73,102,118,127,161,198,200,180,158,145,153,138,81,58,55,49,49,54,49,46,51,78,99,124,180,217,197,172,153,135,93,78,87,77,37,40,38,8,2,1,0,0,32,104,108,66,44,24,8,0,0,18,38,77,84,70,86,126,130,124,164,193,203,187,134,86,60,58,30,26,17,
28,43,54,62,81,102,113,129,156,167,141,104,66,38,47,63,25,4,13,46,64,83,116,129,102,80,90,124,156,180,193,205,203,174,141,126,93,71,44,50,90,104,76,39,16,33,19,4,13,7,9,17,9,2,13,30,28,49,80,115,156,193,208,202,185,162,134,104,81,52,26,12,21,27,19,14,27,26,32,83,129,172,208,189,137,90,81,56,39,67,84,67,50,28,28,15,9,8,15,47,108,121,66,20,7,1,0,0,27,67,96,60,46,81,143,164,169,202,242,228,171,121,86,64,68,51,40,25,
51,50,34,58,76,86,99,96,127,140,108,63,26,15,23,45,56,34,28,44,50,90,121,102,60,35,32,52,68,90,137,184,212,215,208,189,141,127,101,102,116,105,98,66,46,37,5,1,2,0,1,13,11,4,8,43,50,46,59,98,141,167,174,187,169,138,122,96,63,35,11,3,10,29,15,5,8,22,16,40,113,172,192,172,126,74,52,30,20,45,50,70,66,32,43,50,19,15,41,89,132,129,83,28,4,0,0,0,34,93,84,37,29,64,113,158,187,218,240,207,156,132,108,99,107,89,67,60,
81,56,39,55,38,27,62,76,105,116,87,58,27,22,27,30,46,49,43,32,32,77,102,70,40,16,13,15,22,29,58,119,154,171,213,217,192,189,158,151,140,110,107,96,77,40,8,8,3,1,11,11,19,40,43,58,71,58,47,73,130,143,134,154,143,99,86,87,76,55,40,35,27,21,15,7,2,11,32,49,98,153,161,138,102,67,41,13,11,32,18,29,58,47,47,71,64,50,80,129,137,92,76,60,17,0,0,5,46,99,76,27,21,43,54,84,134,180,207,205,175,129,101,98,98,98,107,105,
81,59,35,32,13,9,39,78,118,138,113,89,67,55,58,60,60,50,47,36,32,70,89,60,37,13,9,0,0,21,51,81,92,113,156,172,193,208,185,190,175,143,124,112,118,74,29,22,20,15,16,25,33,47,70,71,68,63,59,68,115,126,115,124,129,119,98,95,89,71,46,20,9,6,4,10,11,10,26,46,66,119,143,116,99,77,51,24,22,28,7,11,32,43,78,101,122,121,129,141,127,66,35,49,40,8,5,21,41,95,78,23,19,23,21,41,81,132,177,203,159,108,99,107,112,118,115,98,
80,68,45,34,17,16,35,66,110,134,134,127,115,105,98,93,78,70,51,40,47,73,92,60,26,16,18,2,1,19,29,34,49,77,107,112,140,192,205,205,192,164,141,122,137,127,90,66,45,28,41,51,35,34,45,58,47,26,30,59,93,107,121,138,126,132,132,121,98,60,24,2,0,2,0,4,27,40,43,50,78,132,134,105,81,60,41,16,17,28,12,11,21,38,78,126,151,143,148,140,112,71,37,35,49,37,26,21,41,96,80,45,30,8,9,28,56,98,146,187,158,129,116,115,115,104,92,73,
102,105,80,62,50,40,49,76,93,96,104,104,95,98,108,107,87,83,55,26,43,70,90,64,19,7,11,3,1,0,0,4,19,54,81,86,95,141,205,227,200,169,137,113,119,119,121,129,108,96,101,76,46,28,38,51,44,11,6,26,71,81,101,151,149,127,130,145,98,52,30,7,0,6,7,7,9,27,63,99,140,149,115,83,25,9,6,0,2,15,6,3,13,21,55,115,149,158,167,164,122,78,46,43,38,46,40,40,63,99,78,70,47,12,14,44,74,108,146,174,151,104,77,78,90,102,105,86,
83,107,124,107,107,98,93,95,78,73,87,87,74,71,73,86,59,55,67,37,46,60,55,49,23,5,2,0,0,0,0,0,10,39,62,80,107,143,192,232,223,177,126,96,95,78,78,98,89,83,73,49,25,22,41,40,17,8,3,5,38,89,102,145,162,129,113,122,101,43,24,22,22,33,30,9,0,2,20,67,119,138,113,68,8,2,0,0,0,4,0,0,9,41,86,108,121,129,134,146,138,95,43,50,46,51,74,71,90,104,78,54,40,43,52,66,76,116,161,179,141,98,73,77,99,124,112,78,
51,92,148,121,134,145,115,80,54,54,76,87,90,40,20,34,32,23,45,58,60,46,28,19,7,7,0,0,0,0,0,0,6,36,44,55,102,148,185,205,217,197,130,104,77,40,49,56,44,34,22,13,7,24,59,30,0,0,1,0,8,68,135,174,177,151,99,66,80,59,30,44,76,52,18,7,1,0,3,34,102,127,113,59,12,2,0,0,4,5,0,0,12,58,70,68,98,122,132,146,171,140,73,66,80,73,98,112,130,127,74,51,30,38,76,101,95,119,161,169,145,115,95,96,104,112,98,67,
58,80,148,149,130,126,105,64,30,43,60,68,64,27,2,1,13,19,35,56,70,45,17,1,0,1,9,5,2,0,0,5,9,11,35,50,60,101,140,164,184,198,159,122,64,36,50,43,33,23,8,3,4,27,76,33,0,0,0,2,8,37,108,180,205,164,98,50,36,40,43,41,36,21,9,10,7,9,16,54,104,80,68,62,22,2,0,0,13,0,0,0,26,70,73,95,135,153,171,185,198,189,138,108,116,121,137,166,179,130,68,60,56,49,68,121,141,137,132,119,107,95,90,98,115,138,126,86,
60,80,119,158,121,93,101,62,28,39,50,47,32,19,3,0,0,12,26,40,51,36,11,0,0,1,23,47,19,4,0,0,0,0,8,38,39,50,99,159,172,167,169,140,86,83,74,41,39,30,8,0,3,32,76,40,8,0,0,1,24,41,89,189,235,137,71,46,25,9,7,8,7,13,16,12,3,15,45,78,84,51,56,62,27,9,1,3,9,0,0,1,41,108,134,138,137,146,166,197,233,220,189,167,156,156,174,203,198,141,99,84,84,99,119,151,169,134,93,77,63,55,77,113,135,158,116,63,
45,87,77,92,108,71,74,56,32,37,54,44,24,4,0,0,0,2,19,45,33,21,15,8,3,5,26,50,26,2,0,0,0,0,1,17,14,19,63,154,175,137,135,145,138,121,59,30,44,35,13,5,8,35,71,44,11,13,11,10,32,67,113,195,218,137,60,30,28,15,6,8,9,15,25,32,19,22,49,89,99,70,43,28,36,19,16,13,4,8,15,40,87,127,135,122,126,145,166,200,240,223,182,164,171,172,189,225,223,190,167,145,118,127,169,190,174,145,107,66,47,35,50,77,96,130,86,47,
55,80,58,59,71,68,77,46,23,32,33,22,21,4,0,0,0,0,5,45,36,19,6,0,0,0,2,27,18,0,0,0,0,0,5,20,16,34,80,153,158,108,108,153,171,108,37,19,44,20,0,0,7,20,49,74,41,28,47,56,49,68,135,182,189,158,90,34,27,34,19,23,34,45,62,67,67,59,68,104,115,71,27,25,47,23,22,38,25,27,51,93,115,99,98,96,93,105,132,177,185,172,169,141,145,169,190,213,225,222,192,184,189,177,187,169,148,138,108,45,29,14,21,52,67,105,81,52,
66,74,67,52,50,74,77,27,17,26,19,9,1,1,1,0,0,0,5,30,28,11,0,0,0,0,0,21,18,0,0,0,0,0,4,25,11,25,80,141,130,96,116,151,167,98,34,17,43,25,0,0,1,5,8,30,54,37,35,77,98,93,119,153,172,141,112,68,29,17,15,22,44,76,108,116,112,112,113,135,124,92,54,62,59,27,22,44,59,60,67,81,87,80,87,93,80,74,101,132,134,137,151,145,137,174,192,175,177,195,185,172,198,195,182,146,130,101,74,63,41,8,4,39,64,87,64,45,
34,38,56,60,54,64,54,21,24,36,30,9,0,0,0,0,1,12,25,37,20,7,0,0,0,0,1,34,38,9,0,0,0,0,0,27,27,33,83,130,119,112,121,137,159,108,58,39,58,36,3,0,0,0,0,1,25,47,41,67,130,135,119,145,164,129,95,86,49,13,4,5,13,32,70,124,158,169,185,198,167,130,110,107,60,32,30,40,62,52,34,49,59,76,104,86,96,89,70,90,102,110,110,132,149,180,184,149,129,149,177,169,190,198,200,175,141,104,70,68,55,24,8,32,71,77,37,22,
18,12,27,46,49,54,40,27,37,45,19,0,0,0,0,0,11,33,49,50,32,25,9,0,0,6,23,62,50,9,0,0,0,0,2,38,62,70,105,118,107,119,119,148,171,143,96,74,74,34,5,0,0,0,0,3,9,20,41,74,121,140,132,137,166,159,122,89,78,38,6,1,7,21,45,101,158,161,182,198,175,153,127,107,60,39,39,43,56,38,28,51,76,107,119,89,112,112,68,66,63,84,71,87,137,172,161,137,112,104,132,137,143,154,175,182,158,124,89,81,52,22,22,29,55,67,29,12,
32,13,24,35,46,38,12,8,22,34,19,2,0,0,0,0,2,26,63,64,51,40,19,22,30,37,50,89,56,8,0,0,0,1,15,50,84,110,124,105,96,113,130,159,172,151,102,73,70,30,4,0,0,0,0,2,4,16,50,84,110,127,124,124,143,156,143,105,77,80,59,29,27,36,58,102,145,146,169,175,156,113,62,59,36,23,25,34,54,40,44,73,101,126,132,130,127,104,78,44,33,68,74,74,108,141,132,96,87,87,80,92,80,73,87,118,119,119,93,102,77,23,13,16,44,73,56,39,
76,47,50,50,40,11,0,0,3,8,28,39,8,0,0,0,9,46,81,81,67,30,7,7,21,49,59,98,81,28,6,1,1,5,26,62,101,99,93,99,104,115,124,130,154,171,141,110,89,36,3,0,0,2,8,15,29,45,58,71,96,110,102,113,141,164,151,119,74,54,83,95,80,73,81,105,121,141,177,169,132,66,27,30,21,18,24,41,73,70,90,124,138,148,167,180,146,93,56,56,40,62,90,93,107,113,105,52,44,56,39,55,34,25,37,71,96,96,90,115,110,55,30,36,80,107,92,86,
112,99,80,62,34,4,0,0,0,0,22,54,49,20,19,37,56,74,93,95,93,44,16,8,12,47,71,96,78,27,9,15,29,46,58,64,73,44,27,43,74,105,118,127,146,184,185,158,104,36,5,0,2,12,25,24,15,17,35,55,73,108,110,121,167,198,179,130,101,60,55,86,93,73,71,68,74,129,151,130,98,43,25,22,15,28,49,80,113,122,141,162,174,192,213,197,146,87,50,62,78,89,104,96,98,89,76,30,30,26,16,36,12,9,22,45,77,96,96,122,132,102,89,99,110,96,74,76,
77,84,71,39,11,0,0,0,0,1,8,15,54,77,66,58,51,64,93,98,108,90,60,45,36,67,108,107,89,54,49,55,73,86,59,22,13,18,4,6,32,76,89,90,127,174,174,146,115,44,12,14,17,14,24,22,4,3,24,54,63,112,148,156,197,220,213,151,108,99,81,76,54,54,50,33,56,124,141,132,92,44,51,32,28,60,95,121,135,130,141,158,162,187,198,180,159,130,108,105,121,115,95,74,66,63,50,16,23,17,18,35,15,15,33,60,90,96,105,122,143,130,110,93,64,52,62,67,
43,52,50,18,3,0,0,0,0,2,0,0,18,70,95,78,71,89,86,105,116,127,130,116,110,116,122,112,110,93,77,71,71,63,35,8,0,3,0,0,17,71,93,78,107,158,156,122,112,77,33,34,32,27,32,37,25,19,55,83,104,130,151,166,177,184,207,180,134,121,102,70,35,40,51,40,68,135,167,151,104,70,81,66,78,105,118,140,132,116,132,148,156,167,149,143,116,90,93,107,92,68,70,49,32,45,27,6,15,35,46,59,49,58,87,107,93,78,98,96,124,124,76,56,36,25,39,52,
46,54,55,37,19,9,6,5,6,0,0,3,28,76,113,129,132,113,98,129,130,151,177,138,119,104,96,110,112,93,83,76,58,24,4,1,1,0,0,0,13,52,105,115,137,167,162,135,119,108,83,59,59,73,81,74,63,71,116,140,149,153,146,143,146,148,169,175,161,130,92,87,52,44,74,83,95,124,169,166,112,70,76,86,105,122,134,145,141,137,148,161,180,158,135,110,84,60,56,46,23,9,18,35,30,44,25,18,39,73,98,102,95,104,130,113,77,74,101,84,113,113,63,45,32,22,35,51,
86,80,71,44,25,21,33,46,28,11,13,29,54,98,140,156,171,164,156,159,149,171,172,116,80,66,77,104,87,74,83,86,55,20,0,0,0,0,0,0,11,33,92,115,154,193,177,153,137,118,121,115,99,104,116,116,101,105,137,121,122,159,162,137,132,130,156,166,161,141,89,83,87,77,89,99,93,99,145,184,132,80,77,108,138,149,130,129,151,169,166,187,175,126,113,73,54,52,27,13,6,0,1,24,47,58,62,80,98,105,116,126,121,122,148,149,115,112,134,118,118,116,81,71,68,64,76,89,
110,98,95,66,43,50,71,77,51,45,51,58,80,138,153,151,182,200,203,197,190,195,158,104,70,55,77,90,51,41,55,87,77,43,11,0,0,0,0,1,11,36,101,126,172,208,166,135,116,101,116,129,108,102,119,137,112,99,105,81,105,135,159,158,129,130,156,153,143,148,116,93,90,70,46,37,58,95,146,207,184,132,126,151,169,137,108,121,129,129,141,154,122,84,92,40,16,15,1,0,3,0,5,28,56,76,99,102,93,98,113,141,172,182,200,208,177,159,169,151,129,129,127,126,113,99,98,104,
141,132,126,115,90,104,129,132,95,83,96,115,146,175,149,146,169,159,169,187,184,167,134,93,66,62,86,70,26,18,29,71,67,34,27,2,0,0,0,2,11,46,113,158,205,223,172,138,115,105,98,68,52,63,73,89,90,89,92,96,116,126,141,161,141,129,124,134,137,129,107,73,40,30,22,13,24,78,146,197,207,205,192,175,159,112,104,105,99,104,95,93,78,64,73,24,6,6,7,8,2,2,13,44,64,87,104,104,107,126,138,143,177,217,223,212,200,202,190,166,141,137,146,130,102,81,104,124,
158,177,172,149,135,156,161,180,161,138,132,137,159,158,119,115,126,119,146,140,134,124,99,86,81,81,93,62,25,13,21,45,29,11,17,8,0,0,0,0,6,45,115,172,218,220,175,135,105,74,26,6,8,25,49,64,60,80,84,95,121,122,122,135,143,108,74,96,143,124,60,19,7,16,21,27,51,98,153,185,185,198,203,202,161,121,110,81,81,74,55,67,67,70,59,19,8,9,21,25,11,13,23,55,83,92,107,104,115,113,127,143,162,197,197,192,198,210,203,192,175,166,153,121,101,107,130,143,
153,174,169,118,101,122,122,137,127,127,119,121,146,132,92,89,102,108,122,119,126,129,116,113,116,107,84,29,15,8,11,24,29,15,11,13,0,0,0,0,6,52,118,164,205,179,138,108,76,32,4,0,0,3,35,78,71,90,104,116,116,115,126,122,129,95,56,66,132,124,41,4,4,6,8,39,73,83,104,153,175,171,193,207,174,149,107,76,68,51,50,62,54,68,37,4,0,3,14,23,43,58,46,44,63,67,95,92,101,95,95,149,180,187,175,162,153,148,159,185,190,185,166,112,95,129,174,159,
137,137,143,95,64,68,76,71,40,52,73,78,112,101,66,60,78,92,101,129,149,148,148,137,126,127,73,15,12,1,1,15,25,4,1,10,0,0,0,0,19,66,95,146,185,148,116,77,43,26,9,5,7,7,32,67,90,115,137,134,122,129,107,92,115,70,58,71,115,107,32,4,1,0,7,26,34,47,66,112,162,167,180,169,148,135,92,70,66,63,68,49,52,70,40,12,1,4,25,49,67,77,39,19,34,46,77,87,84,108,107,162,180,156,124,84,80,81,93,129,149,153,113,77,78,108,169,169,
112,105,113,86,54,39,44,23,2,4,33,55,67,55,54,56,63,74,93,104,101,115,127,137,159,151,80,38,23,1,2,15,20,1,0,9,0,0,0,0,36,62,95,156,161,141,122,56,23,11,2,5,25,37,37,46,90,132,127,112,116,124,84,83,107,41,38,80,104,83,20,1,0,0,9,4,0,8,43,89,122,153,177,149,118,116,87,60,54,68,77,59,81,76,35,26,30,40,50,50,50,39,28,19,18,18,59,89,80,116,149,174,145,119,86,40,40,44,54,89,115,105,78,56,55,70,118,127,
96,92,81,54,27,17,15,2,0,0,12,40,32,33,44,29,27,47,63,62,43,59,113,151,177,158,108,89,55,28,32,29,30,13,3,12,3,0,0,4,56,96,130,179,169,154,134,64,32,17,13,27,41,39,28,45,102,156,156,138,148,156,118,112,105,41,46,78,99,76,8,0,0,0,11,0,0,0,15,56,84,124,175,158,115,105,98,62,40,52,76,92,108,83,43,29,45,67,37,11,18,19,27,27,6,11,64,96,86,116,161,162,127,98,63,33,35,40,56,70,66,66,54,52,43,55,102,112,
112,99,71,44,19,9,4,0,0,0,10,12,4,17,16,2,4,18,19,37,34,33,78,151,187,171,116,86,60,44,45,37,26,9,8,17,15,7,10,38,74,101,141,156,153,158,143,90,51,38,36,27,40,44,43,66,108,140,172,189,195,179,161,156,116,74,76,87,115,68,3,0,0,0,13,1,1,7,19,47,74,96,137,164,140,107,86,81,59,49,55,78,110,105,93,84,93,80,28,3,5,15,30,41,25,45,101,107,92,113,145,161,126,87,62,46,52,66,59,28,25,27,10,27,26,45,108,121,
137,129,90,54,27,18,6,0,0,0,11,3,0,9,13,0,0,1,1,11,35,36,50,110,197,200,122,73,39,21,27,55,41,13,9,19,17,19,30,62,68,80,138,118,95,116,132,107,59,45,35,23,55,81,60,59,54,70,132,164,195,205,185,175,122,87,83,105,126,66,10,5,0,0,11,2,3,4,6,21,47,68,101,129,143,129,90,78,87,74,58,71,104,92,93,108,121,99,39,9,11,18,28,49,44,63,95,110,108,116,151,143,96,74,60,60,63,80,49,13,13,9,0,11,18,38,93,110,
127,135,99,67,50,29,1,0,0,0,6,8,4,10,11,0,0,0,0,2,27,44,56,86,167,189,148,102,67,51,60,78,52,22,19,27,17,14,27,59,77,90,132,116,83,98,112,112,70,44,45,34,44,62,43,32,15,40,98,107,154,210,222,197,127,89,101,135,126,66,26,8,0,2,5,0,0,0,0,4,9,35,77,89,104,134,138,116,101,105,90,86,80,51,50,68,108,115,51,29,35,21,30,45,39,64,112,134,137,141,167,122,90,66,52,73,66,74,64,19,5,6,0,5,14,29,67,90,
83,96,98,89,64,34,6,0,0,1,6,19,36,19,7,1,0,0,0,0,20,44,66,113,158,146,127,115,93,89,92,86,51,29,39,43,33,43,58,89,119,145,143,137,118,112,105,112,84,54,44,27,15,27,33,21,9,36,62,54,93,175,220,200,158,138,138,145,115,41,13,8,4,4,3,0,0,0,0,0,0,20,51,55,60,87,143,167,145,118,110,96,64,39,38,51,95,105,68,58,66,63,60,46,56,90,134,164,169,175,175,130,115,73,67,99,64,44,54,32,8,2,4,5,7,12,40,70,
43,66,86,107,78,55,15,2,6,6,13,37,43,28,6,0,0,0,0,0,9,44,67,129,166,127,101,98,105,92,66,70,59,55,73,58,70,80,80,129,171,198,172,148,140,124,96,66,52,49,27,16,18,27,37,28,21,37,27,29,64,141,202,187,164,138,118,129,110,36,18,7,0,2,2,3,0,0,0,0,0,23,13,13,35,55,93,137,162,145,112,95,68,46,35,50,84,70,58,81,96,110,99,77,80,78,104,167,197,198,187,166,134,95,101,127,67,29,27,24,26,15,7,6,4,1,11,35,
32,58,93,102,105,78,12,2,8,7,23,32,15,15,11,0,0,0,0,0,6,52,80,132,164,137,112,112,108,81,50,46,66,86,93,93,119,108,110,145,156,172,146,119,121,102,73,34,28,35,17,6,9,26,45,46,54,49,27,32,56,119,192,184,143,112,98,116,122,68,35,1,0,3,0,8,0,0,1,0,13,18,1,0,10,40,52,67,118,156,146,101,55,47,29,45,60,27,27,60,104,127,108,104,102,105,137,182,195,198,198,161,98,73,102,137,80,29,6,0,0,9,8,2,7,4,8,27,
37,55,107,115,138,101,26,15,9,8,27,19,9,5,5,0,0,0,0,8,16,68,121,151,149,149,143,124,104,77,47,27,41,84,104,105,115,118,135,145,145,148,107,92,99,45,35,24,19,13,10,14,20,29,44,40,32,35,44,47,59,115,187,180,146,122,104,102,116,107,60,20,19,9,5,23,7,5,10,9,32,14,1,4,13,21,43,46,67,124,158,105,50,51,33,41,27,1,7,44,89,135,146,134,140,145,154,174,180,192,175,140,76,49,74,92,98,45,7,0,0,0,6,2,5,20,27,41,
39,46,104,145,158,107,50,36,14,19,30,5,8,18,3,0,1,0,0,24,54,90,154,169,149,137,137,118,80,43,46,41,50,87,67,50,62,98,137,164,167,129,77,71,71,36,27,10,10,6,4,28,58,60,59,47,27,24,49,70,80,135,171,149,154,156,130,107,96,99,92,80,62,37,32,47,30,29,16,19,55,19,5,16,21,13,33,67,74,101,130,134,83,67,52,40,9,0,5,20,54,115,151,175,184,192,187,185,193,185,141,113,77,46,40,37,93,93,34,7,2,0,4,9,10,35,50,46,
35,54,98,153,171,116,76,49,24,37,44,8,9,36,15,0,1,10,13,39,92,115,151,182,154,112,81,68,51,36,46,71,68,64,37,37,56,80,112,141,134,98,59,52,49,47,44,8,6,11,6,27,27,46,60,56,64,63,67,77,110,151,153,137,153,171,164,148,122,96,60,51,71,95,95,84,52,51,46,50,89,44,15,19,37,49,54,74,87,71,66,105,127,102,74,40,11,4,6,4,25,80,122,158,198,237,242,225,205,169,129,99,74,49,27,30,74,90,60,40,22,3,0,9,19,25,26,27,
16,60,130,185,200,140,87,44,27,47,73,37,35,59,30,5,1,13,51,95,112,119,164,162,98,49,27,33,45,37,27,37,45,45,46,49,45,56,101,127,99,84,58,39,28,22,25,25,17,21,21,19,9,26,28,25,51,74,81,105,135,158,164,154,151,132,110,107,112,102,80,78,99,124,143,115,78,71,98,113,124,92,51,27,38,78,70,62,78,60,34,40,90,107,83,49,30,25,17,19,35,51,78,126,180,225,242,237,190,138,99,86,80,45,26,30,38,62,70,56,35,36,17,8,8,4,10,21,
27,60,129,177,172,124,70,34,30,60,80,84,78,70,46,34,25,39,92,118,113,138,151,84,24,6,2,20,35,19,15,19,25,35,52,40,45,71,112,124,112,108,83,51,26,9,10,26,46,43,29,15,13,29,24,20,39,71,112,143,159,156,149,151,140,113,74,54,71,99,116,127,132,154,179,167,149,135,145,171,156,107,89,64,63,71,51,44,44,38,21,20,32,58,93,62,25,19,21,29,56,55,51,89,159,213,218,207,179,121,68,44,52,40,21,21,29,55,80,49,19,36,58,32,13,15,30,40,
23,40,105,145,122,99,63,39,49,58,67,118,102,90,89,83,92,105,95,93,126,143,102,26,1,0,0,14,11,2,3,14,27,30,64,89,112,143,158,130,105,113,118,87,62,49,40,47,58,60,38,19,17,32,37,41,64,87,98,105,134,143,124,141,116,81,64,40,58,64,87,129,175,212,227,213,187,166,166,175,172,145,130,112,116,83,27,21,33,14,0,0,1,11,78,78,18,13,1,9,40,76,90,119,169,227,222,192,171,130,55,11,14,18,21,35,38,54,86,76,39,46,77,64,50,45,47,47,
43,58,116,129,95,80,66,55,56,51,80,141,138,135,130,118,124,108,89,96,129,122,62,4,0,0,0,0,4,6,8,26,52,51,78,118,143,177,193,166,141,132,137,135,92,62,62,70,64,62,55,19,6,6,13,15,27,56,64,77,98,110,113,132,99,62,41,23,43,49,96,149,166,192,208,193,169,159,169,182,182,180,162,138,146,81,15,5,27,19,0,0,0,1,44,89,51,24,1,11,29,70,148,187,223,242,232,202,177,141,73,7,1,10,12,39,59,71,110,110,77,80,108,93,83,73,73,77,
80,98,137,121,87,78,87,73,66,80,104,141,169,166,140,118,96,95,110,129,134,108,45,1,0,0,0,0,5,23,30,55,98,102,119,135,151,180,187,185,185,140,107,108,87,39,26,38,59,80,68,37,9,0,0,0,0,17,35,54,68,80,101,112,93,74,26,11,41,63,107,118,115,140,153,148,140,154,189,213,195,169,159,164,158,105,50,20,27,24,0,0,0,2,27,64,87,68,28,41,49,87,161,202,242,205,159,158,171,162,99,38,8,14,23,46,86,98,115,124,113,124,161,159,132,101,92,95,
74,101,143,130,113,119,118,89,93,96,101,134,182,184,149,112,92,78,73,87,105,78,46,15,2,3,10,5,11,23,36,58,96,126,145,153,172,182,182,193,192,121,60,43,46,29,13,21,45,63,71,54,36,2,0,0,0,4,15,36,54,56,70,81,84,86,37,19,51,83,110,108,101,99,89,108,105,122,180,195,184,161,153,177,175,119,67,50,39,21,1,0,0,0,9,21,49,101,107,104,99,135,179,190,210,158,96,81,102,151,116,67,44,38,51,67,98,93,105,146,169,192,217,177,121,96,95,74,
76,115,138,127,129,118,102,92,102,116,130,148,187,203,169,134,101,63,56,77,95,70,40,22,19,16,11,15,30,26,32,60,59,83,119,153,164,167,172,172,166,104,49,26,15,13,5,16,43,44,55,50,45,35,7,3,0,3,5,33,55,30,39,45,70,96,62,46,68,99,121,102,81,55,47,84,84,107,141,127,140,153,149,171,190,164,102,71,59,34,8,3,0,0,5,11,18,51,110,137,148,171,200,195,174,135,84,50,60,110,108,105,90,66,66,58,95,119,135,180,230,230,200,148,112,93,77,62,
76,95,107,112,124,112,92,83,77,80,105,138,167,187,187,171,118,83,84,90,96,81,35,7,2,3,4,23,66,35,18,34,28,58,108,140,148,154,164,158,143,73,23,11,12,13,4,4,17,28,36,37,21,39,45,11,0,2,6,12,22,26,32,26,62,99,78,77,77,95,101,84,70,44,41,66,63,81,89,76,95,129,159,179,190,198,172,134,99,45,21,11,1,4,4,6,20,46,89,134,162,184,195,213,159,105,87,73,54,67,71,110,89,51,55,47,99,145,162,217,242,222,197,146,119,92,66,51,
81,95,115,129,140,124,83,62,73,63,76,113,141,158,189,195,154,132,110,86,93,89,47,16,6,7,11,18,44,28,17,35,51,84,112,149,158,159,172,167,156,98,41,23,29,16,0,0,0,16,18,36,34,21,33,15,0,0,0,0,2,33,43,33,56,63,76,90,70,74,62,33,25,20,15,19,39,60,59,67,96,137,187,193,179,172,167,162,113,59,39,28,19,11,3,2,13,50,93,130,172,203,218,217,161,101,60,43,26,22,36,64,58,54,67,70,126,141,159,222,215,185,153,118,112,104,76,74,
98,96,113,141,153,124,83,62,70,77,86,93,105,134,169,185,172,175,141,112,104,81,68,49,24,10,6,4,9,18,20,28,30,28,49,127,175,192,200,174,159,122,78,51,36,12,0,0,1,14,7,19,54,35,18,18,11,4,0,0,0,4,20,47,50,41,59,60,39,56,37,4,0,0,3,1,13,41,60,90,132,167,197,197,166,137,124,126,84,51,63,56,46,24,19,28,52,87,124,148,180,217,242,223,167,102,39,10,4,4,11,24,29,55,92,108,149,140,166,187,149,126,90,63,73,87,96,101,
101,86,84,101,119,126,119,95,76,77,68,58,58,86,121,146,159,166,154,135,98,74,76,67,47,25,8,2,0,0,2,0,0,3,25,102,180,233,222,180,161,116,86,70,43,13,1,0,3,19,15,16,50,47,25,18,17,6,0,0,0,0,1,32,62,60,51,27,26,35,29,14,0,0,6,5,13,44,77,127,159,169,172,166,138,108,98,98,67,40,41,39,74,71,74,104,119,143,148,151,172,213,242,215,143,73,26,5,0,0,11,8,4,27,76,137,158,145,169,153,115,95,70,50,28,22,67,96,
110,92,67,66,74,74,68,70,63,62,64,60,60,67,76,96,146,149,140,141,99,78,70,52,30,13,7,5,0,0,0,0,0,8,33,98,180,238,213,169,132,92,83,87,67,40,5,0,8,23,38,49,62,36,13,1,0,0,7,0,0,3,6,25,59,74,44,19,20,27,35,52,33,15,37,49,59,84,121,164,156,138,130,127,127,112,107,104,66,46,38,36,78,98,102,108,92,115,105,110,149,200,228,212,145,64,20,6,0,0,11,8,1,11,54,141,156,141,171,145,104,87,77,39,7,6,40,86,
105,98,71,54,43,37,28,37,66,59,41,39,46,60,50,49,108,137,127,146,104,80,73,38,5,0,0,5,0,0,0,0,9,45,80,132,193,222,190,145,110,67,49,64,66,80,54,35,29,28,59,60,56,18,0,0,0,0,2,2,0,4,21,33,37,73,83,40,34,40,34,36,67,89,102,121,129,121,135,161,146,138,122,119,146,137,118,107,63,58,70,55,60,77,83,67,62,87,73,87,138,193,197,179,154,116,77,47,19,3,0,5,7,15,62,135,145,143,184,154,115,102,90,50,20,23,50,77,
77,77,66,58,44,27,15,24,64,63,46,29,26,46,40,29,73,107,135,154,115,101,90,39,6,0,0,2,1,10,10,14,45,71,116,171,205,210,174,129,78,41,33,24,26,54,84,81,56,58,66,34,25,5,0,0,0,0,0,0,11,26,36,40,36,49,87,78,60,58,59,50,50,84,77,70,86,86,90,107,127,146,158,153,149,132,105,89,74,76,66,43,46,59,63,55,66,87,78,102,161,195,158,115,116,130,121,68,11,0,0,3,8,27,81,129,137,172,215,200,162,118,71,27,9,19,51,70,
55,44,27,29,40,24,7,17,68,59,47,15,2,20,37,23,49,78,145,179,146,132,98,46,15,4,8,8,4,15,19,28,30,36,71,141,193,195,164,129,60,19,12,8,13,38,66,71,70,64,49,11,7,1,0,0,0,0,0,0,27,59,67,70,36,25,54,89,74,40,40,54,44,43,28,30,51,44,45,63,101,135,172,175,135,137,134,107,95,74,40,43,32,49,60,58,81,107,110,137,192,166,83,50,76,84,86,70,13,6,4,0,10,45,92,124,145,195,238,217,149,89,43,7,0,8,21,41,
37,25,11,14,27,22,8,25,81,45,15,0,0,8,26,14,32,73,137,189,175,138,108,77,50,36,39,15,15,23,20,28,20,29,70,130,189,202,175,143,86,43,27,23,33,41,45,55,54,49,34,15,7,2,0,0,0,0,2,8,39,60,105,74,24,24,36,45,77,64,34,33,36,30,26,38,49,22,9,20,59,98,149,189,166,158,162,143,107,60,46,58,32,59,80,73,95,129,148,167,169,118,47,24,47,58,58,63,43,38,15,10,34,56,76,95,115,154,200,193,135,87,50,18,11,11,7,28,
54,52,44,36,22,12,15,49,76,24,4,0,0,11,7,6,20,66,122,159,187,164,138,124,113,105,87,44,46,30,27,44,29,54,107,156,197,223,203,159,98,52,32,25,15,6,21,32,35,50,25,15,15,4,0,0,1,20,29,44,66,64,89,67,25,27,29,30,62,90,67,39,25,27,25,26,27,5,0,0,30,58,108,182,190,159,122,126,122,95,86,98,71,89,115,104,116,140,180,185,124,80,52,26,23,45,58,66,71,83,68,62,54,47,62,71,86,126,172,184,167,138,90,58,50,21,19,50,
95,71,34,8,1,7,30,55,59,23,1,0,0,4,1,7,14,70,132,132,175,200,169,159,140,143,134,96,87,56,51,76,64,101,135,172,203,222,212,164,87,38,24,14,1,0,19,19,25,54,37,34,38,25,11,13,37,68,68,83,83,55,46,41,49,40,50,73,95,95,96,71,36,27,46,47,36,15,4,0,17,51,84,143,177,162,107,96,140,159,143,143,143,140,146,141,145,156,202,200,119,62,41,37,15,18,38,34,37,52,89,84,63,66,76,87,99,113,140,148,143,164,138,105,104,73,58,77,
76,37,12,3,4,15,41,55,38,10,2,3,4,8,7,18,24,90,145,130,141,185,207,184,148,141,129,102,87,68,80,112,122,141,124,135,166,187,205,187,122,67,47,29,19,17,29,34,32,38,28,51,63,62,66,64,95,110,108,92,74,52,24,16,43,66,67,87,105,105,98,104,86,63,83,93,68,51,37,26,29,50,71,99,143,171,121,101,124,156,184,179,182,175,164,171,185,190,222,215,146,84,51,62,43,19,23,15,5,10,49,78,63,81,99,101,96,98,122,132,122,151,166,159,161,140,102,84,
95,59,32,19,13,11,36,59,39,23,19,17,21,16,27,35,55,104,119,104,105,137,210,212,167,129,101,105,87,63,86,138,158,132,89,90,130,158,189,215,197,138,99,87,83,81,60,55,52,40,33,43,40,50,87,113,126,124,118,118,93,59,21,13,33,64,80,83,98,121,110,108,132,140,138,127,107,90,52,56,62,46,43,87,124,149,134,108,95,121,159,164,187,193,185,207,217,232,242,218,162,116,87,73,56,38,25,13,11,14,41,70,64,83,80,77,87,110,145,162,140,154,161,154,172,177,159,121,
134,83,45,36,26,26,51,70,63,44,27,27,19,22,38,27,40,78,76,78,96,126,177,210,184,121,96,121,87,62,98,140,171,124,87,96,119,146,180,200,215,189,162,146,105,102,83,80,81,43,40,44,50,54,71,105,134,130,105,89,96,93,54,43,70,99,116,129,138,148,162,154,161,180,180,171,159,141,93,87,93,68,43,67,112,105,105,83,76,81,90,122,149,187,207,223,205,205,227,218,175,146,116,73,33,21,29,12,5,23,49,70,78,92,62,46,59,83,86,116,143,145,143,137,149,174,189,164,
159,121,90,83,71,56,56,66,60,34,34,37,16,7,11,2,13,41,43,70,89,137,169,198,187,129,113,119,92,84,107,132,179,146,122,129,119,143,164,180,190,197,195,169,107,104,110,116,124,63,63,43,45,76,73,95,135,138,86,58,76,118,124,113,113,127,143,159,190,192,198,202,180,174,172,174,175,154,119,118,95,86,81,63,74,66,51,50,51,40,38,68,116,161,217,218,179,182,208,197,184,179,148,67,11,4,15,28,22,40,78,92,74,77,63,39,32,47,46,51,104,137,137,127,122,132,161,161,
153,148,118,95,77,60,52,49,40,27,30,37,8,0,0,0,2,3,9,43,55,101,159,205,190,140,122,118,121,127,118,137,180,156,141,122,110,145,148,166,184,185,202,212,172,154,161,161,159,118,108,67,56,89,78,105,153,158,104,74,89,127,148,143,146,171,192,208,242,242,218,218,210,179,153,149,164,126,87,84,50,34,52,62,54,45,34,28,24,12,7,27,83,130,207,213,174,182,172,134,129,159,159,74,19,9,14,29,55,62,70,78,59,46,47,54,25,23,49,58,96,116,122,99,74,84,119,151,
95,89,87,68,58,52,47,43,32,19,12,25,11,0,0,0,0,0,0,13,26,77,146,207,200,156,126,124,113,108,121,158,189,159,124,80,71,105,102,129,159,169,193,222,225,228,213,198,205,189,162,119,102,116,115,137,158,169,130,107,129,143,161,184,198,210,210,215,237,223,203,202,215,205,159,138,143,110,63,58,41,19,22,36,43,20,6,3,6,0,0,15,59,104,184,185,172,159,108,66,63,105,154,112,41,8,4,25,50,55,49,49,43,34,28,43,40,29,60,98,105,92,108,95,52,51,80,110,
64,58,45,36,36,27,23,24,32,19,13,26,27,1,0,0,0,0,0,6,27,90,134,179,195,180,149,124,101,78,93,148,200,169,110,81,68,78,74,102,138,166,180,187,205,237,228,197,187,190,175,153,146,146,138,129,149,189,175,159,177,159,162,175,182,198,200,193,179,166,156,145,154,184,175,145,116,104,93,64,43,23,17,18,30,19,0,0,0,0,0,19,40,101,161,137,149,119,70,45,49,90,135,112,33,2,4,27,35,56,78,62,37,21,19,15,30,56,77,70,67,47,63,101,63,36,43,70,
55,36,13,18,41,35,37,44,43,35,16,10,19,8,0,0,0,0,0,4,37,101,127,158,167,190,169,102,84,90,92,126,189,187,105,67,74,71,74,99,132,145,129,129,141,161,189,175,146,138,132,134,129,113,119,119,146,197,208,202,195,158,146,141,154,185,175,143,113,93,105,112,116,148,158,146,115,105,110,76,44,39,36,36,38,37,2,0,0,0,4,26,52,124,130,102,122,92,59,38,46,81,101,86,45,15,18,20,7,17,68,83,37,9,15,9,13,45,34,17,25,37,33,63,77,41,33,51,
38,29,26,33,38,34,32,47,59,49,12,3,16,22,3,1,0,0,0,16,71,115,148,158,146,175,162,95,63,78,108,116,159,190,140,83,84,98,87,76,92,116,92,83,90,83,99,146,145,124,98,96,104,86,104,127,148,177,203,205,197,175,159,143,158,169,122,90,66,55,77,81,105,141,113,105,110,90,80,87,78,76,76,77,62,49,19,0,0,1,15,38,83,121,93,90,99,56,50,51,52,89,86,63,47,12,8,7,2,7,38,49,33,12,14,16,17,36,28,25,28,35,26,28,54,45,29,29,
19,30,41,30,20,13,9,19,64,74,27,11,21,27,17,3,0,2,26,73,127,146,177,161,143,158,166,132,102,101,121,137,153,175,167,129,127,116,81,47,51,86,90,62,41,54,60,108,151,126,67,58,47,39,68,112,156,177,187,172,164,166,161,154,162,146,98,76,55,58,51,40,93,115,77,80,113,115,90,99,122,113,92,66,46,39,38,35,22,15,22,41,71,74,59,81,78,32,26,43,74,118,90,49,20,1,1,0,0,7,33,19,7,11,16,23,32,43,49,47,45,36,23,21,35,16,7,12,
10,15,23,15,18,21,26,44,66,74,49,27,28,24,16,7,10,33,64,110,140,158,195,182,154,158,158,146,141,145,148,161,182,189,172,149,134,80,51,43,39,35,56,59,18,27,66,99,138,126,70,45,27,18,27,63,132,174,145,116,101,113,145,153,161,140,107,77,56,46,18,23,78,110,92,101,130,154,134,130,151,154,135,105,83,62,49,46,44,35,46,70,89,67,54,70,55,19,21,34,68,127,99,43,11,0,0,0,0,0,17,8,0,1,9,16,30,32,27,30,43,49,36,33,34,5,1,3,
11,15,27,37,45,45,40,39,40,47,50,46,39,27,22,18,32,45,74,134,166,190,212,192,164,138,105,92,89,108,140,140,171,182,146,122,132,92,39,23,17,7,11,40,30,19,23,37,78,101,77,44,20,21,28,43,105,153,116,90,78,89,129,154,145,115,89,62,34,21,5,19,68,83,64,68,112,167,189,195,190,180,175,137,77,56,60,55,62,86,95,90,77,50,43,44,37,19,17,18,63,112,84,33,4,0,0,0,0,0,12,9,0,0,0,5,19,11,11,22,29,37,34,36,41,9,4,4,
20,21,27,44,86,108,83,49,45,51,58,54,33,17,24,15,37,63,95,164,210,217,195,177,143,98,70,58,49,66,108,108,137,151,122,102,108,116,76,32,10,1,0,6,8,0,0,2,37,51,78,54,13,20,38,52,108,121,90,58,35,33,62,121,143,119,90,34,10,7,0,14,67,55,35,51,110,166,213,233,202,171,148,124,87,59,66,84,96,115,124,98,51,24,24,34,46,38,15,12,66,84,54,19,0,0,0,0,0,1,23,24,5,0,0,0,14,5,11,10,6,15,33,38,41,27,17,19,
37,34,33,58,89,74,66,55,39,33,35,33,34,35,39,18,40,98,129,172,197,202,182,156,122,76,43,35,34,55,98,115,145,151,127,104,87,87,87,62,20,4,1,2,0,0,0,1,23,37,89,83,39,39,33,64,104,92,58,22,10,9,27,84,145,153,99,20,6,1,0,21,44,28,32,54,96,137,198,227,207,154,126,108,108,92,71,67,80,113,134,115,67,34,36,55,81,67,27,39,80,43,28,8,4,1,0,0,0,9,45,15,0,0,0,0,7,11,19,13,16,27,38,40,35,45,49,45,
66,64,71,86,37,9,13,15,15,18,21,25,39,67,78,58,70,130,143,140,158,171,151,112,87,63,29,22,36,64,89,115,138,129,95,60,50,58,59,64,54,24,13,0,0,0,0,11,40,54,71,93,86,43,27,67,66,67,58,24,13,10,19,62,127,171,121,44,18,1,3,27,6,4,13,30,58,83,158,210,215,166,115,73,55,68,81,86,95,115,108,113,89,51,63,78,99,92,73,96,80,20,11,6,6,8,0,0,0,23,45,1,0,0,0,1,9,25,35,40,37,29,40,49,39,60,66,54,
101,101,96,54,9,3,9,15,19,17,13,5,8,43,80,108,132,127,101,105,134,116,89,70,45,40,39,41,58,84,90,95,113,105,81,58,47,50,27,21,37,46,25,2,0,1,14,36,32,29,46,76,86,43,35,51,37,49,54,33,32,23,27,67,126,162,134,74,27,11,18,14,0,1,0,5,37,46,108,166,200,189,143,90,52,50,71,78,77,73,84,122,110,90,99,80,92,115,134,145,81,23,4,2,3,5,6,11,13,38,41,5,0,0,1,17,34,38,41,45,44,38,56,71,68,93,80,76,
127,89,63,45,20,9,4,5,7,8,7,1,2,32,46,89,116,76,62,73,84,62,49,47,18,6,13,50,92,110,115,115,119,113,92,71,67,58,39,19,19,32,39,26,18,27,51,35,5,11,27,49,66,46,40,43,44,39,28,19,36,58,58,76,115,154,118,77,43,30,29,17,4,3,0,1,23,32,62,104,167,203,190,143,86,71,70,49,51,71,107,112,92,112,124,101,99,116,145,134,83,44,8,0,0,6,26,41,41,58,41,19,14,21,35,41,58,54,50,58,52,38,43,67,83,101,99,112,
122,78,52,51,27,4,0,7,15,18,22,15,15,40,45,66,64,54,50,44,47,40,32,30,9,4,14,52,98,93,98,113,115,110,76,58,73,62,56,47,36,27,33,28,25,34,52,41,11,20,20,43,52,24,32,35,50,41,17,8,27,67,59,63,108,159,127,101,68,37,34,16,10,12,13,14,18,23,37,63,118,166,187,175,116,99,81,45,62,95,104,77,73,101,115,122,126,129,135,105,55,46,20,3,7,27,50,66,52,58,39,25,32,40,50,43,58,78,66,50,37,28,28,41,66,84,98,113
}


-------------------------------------------------------------------------------
--- CONSTANTS
-------------------------------------------------------------------------------

local sin = math.sin
local cos = math.cos
local atan = math.atan
local abs = math.abs
local sqrt = math.sqrt
local table_insert = table.insert
local table_sort = table.sort
local min = math.min
local random = math.random

-------------------------------------------------------------------------------
--- VARIABLES
-------------------------------------------------------------------------------
local _timer = core_timer_create()
local _localTimer = {}

local _tracklist = core_sound_create_tracklist({
    { bank = 0, track = 1, pattern = -1, row = -1, lastPattern = -1, lastRow = -1 },
    { bank = 0, track = 0, pattern = -1, row = -1, lastPattern = -1, lastRow = -1 },
    { bank = 0, track = 2, pattern = -1, row = -1, lastPattern = -1, lastRow = -1 },
    { bank = 0, track = 3, pattern = -1, row = -1, lastPattern = -1, lastRow = -1 },
    { bank = 1, track = 0, pattern = -1, row = -1, lastPattern = -1, lastRow = -1 },
})
local _soundState = core_sound_create_soundstate()
local _musicStarted = false
local _syncKick4 = 0
local _syncSnare4 = 0
local _syncSnare8 = 0
local _syncKickSnare4 = 0

local _imgFont

local _matProj = core_mat4_projection()

--- STARTUP_DELAY VARIABLES
-- Shared with screen fader..

--- SCREEN_FADER VARIABLES
_screenTic = nil
_palTic = nil

--- PRESENTS VARIABLES
-- Uses palette "_imgNeobyte.palette"
_presentsFadeIndexes = { 0, 1, 0, 0, 1, 1, 0, 1, 1, 1, 2, 1, 1, 2, 2, 1, 2, 2, 2, 5, 2, 2, 5, 5, 2, 5, 5, 5, 6, 5, 5, 6, 6, 5, 6, 6, 6, 7, 6, 6, 7, 7, 6, 7, 7, 7, 8, 7, 7, 8, 8, 7, 8, 8, 8, 9, 8, 8, 9, 9, 8, 9, 9, 9, 10, 9, 9, 10, 10, 9, 10, 10, 10, 11, 10, 10, 11, 11, 10, 11 }

--- TITLE VARIABLES
_imgPrelude = nil
_imgNeobyte = nil

_palTitleHighlightOnly = nil
_titleGridY = -200 -- Start y pos when moving grid vertically.

_pressStartBlinkerEnabled = true

--- BLOB VARIABLES
_blobSyncSnare = 0
_imgBlobSprites = {}
_blobShapes = {}
_blobList = {}
_blobListIdx = 1
_modelMatBlobs = core_mat4_identity()

-- 0: Black, 1-4: Blob colors. 5-7: Grid colors.
_palBlob = { 0, 0, 0, 20, 52, 100, 255, 255, 255, 36, 159, 222, 188, 74, 155, 9, 10, 20, 23, 32, 56, 37, 58, 94, 255, 0, 255, 255, 0, 255, 255, 0, 255, 255, 0, 255, 255, 0, 255, 255, 0, 255, 255, 0, 255, 255, 0, 255 }
_palBlobBright = {
    0, 0, 0, 20, 52, 100, 255, 255, 255, 36, 159, 222, 188, 74, 155, 89, 97, 186, 124, 148, 213, 184, 210, 255, 255, 0, 255, 255, 0, 255, 255, 0, 255, 255, 0, 255, 255, 0, 255, 255, 0, 255, 255, 0, 255, 255, 0, 255
}

--- CREDITS VARIABLES
-- Nothing.

--- TANSIT2LISSA VARIABLES
_coverLines = {}

-- 0: Black (transparent), 1-9: Gradient, 10-14: unused.
_palTransit2LissaFont = { 0, 0, 0, 36, 34, 52, 64, 51, 83, 121, 58, 128, 188, 74, 155, 232, 106, 115, 245, 160, 151, 250, 214, 184, 254, 243, 192, 255, 255, 255, 255, 0, 0, 255, 0, 0, 255, 0, 0, 255, 0, 0, 255, 0, 0, 255, 0, 0 }

_lissaLogoFadeIndexes1 = { 0, 1, 0, 0, 1, 1, 0, 1, 1, 1, 3, 1, 1, 3, 3, 1, 3, 3, 3, 5, 3, 3, 5, 5, 3, 5, 5, 5, 7, 5, 7, 7, 5, 5, 7, 7, 7, 9, 7, 7, 9, 9, 7, 9, 9, 9, 7, 9, 9, 7, 7, 9, 7, 7, 7, 5, 7, 7, 5, 5, 7, 5, 5, 5, 4, 5, 5, 4, 4, 5, 4 }
_lissaLogoFadeIndexes2 = { 0, 1, 0, 0, 1, 1, 0, 1, 1, 1, 3, 1, 1, 3, 3, 1, 3, 3, 3, 5, 3, 3, 5, 5, 3, 5, 5, 5, 7, 5, 7, 7, 5, 5, 7, 7, 7, 9, 7, 7, 9, 9, 7, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9 }

_lissaLogoChars = {
    {
        v = 's',
        x = 89,
        colIdx = { 0 }
    },
    {
        v = 'p',
        x = 97,
        colIdx = { 0 }
    },
    {
        v = 'e',
        x = 105,
        colIdx = { 0 }
    },
    {
        v = 'c',
        x = 113,
        colIdx = { 0 }
    },
    {
        v = 't',
        x = 120,
        colIdx = { 0 }
    },
    {
        v = 'r',
        x = 128,
        colIdx = { 0 }
    },
    {
        v = 'o',
        x = 136,
        colIdx = { 0 }
    },
    {
        v = 'x',
        x = 144,
        colIdx = { 0 }
    },
}

--- LISSA VARIABLES
-- 0: Black (Transparent), 1: Black (Opaque), 2-9: Colors.
_imgLissaBG = nil

_lissaPoints = {}
_modelMatLissa = core_mat4_identity()

_lissaLinePoints = {}
_modelMatLissaLines = core_mat4_identity()
_numLissaLines = 8
_lissaLineDist = 1
_lissaLinesTotalDist = _numLissaLines * _lissaLineDist

_palLissa = { 0, 0, 0, 20, 52, 100, 255, 255, 255, 36, 159, 222, 188, 74, 155, 37, 58, 94, 90, 65, 113, 144, 73, 132, 198, 81, 151, 210, 133, 178, 222, 185, 205, 235, 237, 233, 255, 0, 255, 255, 0, 255, 255, 0, 255, 255, 0, 255 }

--- TRANSIT2SHADOW VARIABLES
-- None.

--- SHADOW VARIABLES
_imgBricks = nil

_epsilon = 0.01
_segments = {}
_rays = {}
_uniqueRays = {}
_uniqueRaysCount = 0

_shadowClips = {
    -- 3 clips (15fps)
    -- { xs = 0, ys = 0,  xe = 239, ye = 45 },
    -- { xs = 0, ys = 46, xe = 239, ye = 90 },
    -- { xs = 0, ys = 90, xe = 239, ye = 135 },

    -- 2 clips (30 fps)
    { xs = 0, ys = 0,  xe = 239, ye = 68 },
    { xs = 0, ys = 69, xe = 239, ye = 135 },
}
_shadowClipsIdx = 1

_lightMap = {}
_lightMapSize = 256

_shadowCastEnabled = {
    true, -- 1
    true, -- 2
    true, -- 3
    true, -- 4
    true, -- 5
    true, -- 6
    true, -- 7
    true, -- 8
    true, -- 9
}

_shadowCastPosBase = {
    nil,          -- 1 BORDER
    { 0,   0 },   -- 2 CUBE (must match _cubeIdx)
    { 58,  69 },  -- 3 BRICK 1
    { 50,  23 },  -- 4 BRICK 2
    { 158, 35 },  -- 5 BRICK 3
    { 108, 12 },  -- 6 BRICK 4
    { 175, 102 }, -- 7 BRICK 5
    { 131, 91 },  -- 8 BRICK 6 (multiple)
    { 2,   102 }, -- 9 BRICK 7
}
_shadowCastPos = core_util_deep_copy(_shadowCastPosBase)

_shadowCubeIdx = 2               -- Index of cube in _polysTemplate.
_shadowCubeColors = { 9, 14, 2 } -- Colors of the cube lines. First one is the non-glitching one.

_polys = nil
_polysTemplate = {
    {
        -- 1: Border. Lightmap is 256 so give space for additional 128.
        { -128,             -128 },
        { -128,             CORE_HEIGHT + 128 },
        { CORE_WIDTH + 128, CORE_HEIGHT + 128 },
        { CORE_WIDTH + 128, -128 }
    },
    {
        -- 2: CUBE (Dummy)
    },
    {
        -- 3: BRICK 1
        { 0,  0 },
        { 0,  10 },
        { 25, 10 },
        { 25, 0 },
    },
    {
        -- 4: BRICK 2
        { 0,  0 },
        { 0,  11 },
        { 18, 11 },
        { 18, 0 },
    },
    {
        -- 5: BRICK 3
        { 0,  0 },
        { 0,  10 },
        { 17, 10 },
        { 17, 0 },
    },
    {
        -- 6: BRICK 4
        { 0,  0 },
        { 0,  10 },
        { 18, 10 },
        { 18, 0 },
    },
    {
        -- 7: BRICK 5
        { 0,  0 },
        { 0,  10 },
        { 18, 10 },
        { 18, 0 },
    },
    {
        -- 8: BRICK 6
        { 0,  0 },
        { 0,  10 },
        { 7,  10 },
        { 7,  22 },
        { -3, 22 },
        { -3, 34 },
        { 15, 34 },
        { 15, 21 },
        { 25, 21 },
        { 25, 11 },
        { 18, 11 },
        { 18, 0 },
    },
    {
        -- 9: BRICK 7
        { 0,  0 },
        { 0,  10 },
        { 18, 10 },
        { 18, 0 },
    },
}

_modelMatShadowCube = core_mat4_identity()
_shadowCubePos = { 0, 0 }
_shadowCubeVerts = {
    { -1, -1, -1 }, { -1, 1, -1 }, { 1, -1, -1 }, { 1, 1, -1 }, --
    { -1, -1, 1 }, { -1, 1, 1 }, { 1, -1, 1 }, { 1, 1, 1 }      --
}

-- 0-9: Lightmap dither.
-- 15: Lightmap mask.
-- 2, 9, 14: Shadow cube.
_palShadow = {
    0, 0, 0, 0, 47, 86, 13, 47, 101, 31, 50, 116, 52, 57, 131, 109, 82, 142, 166, 107, 153, 203, 170, 178, 241, 234, 203, 230, 230, 230, 255, 0, 0, 255, 0, 0, 255, 0, 0, 13, 47, 101, 188, 74, 155, 0, 255, 0,
}

--- HAVE2RUN VARIABLES
_runTexts = {
    "some days you", "love", "to run",
    "some days you", "hate", "to run",
    "every day you", "have", "to run",
}

_runTextFadeColors = { 0, 1, 0, 0, 1, 1, 0, 1, 1, 1, 5, 1, 1, 5, 5, 1, 5, 5, 5, 4, 5, 5, 4, 4, 5, 4, 4, 4, 6, 4, 4, 6, 6, 4, 6, 6, 6, 7, 6, 6, 7, 7, 6, 7, 7, 7, 8, 7, 7, 8, 8, 7, 8, 8, 8, 9, 8, 8, 9, 9, 8, 9, 9, 9, 2, 9, 9, 2, 2, 9, 2 }

--- RUNNER VARIABLES
_runnerSyncSnare = 0
_runnerGreetsBGPosX = 280
_runnerGreetsFGPosX = 280
_runnerBGOffX = -100
_runnerSunOffX = 0

_imgSkylineFront = nil
_imgSkylineBack = nil
_imgSkylineBackShort = nil
_imgSkylineTrees = nil

_imgRunnerSheet = nil
_runnerSprites = {}

_imgDoggySheet = nil
_imgDoggySprites = {}

_imgHeli = nil

_runnerBulk = {}

_txtGreets1 =
"ABYSS ALCATRAZ CALODOX FARBRAUSCH MELON OFTENHIDE SKE SPREADPOINT TEK TELETYPE TPOLM VANTAGE"
_txtGreets2 =
"AGIMA ANDROMEDA ARTSTATE DESIRE ECHTZEIT FROZAK ISTARI NGC SMFX SUBURBAN"

-- 0: Black. 1-4: Runner sprites. 5-9: Gradient for font fading. 10: Clear color. 11: Sun. 12: Skyline background layer color. 13-14: Letter front and top. 15: Skyline windows.
_palRunner = { 0, 0, 0, 20, 52, 100, 255, 255, 255, 36, 159, 222, 188, 74, 155, 121, 58, 128, 232, 106, 115, 245, 160, 151, 250, 214, 184, 254, 243, 192, 0, 255, 0, 190, 83, 149, 64, 94, 153, 0, 255, 0, 0, 255, 0, 255, 0, 255 }

-- Pink/blue BG.
_gradBGPink =
{ 8, 8, 17, 0, 17, 33, 8, 25, 33, 0, 17, 33, 8, 25, 50, 8, 25, 41, 17, 33, 58, 8, 25, 50, 17, 33, 58, 17, 33, 66, 17, 33, 58, 17, 41, 66, 25, 41, 74, 17, 41, 66, 25, 41, 74, 25, 50, 74, 25, 41, 74, 25, 50, 91, 33, 50, 91, 25, 50, 91, 33, 58, 91, 41, 58, 99, 33, 58, 91, 41, 66, 99, 41, 66, 107, 41, 66, 99, 41, 66, 116, 50, 74, 116, 41, 66, 116, 50, 74, 124, 50, 74, 124, 58, 83, 132, 50, 74, 124, 50, 91, 132, 58, 83, 132, 58, 91, 140, 50, 91, 140, 74, 91, 140, 66, 91, 140, 83, 91, 140, 74, 91, 140, 99, 91, 140, 91, 91, 149, 107, 91, 140, 116, 91, 149, 107, 91, 140, 124, 91, 149, 116, 91, 149, 140, 91, 149, 124, 91, 140, 140, 91, 149, 149, 91, 149, 140, 91, 149, 165, 91, 149, 157, 91, 140, 173, 91, 149, 165, 83, 149, 182, 91, 157, 173, 83, 149, 190, 91, 149, 190, 83, 149, 198, 83, 149, 215, 107, 157, 198, 83, 149, 223, 149, 190, 223, 132, 173, 231, 198, 215, 231, 173, 198, 239, 239, 239, 248, 223, 231, 8, 25, 50, 17, 33, 50, 8, 25, 50, 8, 25, 50, 8, 25, 50, 8, 25, 50, 8, 25, 50, 8, 25, 50, 8, 25, 41, 8, 25, 50, 8, 25, 41, 0, 17, 41, 8, 25, 41, 8, 25, 33, 8, 25, 33, 0, 17, 33, 8, 25, 33, 0, 17, 33, 0, 17, 33, 0, 17, 33, 0, 17, 33, 0, 17, 33, 0, 17, 25, 0, 17, 33, 0, 17, 25, 8, 17, 25, 0, 17, 25, 8, 8, 17, 8, 8, 17, 8, 8, 17, 8, 8, 17, 8, 8, 17, 8, 8, 17, 0, 8, 17, 8, 8, 17, 0, 8, 17, 0, 8, 17, 0, 8, 8, 0, 8, 17, 0, 8, 8, 0, 8, 8, 0, 0, 8, 0, 0, 8, 0, 0, 8, 0, 0, 8, 0, 0, 8, 0, 0, 8, 0, 0, 8, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0 }

_gradRunnerBGNight =
{ 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 8, 0, 0, 8, 0, 0, 8, 0, 0, 8, 0, 0, 17, 0, 0, 17, 0, 0, 17, 0, 8, 25, 0, 0, 17, 8, 8, 25, 0, 8, 25, 8, 8, 33, 8, 8, 33, 8, 8, 33, 8, 8, 41, 8, 17, 33, 8, 17, 41, 8, 17, 41, 8, 17, 50, 0, 17, 58, 8, 17, 50, 0, 25, 58, 0, 17, 58, 0, 25, 66, 0, 25, 58, 0, 25, 66, 8, 33, 74, 0, 25, 66, 8, 33, 83, 0, 25, 74, 41, 41, 91, 17, 33, 83, 91, 58, 116, 66, 50, 107, 149, 66, 124, 116, 66, 124, 198, 83, 149, 173, 74, 140, 231, 165, 190, 215, 124, 173, 239, 239, 239, 231, 206, 223, 8, 25, 50, 17, 33, 50, 8, 25, 50, 8, 25, 50, 8, 25, 50, 8, 25, 50, 8, 25, 50, 8, 25, 50, 8, 25, 41, 8, 25, 50, 8, 25, 41, 0, 17, 41, 8, 25, 41, 8, 25, 33, 8, 25, 33, 0, 17, 33, 8, 25, 33, 0, 17, 33, 0, 17, 33, 0, 17, 33, 0, 17, 33, 0, 17, 33, 0, 17, 25, 0, 17, 33, 0, 17, 25, 8, 17, 25, 0, 17, 25, 8, 8, 17, 8, 8, 17, 8, 8, 17, 8, 8, 17, 8, 8, 17, 8, 8, 17, 0, 8, 17, 8, 8, 17, 0, 8, 17, 0, 8, 17, 0, 8, 8, 0, 8, 17, 0, 8, 8, 0, 8, 8, 0, 0, 8, 0, 0, 8, 0, 0, 8, 0, 0, 8, 0, 0, 8, 0, 0, 8, 0, 0, 8, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0 }

_gradRunnerBGMix = core_util_deep_copy(_gradBGPink)

-- Letters top.
_gradLettersTopTemplate =
{ 33, 157, 223, 33, 157, 223, 33, 157, 223, 33, 157, 223, 33, 157, 223, 33, 157, 223, 33, 157, 223, 33, 157, 223, 33, 157, 223, 33, 157, 223, 33, 157, 223, 33, 157, 223, 33, 157, 223, 33, 157, 223, 33, 157, 223, 33, 157, 223, 33, 157, 223, 33, 157, 223, 33, 157, 223, 33, 157, 223, 33, 157, 223, 33, 157, 223, 33, 157, 223, 33, 157, 223, 33, 157, 223, 33, 157, 223, 33, 157, 223, 33, 157, 223, 33, 157, 223, 33, 157, 223, 33, 157, 223, 33, 157, 223, 33, 157, 223, 33, 157, 223, 33, 157, 223, 33, 157, 223, 33, 157, 223, 33, 157, 223, 33, 157, 223, 33, 157, 223, 33, 157, 223, 33, 157, 223, 33, 157, 223, 33, 157, 223, 33, 157, 223, 33, 157, 223, 33, 157, 223, 33, 157, 223, 33, 157, 223, 33, 157, 223, 33, 157, 223, 33, 157, 223, 33, 157, 223, 33, 157, 223, 33, 157, 223, 33, 157, 223, 33, 157, 223, 33, 157, 223, 33, 157, 223, 33, 157, 223, 33, 157, 223, 33, 157, 223, 33, 157, 223, 33, 157, 223, 33, 157, 223, 33, 157, 223, 33, 157, 223, 33, 157, 223, 33, 157, 223, 33, 157, 223, 33, 157, 223, 33, 157, 223, 33, 157, 223, 81, 178, 230, 33, 157, 223, 33, 157, 223, 33, 157, 223, 33, 157, 223, 33, 157, 223, 33, 157, 223, 33, 157, 223, 33, 157, 223, 33, 157, 223, 33, 157, 223, 33, 157, 223, 33, 157, 223, 33, 157, 223, 33, 157, 223, 33, 157, 223, 33, 157, 223, 33, 157, 223, 33, 157, 223, 33, 157, 223, 81, 178, 230, 33, 157, 223, 81, 178, 230, 33, 157, 223, 33, 157, 223, 33, 157, 223, 33, 157, 223, 33, 157, 223, 33, 157, 223, 33, 157, 223, 33, 157, 223, 33, 157, 223, 33, 157, 223, 33, 157, 223, 81, 178, 230, 33, 157, 223, 81, 178, 230, 33, 157, 223, 33, 157, 223, 33, 157, 223, 33, 157, 223, 33, 157, 223, 33, 157, 223, 33, 157, 223, 33, 157, 223, 33, 157, 223, 33, 157, 223, 33, 157, 223, 81, 178, 230, 33, 157, 223, 81, 178, 230, 33, 157, 223, 33, 157, 223, 33, 157, 223, 33, 157, 223, 33, 157, 223, 33, 157, 223, 33, 157, 223, 33, 157, 223, 33, 157, 223, 33, 157, 223, 33, 157, 223, 33, 157, 223 }
_gradLettersTop = {}

-- Letters front.
_gradLettersFront =
{ 0, 8, 17, 0, 8, 17, 0, 8, 17, 0, 8, 17, 0, 8, 17, 0, 8, 17, 0, 8, 17, 0, 8, 17, 0, 8, 17, 0, 8, 17, 0, 8, 17, 0, 8, 17, 0, 8, 17, 0, 8, 17, 0, 8, 17, 0, 8, 17, 0, 8, 17, 0, 8, 17, 0, 8, 17, 0, 8, 17, 0, 8, 17, 0, 8, 17, 0, 8, 17, 0, 8, 17, 0, 8, 17, 0, 8, 17, 0, 8, 17, 0, 8, 17, 0, 8, 17, 0, 8, 17, 0, 8, 17, 0, 8, 17, 0, 8, 17, 0, 8, 17, 0, 8, 17, 0, 8, 17, 0, 8, 17, 0, 8, 17, 0, 8, 17, 0, 8, 17, 0, 8, 17, 0, 8, 17, 0, 8, 17, 0, 8, 17, 0, 8, 17, 0, 8, 17, 0, 8, 17, 0, 8, 17, 0, 8, 17, 0, 8, 17, 0, 8, 17, 0, 8, 17, 0, 8, 17, 0, 8, 17, 0, 8, 17, 0, 8, 17, 0, 8, 17, 0, 8, 17, 0, 8, 17, 0, 8, 17, 0, 8, 17, 0, 8, 17, 0, 8, 17, 0, 8, 17, 0, 8, 17, 0, 8, 17, 0, 8, 17, 0, 8, 17, 0, 8, 17, 0, 8, 17, 0, 8, 17, 0, 8, 17, 0, 8, 17, 0, 8, 17, 0, 8, 17, 0, 8, 17, 0, 8, 17, 0, 8, 17, 0, 8, 17, 0, 8, 17, 0, 8, 17, 0, 8, 17, 0, 8, 17, 0, 8, 17, 0, 8, 17, 0, 8, 17, 0, 8, 17, 0, 8, 17, 0, 8, 17, 0, 8, 17, 0, 8, 17, 2, 17, 29, 2, 17, 29, 4, 26, 42, 2, 17, 29, 0, 8, 17, 2, 17, 29, 0, 8, 17, 0, 8, 17, 2, 17, 29, 0, 8, 17, 0, 8, 17, 0, 8, 17, 0, 8, 17, 0, 8, 17, 0, 8, 17, 0, 8, 17, 0, 8, 17, 0, 0, 0, 0, 8, 17, 0, 0, 0, 0, 8, 17, 0, 2, 5, 0, 5, 11, 0, 2, 5, 0, 5, 11, 0, 2, 5, 0, 2, 5, 0, 2, 5, 0, 5, 11, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 5, 11, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0 }

-- Sun gradient.
_gradSun = { 255, 255, 205, 255, 255, 205, 255, 255, 205, 255, 255, 188, 255, 239, 188, 255, 255, 188, 255, 239, 171, 255, 239, 171, 255, 239, 171, 255, 239, 154, 255, 222, 154, 255, 222, 154, 255, 222, 137, 255, 222, 137, 255, 222, 137, 255, 205, 137, 255, 222, 119, 255, 205, 119, 255, 205, 119, 255, 205, 102, 255, 205, 102, 255, 188, 85, 255, 188, 85, 255, 188, 68, 255, 188, 68, 255, 188, 68, 255, 171, 68, 255, 188, 34, 255, 171, 17, 255, 171, 51, 255, 171, 51, 255, 154, 68, 255, 154, 85, 255, 154, 85, 239, 154, 85, 239, 154, 85, 239, 154, 102, 239, 137, 102, 239, 137, 102, 222, 137, 102, 239, 137, 119, 222, 119, 119, 222, 137, 119, 222, 119, 137, 222, 119, 119, 222, 119, 137, 222, 102, 137, 205, 102, 137, 222, 102, 137, 205, 102, 137, 205, 85, 137, 205, 85, 154, 205, 85, 137, 188, 85, 154, 205, 68, 154, 188, 68, 154, 188, 68, 154, 188, 68, 154, 188, 68, 154, 188, 68, 154, 188, 68, 154, 171, 68, 154, 171, 68, 154, 171, 51, 154, 171, 68, 154, 171, 51, 154, 154, 68, 137, 171, 68, 154, 154, 51, 137, 154, 68, 154, 154, 51, 154, 154, 68, 137, 137, 51, 154, 137, 51, 137, 154, 51, 154, 137, 51, 137, 137, 51, 137, 137, 51, 154, 119, 51, 154, 119, 51, 137, 137, 51, 137, 119, 51, 137, 119, 51, 137, 119, 51, 137, 102, 51, 137, 119, 51, 137, 102, 51, 119, 102, 51, 119, 85, 51, 137, 85, 51, 119, 85, 51, 119, 68, 51, 119, 85, 51, 119, 68, 51, 119, 68, 51, 119, 51, 51, 102, 68, 51, 119, 51, 51, 102, 51, 51, 102, 34, 51, 102, 34, 51, 102, 34, 51, 102, 17, 51, 102, 0, 51, 102, 17, 51, 102, 17, 34, 85, 17, 51, 102, 17, 34, 85, 17, 51, 85, 17, 51, 85, 17, 34, 85, 17, 34, 85, 17, 34, 68, 17, 34, 85, 0, 34, 68, 17, 34, 68, 17, 34, 68, 0, 34, 68, 17, 34, 68, 0, 17, 68, 0, 34, 68, 0, 34, 68, 17, 34, 51, 0, 17, 51, 0, 34, 51, 0, 34, 51, 0, 17, 51, 0, 17, 51, 0, 17, 34, 0, 17, 51, 0, 17, 34, 0, 17, 34, 0, 17, 34, 0, 17, 34, 0, 17, 34, 0, 17, 34 }

-- Windows gradient.
_gradWindows = { 20, 82, 133, 20, 82, 133, 20, 82, 133, 20, 82, 133, 20, 84, 134, 21, 85, 135, 21, 86, 137, 21, 87, 138, 21, 88, 140, 22, 90, 141, 22, 91, 143, 22, 92, 144, 22, 93, 146, 22, 94, 147, 23, 96, 148, 23, 97, 150, 23, 98, 151, 23, 99, 153, 24, 100, 154, 24, 102, 156, 24, 103, 157, 24, 104, 159, 25, 105, 160, 25, 107, 162, 25, 108, 163, 25, 109, 164, 26, 110, 166, 26, 111, 167, 26, 113, 169, 26, 114, 170, 27, 115, 172, 27, 116, 173, 27, 117, 175, 27, 119, 176, 27, 120, 177, 28, 121, 179, 28, 122, 180, 28, 123, 182, 28, 125, 183, 29, 126, 185, 29, 127, 186, 29, 129, 188, 29, 131, 189, 30, 132, 191, 30, 133, 192, 30, 134, 193, 30, 135, 195, 31, 137, 196, 31, 138, 198, 31, 139, 199, 31, 140, 201, 32, 141, 202, 32, 143, 204, 32, 144, 205, 32, 145, 206, 33, 146, 208, 33, 148, 209, 33, 149, 211, 33, 150, 212, 33, 151, 214, 34, 152, 215, 34, 154, 217, 34, 155, 218, 34, 156, 220, 35, 157, 221, 35, 158, 222, 35, 160, 224, 35, 160, 224, 35, 160, 224, 34, 160, 224, 34, 160, 224, 34, 158, 222, 34, 157, 221, 33, 156, 219, 33, 155, 218, 33, 153, 217, 33, 152, 215, 33, 151, 214, 32, 150, 212, 32, 149, 211, 32, 147, 209, 32, 146, 208, 31, 145, 206, 31, 144, 205, 31, 142, 203, 31, 141, 202, 31, 140, 200, 30, 139, 199, 30, 137, 197, 30, 136, 196, 30, 135, 194, 29, 134, 193, 29, 133, 191, 29, 131, 190, 29, 130, 189, 28, 129, 187, 28, 127, 186, 28, 125, 184, 28, 124, 183, 28, 123, 181, 27, 122, 180, 27, 120, 178, 27, 119, 177, 27, 118, 175, 26, 117, 174, 26, 116, 172, 26, 114, 171, 26, 113, 169, 26, 112, 168, 25, 111, 166, 25, 109, 165, 25, 108, 163, 25, 107, 162, 24, 106, 161, 24, 104, 159, 24, 103, 158, 24, 102, 156, 23, 101, 155, 23, 100, 153, 23, 98, 152, 23, 97, 150, 23, 96, 149, 22, 95, 147, 22, 93, 146, 22, 92, 144, 22, 91, 143, 21, 90, 141, 21, 88, 140, 21, 87, 138, 21, 86, 137, 21, 85, 135, 20, 84, 134, 20, 82, 133, 20, 82, 133, 20, 82, 133, 20, 82, 133 }

_gradSkylineAndFloor = { 64, 94, 153, 64, 94, 153, 64, 94, 153, 64, 94, 153, 64, 94, 153, 64, 94, 153, 64, 94, 153, 64, 94, 153, 64, 94, 153, 64, 94, 153, 64, 94, 153, 64, 94, 153, 64, 94, 153, 64, 94, 153, 64, 94, 153, 64, 94, 153, 64, 94, 153, 64, 94, 153, 64, 94, 153, 64, 94, 153, 64, 94, 153, 64, 94, 153, 64, 94, 153, 64, 94, 153, 64, 94, 153, 64, 94, 153, 64, 94, 153, 64, 94, 153, 64, 94, 153, 64, 94, 153, 64, 94, 153, 64, 94, 153, 64, 94, 153, 64, 94, 153, 64, 94, 153, 64, 94, 153, 64, 94, 153, 64, 94, 153, 64, 94, 153, 64, 94, 153, 64, 94, 153, 64, 94, 153, 64, 94, 153, 64, 94, 153, 64, 94, 153, 64, 94, 153, 64, 94, 153, 64, 94, 153, 64, 94, 153, 64, 94, 153, 64, 94, 153, 64, 94, 153, 64, 94, 153, 64, 94, 153, 64, 94, 153, 64, 94, 153, 64, 94, 153, 64, 94, 153, 64, 94, 153, 64, 94, 153, 64, 94, 153, 64, 94, 153, 64, 94, 153, 64, 94, 153, 64, 94, 153, 64, 94, 153, 64, 94, 153, 64, 94, 153, 64, 94, 153, 20, 52, 100, 20, 52, 100, 20, 52, 100, 20, 52, 100, 20, 52, 100, 20, 52, 100, 20, 52, 100, 20, 52, 100, 20, 52, 100, 20, 52, 100, 20, 52, 100, 20, 52, 100, 20, 52, 100, 20, 52, 100, 20, 52, 100, 20, 52, 100, 20, 52, 100, 20, 52, 100, 20, 52, 100, 20, 52, 100, 20, 52, 100, 20, 52, 100, 20, 52, 100, 20, 52, 100, 20, 52, 100, 20, 52, 100, 20, 52, 100, 20, 52, 100, 20, 52, 100, 20, 52, 100, 20, 52, 100, 20, 52, 100, 20, 52, 100, 20, 52, 100, 20, 52, 100, 20, 52, 100, 20, 52, 100, 20, 52, 100, 20, 52, 100, 20, 52, 100, 20, 52, 100, 20, 52, 100, 20, 52, 100, 20, 52, 100, 20, 52, 100, 20, 52, 100, 20, 52, 100, 20, 52, 100, 20, 52, 100, 20, 52, 100, 20, 52, 100, 20, 52, 100, 20, 52, 100, 20, 52, 100, 20, 52, 100, 20, 52, 100, 20, 52, 100, 20, 52, 100, 20, 52, 100, 20, 52, 100, 20, 52, 100, 20, 52, 100, 20, 52, 100, 20, 52, 100, 20, 52, 100, 20, 52, 100, 20, 52, 100 }
_gradSkylineAndFloorFlash = { 63, 93, 155, 63, 93, 155, 63, 93, 155, 63, 93, 155, 63, 93, 155, 63, 93, 155, 63, 93, 155, 63, 93, 155, 63, 93, 155, 63, 93, 155, 63, 93, 155, 63, 93, 155, 63, 93, 155, 63, 93, 155, 63, 93, 155, 63, 93, 155, 63, 93, 155, 63, 93, 155, 63, 93, 155, 63, 93, 155, 63, 93, 155, 63, 93, 155, 63, 93, 155, 63, 93, 155, 63, 93, 155, 63, 93, 155, 63, 93, 155, 63, 93, 155, 63, 93, 155, 63, 93, 155, 63, 93, 155, 63, 93, 155, 63, 93, 155, 63, 93, 155, 63, 93, 155, 63, 93, 155, 63, 93, 155, 63, 93, 155, 63, 93, 155, 63, 93, 155, 63, 93, 155, 63, 93, 155, 63, 93, 155, 63, 93, 155, 63, 93, 155, 63, 93, 155, 63, 93, 155, 63, 93, 155, 63, 93, 155, 63, 93, 155, 63, 93, 155, 63, 93, 155, 63, 93, 155, 62, 93, 155, 62, 93, 155, 62, 93, 155, 62, 93, 155, 62, 93, 155, 62, 93, 155, 62, 93, 155, 62, 93, 155, 62, 93, 155, 62, 92, 155, 62, 92, 155, 62, 92, 155, 62, 92, 155, 62, 92, 155, 62, 92, 155, 62, 92, 155, 62, 92, 155, 62, 92, 155, 62, 93, 155, 48, 79, 137, 33, 66, 118, 19, 52, 101, 19, 52, 100, 19, 52, 100, 19, 52, 100, 19, 52, 100, 19, 52, 100, 19, 52, 100, 19, 52, 100, 19, 52, 100, 19, 52, 100, 19, 52, 100, 19, 52, 100, 19, 52, 100, 20, 55, 106, 21, 58, 112, 22, 61, 117, 24, 64, 123, 25, 66, 130, 26, 69, 135, 27, 72, 141, 28, 75, 147, 29, 78, 152, 30, 81, 158, 31, 84, 164, 32, 86, 169, 33, 89, 175, 35, 92, 181, 36, 95, 186, 37, 98, 192, 38, 101, 198, 39, 104, 203, 40, 106, 209, 39, 104, 204, 38, 101, 199, 37, 99, 194, 36, 97, 189, 35, 94, 184, 34, 92, 180, 33, 89, 175, 32, 87, 170, 32, 84, 165, 31, 82, 160, 30, 79, 155, 29, 77, 150, 28, 74, 145, 27, 72, 140, 26, 69, 136, 25, 67, 131, 24, 65, 125, 23, 62, 120, 22, 60, 115, 21, 57, 110, 20, 55, 105, 19, 52, 101, 19, 52, 100, 19, 52, 100, 19, 52, 100, 19, 52, 100, 19, 52, 100, 19, 52, 100, 19, 52, 100, 19, 52, 100 }
_gradSkylineAndFloorMix = core_grad_new(136)

--- TRANSIT2VOX VARIABLES
_voxKickSnare4 = 0
-- Gradient that will contain animated stripes.
_voxStripesModeBlackEnables = false
_voxGradStripes = core_grad_new(136)
_voxGradVbank0 = core_grad_new(136)

--Image palette: 0: Black, 1: unused (bdr / clear color), 2: white matching runner scene, 3-15: logo colors.
_imgSpectrox = nil
_voxLinesSpectrox = {}

-- Matches palette from Spectrox logo.
_voxTextFadeColors = { 0, 5, 0, 0, 5, 5, 0, 5, 5, 5, 9, 5, 5, 9, 9, 5, 9, 9, 9, 10, 9, 9, 10, 10, 9, 10, 10, 10, 11, 10, 10, 11, 11, 10, 11, 11, 11, 12, 11, 11, 12, 12, 11, 12, 12, 12, 13, 12, 12, 13, 13, 12, 13, 13, 13, 14, 13, 13, 14, 14, 13, 14, 14, 14, 2, 14, 14, 2, 2, 14, 2 }

--- VOX VARIABLES
_voxLogoSyncSnare8 = 0
_voxLogoFlashFactor = .3
_imgTime = nil
_voxLinesTime = {}

_voxCam = {
    pos = { x = 40, y = 40 }, -- Nice start pos for twister illusion.
    height = 1475,
    scale = 240,
    dist = 2400,
    angle = 0,
    horizon = -105,
}

_voxClipIdx = 0
_voxClips = {
    { 0,                   CORE_WIDTH_HALF },
    { CORE_WIDTH_HALF + 1, CORE_WIDTH_1 },
}

_voxMapW, _voxMapH, _voxMap = nil, nil, nil
_voxPal =
{ 6, 6, 8, 10, 21, 38, 15, 36, 69, 22, 29, 89, 43, 30, 109, 79, 39, 129, 122, 50, 148, 168, 61, 167, 188, 74, 155, 198, 95, 157, 207, 117, 163, 217, 142, 172, 226, 167, 185, 236, 195, 203, 245, 224, 226, 255, 255, 255, }
-- Used by Vox and Waterfield.
_voxGradLakes1 =
{ 0, 17, 34, 0, 17, 34, 0, 17, 34, 0, 17, 34, 0, 17, 51, 0, 17, 34, 0, 17, 51, 0, 17, 51, 17, 17, 68, 0, 17, 51, 17, 17, 68, 17, 0, 68, 17, 17, 68, 17, 17, 85, 17, 0, 68, 17, 17, 85, 17, 17, 85, 34, 17, 102, 17, 17, 85, 34, 17, 102, 51, 17, 102, 34, 17, 102, 51, 34, 119, 51, 34, 102, 68, 34, 119, 51, 34, 119, 85, 34, 119, 68, 34, 119, 85, 34, 137, 85, 34, 137, 102, 34, 137, 102, 34, 137, 119, 51, 154, 102, 51, 154, 119, 51, 154, 137, 51, 154, 119, 51, 154, 137, 51, 154, 154, 51, 154, 137, 51, 154, 154, 68, 154, 154, 68, 154, 154, 68, 154, 171, 68, 154, 154, 68, 154, 171, 68, 154, 188, 85, 154, 171, 68, 154, 188, 68, 154, 188, 85, 154, 188, 68, 154, 205, 85, 154, 205, 85, 154, 205, 85, 154, 222, 102, 154, 205, 85, 154, 222, 119, 171, 222, 102, 154, 222, 137, 171, 222, 119, 171, 222, 137, 171, 239, 154, 188, 222, 137, 171, 239, 171, 188, 239, 154, 188, 239, 171, 188, 239, 188, 205, 239, 171, 188, 239, 205, 205, 239, 188, 205, 255, 205, 205, 239, 205, 222, 239, 205, 205, 222, 205, 205, 205, 188, 222, 222, 205, 205, 188, 188, 205, 205, 188, 222, 171, 171, 222, 188, 171, 205, 154, 154, 222, 154, 171, 222, 137, 154, 222, 154, 154, 222, 119, 154, 222, 102, 137, 205, 119, 137, 222, 85, 119, 222, 102, 119, 222, 68, 119, 222, 68, 119, 222, 51, 102, 222, 68, 102, 205, 34, 85, 205, 17, 85, 205, 34, 85, 205, 17, 85, 188, 17, 85, 188, 17, 68, 171, 17, 85, 188, 17, 68, 171, 0, 68, 154, 17, 68, 171, 0, 68, 154, 17, 68, 154, 0, 51, 137, 0, 68, 137, 0, 51, 119, 0, 51, 119, 0, 51, 119, 0, 51, 102, 0, 51, 119, 0, 51, 102, 17, 51, 102, 17, 34, 102, 17, 34, 102, 17, 34, 85, 17, 34, 85, 17, 34, 85, 17, 34, 85, 17, 34, 68, 17, 34, 85, 0, 34, 68, 0, 17, 68, 0, 34, 68, 0, 17, 51, 0, 17, 68, 0, 17, 51, 0, 17, 51, 0, 17, 51, 0, 17, 34, 0, 17, 51, 0, 17, 34, 0, 17, 34, 0, 17, 34, 0, 17, 34 }
_voxGradLakes2 =
{ 10, 22, 40, 10, 21, 38, 10, 22, 40, 12, 21, 39, 10, 22, 40, 12, 21, 39, 10, 22, 40, 12, 24, 42, 10, 22, 40, 12, 24, 42, 10, 24, 43, 12, 24, 42, 10, 24, 43, 10, 24, 43, 10, 24, 43, 10, 24, 43, 10, 26, 47, 10, 24, 43, 10, 26, 47, 10, 24, 43, 10, 26, 47, 10, 24, 43, 13, 25, 47, 13, 25, 47, 13, 25, 47, 13, 25, 47, 13, 27, 50, 13, 25, 47, 13, 27, 50, 11, 25, 48, 13, 27, 50, 11, 25, 48, 13, 27, 50, 11, 28, 52, 13, 27, 50, 11, 28, 52, 14, 30, 54, 11, 28, 52, 14, 30, 54, 11, 28, 52, 14, 30, 54, 11, 28, 52, 11, 30, 55, 14, 29, 55, 11, 30, 55, 14, 29, 55, 11, 30, 55, 14, 29, 55, 11, 30, 55, 14, 31, 59, 17, 31, 58, 14, 31, 59, 14, 31, 59, 14, 31, 59, 14, 31, 59, 14, 31, 59, 14, 31, 59, 15, 33, 63, 14, 31, 59, 15, 33, 63, 12, 31, 60, 15, 33, 63, 12, 31, 60, 15, 33, 63, 12, 34, 64, 15, 33, 63, 12, 34, 64, 18, 35, 66, 12, 34, 64, 18, 35, 66, 12, 34, 64, 18, 35, 66, 12, 34, 64, 15, 35, 68, 15, 35, 68, 15, 35, 68, 15, 35, 68, 15, 35, 68, 15, 36, 69, 15, 35, 68, 15, 35, 68, 15, 35, 68, 15, 35, 68, 12, 34, 64, 15, 35, 68, 12, 34, 64, 18, 35, 66, 12, 34, 64, 18, 35, 66, 15, 33, 63, 15, 33, 63, 15, 33, 63, 15, 33, 63, 12, 31, 60, 15, 33, 63, 14, 31, 59, 14, 31, 59, 14, 31, 59, 14, 31, 59, 17, 31, 58, 14, 31, 59, 14, 29, 55, 11, 30, 55, 14, 29, 55, 11, 30, 55, 11, 28, 52, 11, 30, 55, 11, 28, 52, 14, 30, 54, 11, 28, 52, 14, 30, 54, 13, 27, 50, 13, 27, 50, 13, 27, 50, 13, 25, 47, 13, 27, 50, 13, 25, 47, 11, 25, 48, 13, 25, 47, 11, 25, 48, 10, 24, 43, 10, 26, 47, 10, 24, 43, 10, 26, 47, 10, 24, 43, 10, 26, 47, 10, 24, 43, 10, 22, 40, 10, 24, 43, 10, 22, 40, 12, 24, 42, 10, 22, 40, 12, 24, 42, 12, 21, 39, 10, 22, 40, 10, 21, 38 }

_voxGradBlack = core_grad_new(136)

-- Gradient that is mixed and rendered in vbank1.
_voxGradVbank1 = {}

--- WATERFIELD VARIABLES
_waterCubeBubblesEmitter = nil
_waterBGPosY = 0
_waterParticles = {}
_waterParticlesTransformed = {}
_waterParticlesDim = 10
_waterParticlesDim2 = _waterParticlesDim * 2
_matModelWaterParticles = core_mat4_identity()

_greets = {
    "a. winston",
    "aghnar",
    "bus error",
    "crossbone",
    "depeche",
    "dipswitch",
    "ferris",
    "gaspode",
    "gigabates",
    "grip",
    "hammerfist",
    "hoffman",
    "jade",
    "maze",
    "mickaleus",
    "motion",
    "mudlord",
    "mystra",
    "ozan",
    "ps",
    "ramon b5",
    "rez",
    "rog",
    "rp",
    "seffren",
    "shana",
    "slaze",
    "soundy",
    "spkr",
    "steffest",
    "suule",
    "unlock",
    "v3nom",
    "virgill",
    "zak",
}

function getGreets(column)
    local txt = ''
    for i = column, #_greets, 2 do
        txt = txt .. _greets[i] .. '\n'
    end
    return txt
end

_txtSections = {
    thanks = 0,
    prelude = 25,
    by = 74,
    ramble = 302,
    love = 532,
    greets = 572,
}

_txtCredits = {
    { txt = "thanks for watching",        x = 120 - 72, y = _txtSections.thanks,                           s = 1 },
    { txt = "prelude to",                 x = 120 - 76, y = _txtSections.prelude + 16 * 0,                 s = 2 },
    { txt = "n e o b y t e",              x = 120 - 80, y = _txtSections.prelude + 16 * 1,                 s = 2 },
    { txt = "by",                         x = 120 - 8,  y = _txtSections.by + 12 * 0,                      s = 1 },
    { txt = "s p e c t r o x",            x = 120 - 46, y = _txtSections.by + 12 * 1,                      s = 1 },
    { txt = "at",                         x = 120 - 8,  y = _txtSections.by + 12 * 3,                      s = 1 },
    { txt = "deadline",                   x = 120 - 32, y = _txtSections.by + 12 * 4,                      s = 1 },
    { txt = "2025",                       x = 120 - 16, y = _txtSections.by + 12 * 5,                      s = 1 },
    { txt = "@",                          x = 120 - 1,  y = _txtSections.by + 12 * 7,                      s = 1 },
    { txt = "code and gfx by",            x = 120 - 54, y = _txtSections.by + 12 * 9,                      s = 1 },
    { txt = "olympian",                   x = 120 - 32, y = _txtSections.by + 12 * 10,                     s = 1 },
    { txt = "music by",                   x = 120 - 30, y = _txtSections.by + 12 * 12,                     s = 1 },
    { txt = "virgill",                    x = 120 - 28, y = _txtSections.by + 12 * 13,                     s = 1 },
    { txt = "@ @ @",                      x = 120 - 8,  y = _txtSections.by + 12 * 16,                     s = 1 },
    { txt = "olympian at the keys . . .", x = 40,       y = _txtSections.ramble + 12 * 0,                  s = 1 },
    { txt = "this prelude turned out",    x = 40,       y = _txtSections.ramble + 12 * 2,                  s = 1 },
    { txt = "longer than i thought :)",   x = 40,       y = _txtSections.ramble + 12 * 3,                  s = 1 },
    { txt = "i had lots of fun with",     x = 40,       y = _txtSections.ramble + 12 * 5,                  s = 1 },
    { txt = "the tic-80 . . .  i wonder", x = 40,       y = _txtSections.ramble + 12 * 6,                  s = 1 },
    { txt = "what that start screen",     x = 40,       y = _txtSections.ramble + 12 * 7,                  s = 1 },
    { txt = "was all about. . . ?",       x = 40,       y = _txtSections.ramble + 12 * 8,                  s = 1 },
    { txt = "many thanks to virgill",     x = 40,       y = _txtSections.ramble + 12 * 10,                 s = 1 },
    { txt = "for composing the great",    x = 40,       y = _txtSections.ramble + 12 * 11,                 s = 1 },
    { txt = "soundtrack!",                x = 40,       y = _txtSections.ramble + 12 * 12,                 s = 1 },
    { txt = "thanks to zak/fzk and",      x = 40,       y = _txtSections.ramble + 12 * 14,                 s = 1 },
    { txt = "hammerfist/dsr for the",     x = 40,       y = _txtSections.ramble + 12 * 15,                 s = 1 },
    { txt = "support!",                   x = 40,       y = _txtSections.ramble + 12 * 16,                 s = 1 },
    { txt = "love and",                   x = 120 - 60, y = _txtSections.love + 16 * 0,                    s = 2 },
    { txt = "respect to",                 x = 120 - 76, y = _txtSections.love + 16 * 1,                    s = 2 },
    { txt = getGreets(1),                 x = 25,       y = _txtSections.greets,                           s = 1 },
    { txt = getGreets(2),                 x = 140,      y = _txtSections.greets,                           s = 1 },
    { txt = "and everyone i forgot!",     x = 120 - 79, y = _txtSections.greets + 12 * #_greets // 2 + 16, s = 1 },
}

-- 0: Black, 1: BG gradient, 2: Font gradient, 3-4: Bubbles, 5-7: Glitch cube, 8-12: Water particles, 13: Font BG. 14-15: used in Unterwater scene's BDR.
_waterPal =
{ 0, 0, 0, 255, 0, 255, 255, 0, 255, 255, 0, 255, 255, 0, 255, 22, 29, 89, 188, 74, 155, 245, 224, 226, 20, 52, 100, 40, 92, 196, 36, 159, 222, 32, 214, 199, 255, 255, 255, 28, 0, 48, 255, 0, 255, 255, 0, 255 }

-- Same as _waterPal with 8-12 made purple.
_waterPurplePal =
{ 0, 0, 0, 255, 0, 255, 255, 0, 255, 255, 0, 255, 255, 0, 255, 22, 29, 89, 188, 74, 155, 245, 224, 226, 36, 34, 52, 64, 51, 83, 121, 58, 128, 188, 74, 155, 255, 255, 255, 28, 0, 48, 255, 0, 255, 255, 0, 255 }

_waterGradBG =
{ 8, 50, 99, 8, 50, 99, 8, 50, 99, 8, 50, 99, 8, 50, 99, 8, 50, 99, 8, 50, 99, 8, 50, 99, 8, 50, 99, 8, 50, 99, 8, 50, 99, 8, 50, 99, 8, 50, 99, 8, 50, 99, 8, 50, 99, 8, 50, 99, 8, 50, 99, 8, 50, 99, 8, 50, 99, 8, 50, 99, 8, 50, 99, 8, 50, 99, 8, 50, 99, 8, 50, 99, 0, 50, 99, 25, 41, 99, 25, 50, 99, 25, 50, 99, 25, 50, 99, 25, 41, 99, 17, 41, 91, 17, 41, 91, 25, 41, 99, 17, 41, 91, 17, 41, 99, 25, 41, 99, 17, 50, 91, 25, 41, 99, 17, 41, 91, 17, 41, 99, 25, 41, 99, 17, 50, 91, 25, 41, 99, 17, 41, 91, 17, 50, 91, 25, 41, 99, 17, 41, 91, 17, 41, 91, 17, 41, 91, 17, 41, 83, 25, 41, 91, 17, 41, 83, 17, 41, 91, 25, 41, 83, 17, 41, 83, 25, 41, 91, 17, 41, 83, 17, 41, 83, 25, 41, 91, 17, 41, 83, 17, 41, 91, 25, 41, 83, 17, 41, 83, 17, 41, 83, 17, 41, 83, 17, 41, 83, 17, 41, 83, 17, 33, 83, 17, 41, 83, 17, 41, 83, 17, 41, 83, 17, 41, 83, 17, 33, 83, 17, 41, 83, 17, 41, 83, 17, 41, 83, 17, 41, 83, 17, 41, 83, 17, 41, 83, 17, 33, 83, 17, 33, 74, 8, 33, 83, 17, 33, 83, 17, 41, 74, 17, 33, 83, 17, 33, 83, 8, 41, 74, 17, 33, 83, 17, 33, 74, 8, 33, 83, 17, 33, 83, 8, 41, 74, 17, 33, 83, 17, 33, 74, 8, 33, 83, 8, 33, 74, 17, 33, 66, 17, 33, 74, 17, 33, 66, 8, 33, 66, 17, 33, 74, 17, 33, 66, 8, 33, 74, 17, 33, 74, 17, 33, 66, 17, 33, 74, 17, 33, 66, 8, 33, 74, 17, 33, 74, 17, 33, 66, 17, 33, 74, 17, 33, 66, 8, 33, 66, 8, 33, 66, 8, 33, 66, 8, 33, 66, 8, 33, 66, 8, 33, 66, 17, 33, 66, 8, 33, 66, 8, 33, 66, 8, 33, 66, 8, 25, 66, 8, 33, 66, 8, 33, 66, 8, 33, 66, 17, 33, 66, 8, 33, 66, 8, 33, 66, 8, 25, 66, 8, 33, 66, 8, 25, 66, 8, 25, 66, 8, 33, 58, 8, 25, 66, 8, 25, 66, 8, 33, 66, 8, 25, 66, 8, 33, 58, 8, 25, 66, 8, 25, 66, 8, 33, 66, 8, 25, 66, 8, 33, 58, 17, 33, 58, 8, 25, 58, 8, 33, 50, 8, 25, 58, 8, 25, 58, 8, 25, 58, 8, 25, 58, 8, 33, 50, 8, 25, 58, 8, 25, 58, 8, 25, 50, 8, 25, 58, 8, 25, 58, 8, 25, 58, 8, 25, 58, 8, 33, 50, 8, 25, 58, 0, 25, 50, 0, 25, 58, 8, 25, 50, 0, 25, 50, 8, 25, 58, 0, 25, 50, 0, 25, 50, 8, 25, 50, 0, 25, 50, 0, 25, 58, 8, 25, 50, 0, 25, 50, 8, 25, 50, 0, 25, 50, 0, 25, 50, 8, 25, 50, 0, 17, 50, 0, 25, 50, 0, 17, 50, 0, 25, 50, 0, 25, 50, 0, 17, 50, 0, 25, 50, 0, 17, 50, 0, 25, 50, 0, 25, 50, 0, 17, 50, 0, 25, 50, 0, 25, 50, 0, 17, 50, 0, 25, 50, 0, 17, 50, 0, 25, 50, 0, 25, 50, 0, 17, 50, 0, 25, 50, 0, 17, 50, 0, 25, 50, 0, 25, 50, 0, 17, 50, 0, 25, 41, 0, 17, 41, 0, 17, 41, 0, 25, 41, 0, 17, 41, 0, 25, 41, 0, 17, 41, 0, 17, 41, 0, 25, 41, 0, 17, 41, 0, 17, 41, 0, 25, 41, 0, 17, 41, 0, 25, 41, 0, 17, 41, 0, 17, 41, 0, 25, 41, 0, 17, 41, 0, 25, 41, 0, 17, 41, 0, 17, 41, 0, 25, 41, 0, 17, 41, 0, 17, 41, 0, 25, 41, 0, 17, 41, 0, 25, 41, 0, 17, 41, 0, 17, 41, 0, 17, 33, 0, 17, 41, 0, 17, 33, 0, 17, 41, 0, 17, 41, 0, 17, 33, 0, 17, 41, 0, 17, 33, 0, 17, 41, 0, 17, 41, 0, 17, 33, 0, 17, 41, 0, 17, 41, 0, 17, 33, 0, 17, 41, 0, 17, 33, 0, 17, 41, 0, 17, 41, 0, 17, 33, 0, 17, 41, 0, 17, 41, 0, 17, 41, 0, 17, 41, 0, 17, 33, 0, 17, 41, 0, 17, 41, 0, 17, 33, 0, 17, 33, 0, 8, 33, 0, 17, 41, 0, 17, 33, 0, 17, 33, 0, 17, 33, 0, 17, 33, 0, 17, 33, 0, 17, 33, 0, 8, 33, 0, 17, 33, 0, 17, 33, 0, 17, 33, 0, 17, 33, 0, 8, 33, 0, 17, 41, 0, 17, 33, 0, 17, 33, 0, 17, 33, 0, 17, 33, 0, 17, 33, 0, 17, 33, 0, 8, 33, 0, 17, 41, 0, 17, 33, 0, 17, 33, 0, 17, 33, 0, 8, 33, 0, 17, 33, 0, 8, 33, 0, 17, 33, 0, 8, 33, 0, 8, 33, 0, 17, 33, 0, 8, 33, 0, 8, 33, 0, 17, 33, 0, 8, 33, 0, 17, 33, 0, 8, 33, 0, 8, 33, 0, 17, 33, 0, 8, 33, 0, 8, 33, 0, 8, 33, 0, 8, 33, 0, 17, 33, 0, 8, 33, 0, 8, 33, 0, 17, 33, 0, 8, 33, 0, 17, 33, 0, 8, 33, 0, 8, 33, 0, 8, 25, 0, 8, 33, 0, 8, 25, 0, 8, 33, 0, 8, 33, 0, 8, 25, 0, 8, 33, 0, 8, 25, 0, 8, 25, 0, 8, 33, 0, 8, 25, 0, 8, 33, 0, 8, 33, 0, 8, 25, 8, 8, 33, 8, 8, 25, 0, 8, 33, 8, 8, 33, 8, 8, 25, 8, 8, 33, 8, 8, 25, 0, 8, 25, 8, 8, 33, 8, 8, 25, 0, 8, 33, 8, 8, 33, 0, 8, 25, 8, 8, 33, 0, 0, 25, 0, 8, 25, 8, 0, 25, 0, 8, 25, 8, 0, 25, 8, 8, 25, 0, 8, 17, 8, 8, 25, 0, 0, 25, 0, 8, 25, 8, 8, 25, 0, 8, 17, 8, 8, 25, 8, 8, 25, 0, 8, 17, 8, 8, 25, 0, 0, 17, 0, 8, 25, 8, 8, 25, 0, 8, 17, 8, 8, 25, 0, 8, 25, 0, 8, 25, 8, 8, 25, 0, 8, 17, 8, 8, 25, 8, 8, 25, 0, 8, 17, 0, 0, 17, 0, 0, 17, 0, 8, 25, 0, 0, 17, 0, 8, 17, 8, 0, 17, 0, 0, 17, 0, 0, 17, 0, 0, 17, 0, 0, 17, 8, 0, 17, 0, 0, 8, 0, 0, 8, 0, 0, 8, 0, 0, 8, 0, 0, 17, 0, 0, 8, 0, 0, 8, 0, 0, 8, 0, 0, 8, 0, 0, 8, 0, 0, 8, 0, 0, 0, 0, 0, 8, 0, 0, 8, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 8, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0 }

_waterBubblesGrad =
{ 17, 51, 102, 0, 51, 102, 0, 51, 102, 17, 68, 102, 17, 51, 119, 17, 68, 102, 17, 51, 119, 17, 68, 102, 0, 68, 119, 0, 68, 119, 0, 68, 102, 0, 85, 119, 0, 68, 119, 0, 85, 119, 17, 68, 119, 17, 85, 119, 0, 85, 137, 0, 85, 119, 0, 85, 137, 0, 102, 119, 0, 85, 137, 0, 102, 119, 17, 85, 137, 34, 102, 137, 0, 102, 137, 0, 102, 137, 0, 102, 137, 0, 119, 137, 0, 119, 137, 0, 102, 154, 0, 119, 137, 17, 119, 137, 0, 119, 154, 0, 119, 137, 0, 119, 154, 0, 119, 154, 0, 137, 154, 0, 119, 154, 0, 137, 154, 17, 137, 154, 0, 137, 154, 0, 137, 154, 0, 137, 154, 0, 154, 171, 0, 137, 171, 0, 154, 154, 17, 137, 171, 17, 154, 171, 0, 154, 171, 0, 154, 171, 0, 171, 171, 0, 154, 171, 0, 171, 171, 34, 154, 171, 34, 171, 171, 34, 171, 171, 0, 171, 188, 0, 171, 171, 0, 188, 188, 0, 171, 188, 0, 188, 188, 0, 171, 188, 34, 188, 188, 51, 188, 171, 0, 188, 188, 0, 205, 188, 0, 188, 205, 0, 205, 188, 0, 205, 188, 0, 188, 205, 34, 205, 205, 34, 205, 205, 0, 222, 205, 0, 205, 188, 0, 222, 205, 0, 205, 205, 34, 222, 205, 34, 222, 205, 34, 222, 205, 51, 222, 205, 34, 222, 205, 51, 222, 205, 51, 222, 222, 34, 222, 222, 51, 222, 205, 51, 222, 205, 51, 222, 205, 51, 222, 205, 34, 222, 205, 51, 222, 205, 51, 222, 205, 34, 222, 222, 51, 222, 222, 51, 222, 205, 51, 222, 205, 51, 222, 205, 34, 222, 205, 51, 222, 205, 51, 222, 205, 34, 222, 205, 51, 222, 222, 51, 222, 222, 34, 222, 205, 51, 222, 205, 51, 222, 205, 34, 222, 205, 51, 222, 205, 51, 222, 205, 34, 222, 205, 51, 222, 222, 51, 222, 222, 17, 222, 205, 34, 222, 205, 34, 222, 205, 34, 222, 205, 17, 205, 188, 0, 222, 222, 34, 205, 205, 51, 205, 205, 34, 205, 205, 34, 205, 205, 34, 205, 188, 17, 205, 205, 0, 188, 188, 0, 205, 205, 17, 188, 188, 34, 188, 205, 34, 188, 188, 34, 188, 188, 17, 188, 188, 0, 171, 205, 0, 188, 188, 0, 171, 188, 0, 171, 188, 17, 171, 188, 0, 171, 188 }

_waterGradVbank0 = nil -- Holds resulting gradient.
_waterGradSplash = nil -- Downsplash color.

--- UNDERWATER VARIABLES
_underwaterEmitter = nil
_underwaterCubeEmitter = nil

_imgSand = nil           --0,54
_imgSandFG = nil         --0,72
_imgKelpBG = nil         -- 0,45
_imgKelp1 = nil          -- 42,36
_imgKelp2 = nil          -- 59,40
_imgKelp3 = nil          -- 166,45

_imgKelpFGLeft1 = nil    -- 0,6
_imgKelpFGLeft2 = nil    -- 9,20
_imgKelpFGLeft3 = nil    -- 13,40
_imgKelpFGLeft4 = nil    -- 19,68

_imgKelpFGRight1 = nil   -- 209,81
_imgKelpFGRight2 = nil   -- 209,55
_imgKelpFGRight3 = nil   -- 213,37
_imgKelpFGRight4 = nil   -- 220,13
_imgKelpFGRight5 = nil   -- 228,25
_imgKelpFGRight6 = nil   -- 232,69

_imgSideRocksLeft = nil  -- 0,69
_imgSideRocksRight = nil -- 186,72

_imgThe = nil
_imgEnd = nil

_gradUnderwater = { 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 4, 0, 5, 4, 0, 5, 4, 0, 5, 4, 0, 5, 4, 0, 5, 4, 0, 5, 8, 4, 13, 8, 0, 13, 12, 4, 22, 8, 4, 13, 24, 4, 32, 12, 4, 22, 24, 4, 32, 20, 4, 22, 37, 9, 43, 20, 4, 22, 37, 9, 43, 24, 9, 32, 37, 9, 43, 24, 9, 32, 50, 17, 56, 42, 13, 57, 42, 17, 66, 51, 24, 75, 42, 17, 66, 35, 33, 67, 51, 24, 75, 43, 41, 84, 43, 41, 75, 41, 48, 81, 54, 42, 87, 49, 56, 89, 41, 48, 81, 38, 62, 92, 49, 56, 89, 46, 70, 100, 46, 69, 92, 29, 74, 95, 55, 71, 103, 44, 83, 113, 36, 75, 105, 30, 91, 119, 52, 91, 113, 45, 99, 128, 37, 98, 120, 45, 113, 128, 61, 107, 121, 53, 122, 137, 52, 114, 129, 30, 129, 144, 61, 123, 138, 61, 138, 154, 45, 130, 145, 68, 146, 163, 61, 146, 162, 53, 155, 170, 46, 154, 169, 66, 175, 186, 61, 165, 175, 38, 180, 189, 59, 174, 185, 15, 170, 179, 38, 180, 189, 15, 170, 179, 38, 180, 189, 15, 170, 179, 38, 180, 189, 15, 170, 179, 15, 170, 179, 15, 170, 179, 15, 170, 179, 15, 170, 179, 15, 170, 179, 15, 170, 179, 15, 170, 179, 15, 160, 175, 15, 170, 179, 15, 160, 175, 15, 170, 179, 15, 160, 175, 15, 170, 179, 15, 160, 175, 53, 155, 170, 15, 160, 175, 53, 155, 170, 61, 165, 175, 53, 155, 170, 61, 165, 175, 53, 155, 170, 46, 154, 169, 53, 155, 170, 46, 154, 169, 53, 155, 170, 46, 154, 169, 53, 155, 170, 46, 154, 169, 46, 154, 169, 46, 154, 169, 22, 144, 159, 46, 154, 169, 15, 144, 167, 15, 144, 167, 15, 144, 167, 15, 144, 167, 15, 144, 167, 15, 144, 167, 15, 144, 167, 15, 144, 167, 15, 144, 167, 15, 144, 167, 53, 146, 170, 15, 144, 167, 46, 138, 160, 46, 138, 160, 46, 138, 160, 46, 138, 160, 38, 138, 160, 46, 138, 160, 38, 138, 160, 46, 138, 160, 38, 138, 160, 46, 138, 160 }
_gradUnderwaterBrightBase = { 10, 0, 17, 10, 0, 17, 10, 0, 17, 10, 0, 17, 10, 0, 17, 10, 0, 17, 10, 0, 17, 10, 0, 17, 10, 0, 17, 10, 0, 17, 10, 0, 17, 10, 0, 17, 10, 0, 17, 10, 0, 17, 10, 0, 17, 10, 0, 17, 10, 0, 17, 10, 0, 17, 10, 0, 17, 10, 0, 17, 15, 7, 28, 15, 0, 28, 18, 6, 38, 15, 7, 28, 34, 6, 51, 18, 6, 38, 34, 6, 51, 31, 6, 39, 49, 11, 62, 31, 6, 39, 49, 11, 62, 34, 12, 51, 49, 11, 62, 34, 12, 51, 59, 24, 72, 48, 16, 73, 45, 23, 79, 54, 33, 86, 45, 23, 79, 36, 42, 78, 54, 33, 86, 46, 49, 95, 46, 50, 86, 44, 57, 92, 58, 50, 98, 53, 65, 101, 44, 57, 92, 40, 71, 102, 53, 65, 101, 49, 79, 111, 48, 80, 103, 31, 86, 108, 59, 81, 114, 47, 93, 126, 40, 86, 117, 32, 101, 132, 56, 103, 126, 48, 110, 141, 39, 110, 133, 48, 126, 141, 66, 119, 134, 57, 135, 150, 56, 126, 142, 31, 142, 157, 65, 135, 151, 64, 151, 168, 48, 142, 158, 69, 160, 179, 62, 160, 178, 52, 169, 187, 45, 168, 185, 70, 188, 199, 61, 180, 191, 37, 196, 205, 60, 189, 199, 16, 184, 191, 37, 196, 205, 16, 184, 191, 37, 196, 205, 16, 184, 191, 37, 196, 205, 16, 184, 191, 16, 184, 191, 16, 184, 191, 16, 184, 191, 16, 184, 191, 16, 184, 191, 16, 184, 191, 16, 184, 191, 15, 173, 188, 16, 184, 191, 15, 173, 188, 16, 184, 191, 15, 173, 188, 16, 184, 191, 15, 173, 188, 52, 169, 187, 15, 173, 188, 52, 169, 187, 61, 180, 191, 52, 169, 187, 61, 180, 191, 52, 169, 187, 45, 168, 185, 52, 169, 187, 45, 168, 185, 52, 169, 187, 45, 168, 185, 52, 169, 187, 45, 168, 185, 45, 168, 185, 45, 168, 185, 23, 157, 172, 45, 168, 185, 15, 156, 180, 15, 156, 180, 15, 156, 180, 15, 156, 180, 15, 156, 180, 15, 156, 180, 15, 156, 180, 15, 156, 180, 15, 156, 180, 15, 156, 180, 52, 159, 187, 15, 156, 180, 47, 150, 174, 47, 150, 174, 47, 150, 174, 47, 150, 174, 40, 149, 173, 47, 150, 174, 40, 149, 173, 47, 150, 174, 40, 149, 173, 47, 150, 174 }
_gradUnderwaterBrightMix = {}

--- HIDDEN PART VARIABLES
-- Pssst, nothing to see here!
_gradHidden = { 37, 73, 219, 37, 73, 219, 37, 73, 219, 37, 73, 219, 37, 73, 219, 37, 73, 219, 37, 37, 219, 37, 73, 219, 73, 37, 219, 37, 37, 219, 73, 37, 183, 73, 37, 183, 73, 37, 183, 73, 37, 183, 73, 37, 183, 73, 37, 183, 73, 37, 183, 73, 37, 183, 73, 37, 183, 110, 37, 146, 73, 37, 183, 110, 37, 146, 110, 37, 146, 110, 37, 146, 110, 37, 146, 110, 37, 146, 110, 37, 146, 146, 37, 146, 110, 37, 146, 146, 37, 146, 146, 37, 146, 146, 37, 146, 146, 37, 146, 183, 37, 146, 146, 37, 146, 183, 37, 146, 183, 37, 146, 183, 37, 146, 183, 37, 146, 219, 73, 146, 183, 37, 146, 219, 73, 146, 219, 73, 146, 219, 73, 146, 219, 73, 146, 219, 73, 146, 219, 73, 146, 255, 110, 183, 219, 73, 146, 255, 110, 183, 255, 110, 183, 255, 146, 183, 255, 110, 183, 255, 146, 183, 255, 146, 219, 255, 146, 183, 255, 183, 219, 255, 183, 219, 255, 183, 219, 255, 219, 219, 255, 183, 219, 255, 219, 255, 255, 219, 219, 255, 219, 255, 255, 255, 255, 255, 219, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 219, 255, 255, 255, 255, 255, 219, 255, 255, 219, 255, 219, 183, 255, 255, 219, 255, 219, 183, 255, 219, 183, 255, 219, 146, 255, 219, 183, 255, 219, 146, 255, 219, 146, 255, 183, 110, 255, 219, 146, 255, 183, 110, 255, 183, 110, 255, 183, 110, 255, 183, 73, 255, 183, 110, 255, 183, 73, 255, 183, 73, 255, 146, 37, 219, 183, 73, 255, 146, 37, 219, 146, 37, 219, 146, 37, 219, 146, 37, 219, 146, 37, 219, 146, 37, 219, 146, 37, 219, 146, 37, 219, 110, 37, 219, 146, 37, 219, 110, 37, 219, 110, 37, 219, 110, 37, 219, 110, 37, 219, 110, 37, 219, 110, 37, 219, 110, 37, 219, 110, 37, 219, 110, 37, 219, 73, 37, 219, 110, 37, 219, 73, 37, 219, 73, 37, 219, 73, 37, 219, 73, 37, 219, 73, 37, 219, 73, 37, 219, 73, 37, 219, 73, 37, 219, 73, 73, 219, 73, 37, 219, 37, 73, 219, 37, 73, 219, 37, 73, 219, 37, 73, 219, 37, 73, 219, 37, 73, 219, 37, 73, 219, 37, 73, 219, 37, 73, 219, 37, 73, 219 }
_gradScanline = { 20, 52, 100, 13, 33, 64, 20, 52, 100, 13, 33, 64, 20, 52, 100, 13, 33, 64, 20, 52, 100, 13, 33, 64, 20, 52, 100, 13, 33, 64, 20, 52, 100, 13, 33, 64, 20, 52, 100, 13, 33, 64, 20, 52, 100, 13, 33, 64, 20, 52, 100, 13, 33, 64, 20, 52, 100, 13, 33, 64, 20, 52, 100, 13, 33, 64, 20, 52, 100, 13, 33, 64, 20, 52, 100, 13, 33, 64, 20, 52, 100, 13, 33, 64, 20, 52, 100, 13, 33, 64, 20, 52, 100, 13, 33, 64, 20, 52, 100, 13, 33, 64, 20, 52, 100, 13, 33, 64, 20, 52, 100, 13, 33, 64, 20, 52, 100, 13, 33, 64, 20, 52, 100, 13, 33, 64, 20, 52, 100, 13, 33, 64, 20, 52, 100, 13, 33, 64, 20, 52, 100, 13, 33, 64, 20, 52, 100, 13, 33, 64, 20, 52, 100, 13, 33, 64, 20, 52, 100, 13, 33, 64, 20, 52, 100, 13, 33, 64, 20, 52, 100, 13, 33, 64, 20, 52, 100, 13, 33, 64, 20, 52, 100, 13, 33, 64, 20, 52, 100, 13, 33, 64, 20, 52, 100, 13, 33, 64, 20, 52, 100, 13, 33, 64, 20, 52, 100, 13, 33, 64, 20, 52, 100, 13, 33, 64, 20, 52, 100, 13, 33, 64, 20, 52, 100, 13, 33, 64, 20, 52, 100, 13, 33, 64, 20, 52, 100, 13, 33, 64, 20, 52, 100, 13, 33, 64, 20, 52, 100, 13, 33, 64, 20, 52, 100, 13, 33, 64, 20, 52, 100, 13, 33, 64, 20, 52, 100, 13, 33, 64, 20, 52, 100, 13, 33, 64, 20, 52, 100, 13, 33, 64, 20, 52, 100, 13, 33, 64, 20, 52, 100, 13, 33, 64, 20, 52, 100, 13, 33, 64, 20, 52, 100, 13, 33, 64, 20, 52, 100, 13, 33, 64, 20, 52, 100, 13, 33, 64, 20, 52, 100, 13, 33, 64, 20, 52, 100, 13, 33, 64, 20, 52, 100, 13, 33, 64, 20, 52, 100, 13, 33, 64, 20, 52, 100, 13, 33, 64, 20, 52, 100, 13, 33, 64, 20, 52, 100, 13, 33, 64, 20, 52, 100, 13, 33, 64, 20, 52, 100, 13, 33, 64, 20, 52, 100, 13, 33, 64, 20, 52, 100, 13, 33, 64, 20, 52, 100, 13, 33, 64, 20, 52, 100, 13, 33, 64, 20, 52, 100, 13, 33, 64, 20, 52, 100, 13, 33, 64 }
_hiddenTracklist = core_sound_create_tracklist({
    { bank = 1, track = 1, pattern = -1, row = -1, lastPattern = -1, lastRow = -1 },
})

_hiddenTitle = "SPECTROX"
_hiddenScrollText =
    "CONGRATULATIONS!!! YOU FOUND THIS TINY HIDDEN PART!      YOU ARE A TRUE LEGEND!!!            " ..
    "SOME PERSONAL MESSAGES THAT DIDN'T FIT INTO THE MAIN DEMO ARE COMING UP . . . . . . . .           " ..
    "%   BUS ERROR / TELETYPE * LOOKING FORWARD TO YOUR NEXT PROJECTS!   " ..
    "%   CROSSBONE / SUBURBAN * IT'S REALLY AWESOME THAT WE FOUND OUT WE'RE TEAMMATES AT WORK!   " ..
    "%   DEPECHE / SPREADPOINT * LOOKING FORWARD TO OUR NEXT MEET-UP AT SPECTROX-HQ!   " ..
    "%   DIPSWITCH / DIVINE STYLERS * THANK YOU SO MUCH FOR THE ASCII LOGO!   " ..
    "%   GASPODE * I REALLY LOVED YOUR LATEST DEMO! THANKS FOR THE GREETS!   " ..
    "%   GIGABATES / DESIRE * I USED 'GRADIENT BLASTER' EXTENSIVELY IN THIS DEMO. THANKS FOR SHARING IT!   " ..
    "%   GRIP / ISTARI * ALWAYS APPRECIATE YOUR RELEASES, CAN'T WAIT FOR THE NEXT ONE!   " ..
    "%   HAMMERFIST / DESIRE * THANK YOU SO MUCH FOR YOUR HELP AND FEEDBACK!   " ..
    "%   MOTION / ARTSTATE * IT'S BEEN NICE TO FINALLY HOOK UP BY EMAIL! I WILL USE YOUR FONT IN ONE OF MY NEXT PRODS.   " ..
    "%   MUDLORD / DESIRE * IT WAS REALLY COOL TO SEE YOU TINKERING WITH THE 'OFFSET OVERDRIVE' SOURCE CODE!   " ..
    "%   OZAN / TEK * SEE YOU IN THE YOUTUBE COMMENTS SECTION!   " ..
    "%   PS / TPOLM * THANKS FOR YOUR WORK ON THE DEMOSCENE REPORT! I NEVER MISS AN EPISODE!   " ..
    "%   RAMON BS / DESIRE * X-MAS IS DRAWING NEAR . . . TIME TO PULL OUT THE UGLY SWEATERS!   " ..
    "%   RP / FARBRAUSCH * I REALLY LOVED CHATTING WITH YOU!   " ..
    "%   SHANA / ECHTZEIT * IT WAS AWESOME OF YOU TO DELIVER THE CHOCOLATE TROPHY FROM MOUNTAINBYTES IN PERSON!   " ..
    "%   SLAZE / DESIRE * I LOVED YOUR LATEST DNB ALBUM, KEEP IT UP!!!   " ..
    "%   SPKR / SMFX * CAN'T WAIT FOR YOUR NEXT RELEASES!   " ..
    "%   SUULE / OFTENHIDE * YOUR STUFF ON THE TIC-BO HAS BEEN A GREAT INSPIRATION TO ME!   " ..
    "%   STEFFEST / DESIRE * LOVE YOUR WORK!   " ..
    "%   UNLOCK / VANTAGE * IT WAS SO COOL TO FINALLY MEET YOU AGAIN AFTER MORE THAN TWENTY YEARS! HERE'S TO THE NEXT TWENTY!!!   " ..
    "%   VIRGILL / ALCATRAZ * IT WAS AN HONOR WORKING WITH YOU! IT WAS AN ABSOLUTE JOY!!!   " ..
    "%   ZAK / FROZAK * YOU ARE THE REAL SMOOTH CRIMINAL!!!   " ..
    "               . . . . . . I GUESS THAT IS IT FOR THE MOMENT . . .   OLYMPIAN / SPECTROX SIGNING OFF AND SEE YOU IN THE NEXT ONE % % %"
_hiddenScrollX = 500

_hiddenDelayMap = {
    0, 75, 80, 165, 170, 245, 200, 195, 120, 115, 40, 35,
    5, 70, 85, 160, 175, 240, 205, 190, 125, 110, 45, 30,
    10, 65, 90, 155, 180, 235, 210, 185, 130, 105, 50, 25,
    15, 60, 95, 150, 185, 230, 215, 180, 135, 100, 55, 20,
    20, 55, 100, 145, 190, 225, 220, 175, 140, 95, 60, 15,
    25, 50, 105, 140, 195, 220, 225, 170, 145, 90, 65, 10,
    30, 45, 110, 135, 200, 215, 230, 165, 150, 85, 70, 5,
    35, 40, 125, 130, 205, 210, 235, 160, 155, 80, 75, 0,
}
_hiddenDelayMapMaxVal = 172 -- NOTE: Don't forget to keep updated!
_hiddenDelayFactor = 8

-------------------------------------------------------------------------------
--- ANIM
-------------------------------------------------------------------------------

-- Must be sequential numeric indices starting at 1.
local AnimNames = {
    STARTUP_DELAY = 1,
    SCREEN_FADER = 2,
    PRESENTS = 3,
    TITLE = 4,
    BLOB = 5,
    CREDITS = 6,
    TRANSIT2LISSA = 7,
    LISSA = 8,
    SHADOW = 9,
    TRANSIT2SHADOW = 10,
    HAVE2RUN = 11,
    RUNNER = 12,
    TRANSIT2VOX = 13,
    VOX = 14,
    WATERFIELD = 15,
    UNDERWATER = 16,
    HIDDEN = 17,
}

local _schedule = core_anim_new_schedule()

-------------------------------------------------------------------------------
--- STARTUP_DELAY ANIM

core_anim_add_anim(AnimNames.STARTUP_DELAY, _schedule, core_anim_new(false,
    {
        core_anim_new_keyframe_nop(1),
    },
    function() end,
    function() core_anim_start(_schedule.anims[AnimNames.SCREEN_FADER], _timer) end
))

-------------------------------------------------------------------------------
--- SCREEN_FADER ANIM

_coverHeight = { 0 }
_faderPalFactor = { 0 }

function screenFaderSceneStarted()
    vbank(1)
    core_pal_apply(_palBlob)
    core_pal_apply_color(14, 255, 255, 255)
    core_pal_apply_color(15, _palTic[1], _palTic[2], _palTic[3])
    _musicStarted = true
end

function screenFaderSceneCompleted()
    -- Reset vbanks and start next anim.
    vbank(1)
    cls(0)
    vbank(0)
    core_anim_start(_schedule.anims[AnimNames.PRESENTS], _timer)
end

core_anim_add_anim(AnimNames.SCREEN_FADER, _schedule, core_anim_new(false,
    {
        core_anim_new_keyframe_tween(0, 2, { -1 }, { 69 }, core_easing_bounce_once_out, _coverHeight),
        core_anim_new_keyframe_tween(0.5, 1.7, { 0 }, { 1 }, core_easing_lerp, _faderPalFactor),
        core_anim_new_keyframe_nop(3),
    },
    screenFaderSceneStarted,
    screenFaderSceneCompleted
))

-------------------------------------------------------------------------------
--- PRESENTS ANIM

_presentsHighlightLines = {
    { y = { 0 } },
    { y = { 0 } },
    { y = { 0 } },
}

_presents1Enabled = true
_presents2Enabled = false
_presents3Enabled = false

_presentsColorIdx = { #_presentsFadeIndexes }
_presentsClipPosY = { -20 }

function presentsSceneStarted()
    vbank(0)
    core_pal_apply(_palTitleHighlightOnly)
    vbank(1)
    core_pal_apply(_imgNeobyte.palette)
end

function presentsSceneCompleted()
    vbank(1)
    cls(0)
    vbank(0)
    core_anim_start(_schedule.anims[AnimNames.TITLE], _timer)
end

core_anim_add_anim(AnimNames.PRESENTS, _schedule, core_anim_new(false,
    {
        core_anim_new_keyframe_tween(0, 2, { 33 }, { 103 }, core_easing_lerp, _presentsHighlightLines[1].y),
        core_anim_new_keyframe_tween(0, 2, { -16 }, { 54 }, core_easing_lerp, _presentsClipPosY), -- Clip top down.
        core_anim_new_keyframe_tween(2.3, 3.65, { #_presentsFadeIndexes }, { 1 }, core_easing_lerp, _presentsColorIdx),
        core_anim_new_keyframe_fire_once(4, function()
            _presents1Enabled = false
            _presents2Enabled = true
            _presentsColorIdx[1] = #_presentsFadeIndexes
        end),
        core_anim_new_keyframe_tween(4, 6, { 33 }, { 103 }, core_easing_lerp, _presentsHighlightLines[1].y),
        core_anim_new_keyframe_tween(4, 6, { 103 }, { 33 }, core_easing_lerp, _presentsHighlightLines[2].y),
        core_anim_new_keyframe_tween(4, 6, { 103 }, { 33 }, core_easing_lerp, _presentsClipPosY), -- Clip bottom up.
        core_anim_new_keyframe_tween(6.3, 7.65, { #_presentsFadeIndexes }, { 1 }, core_easing_lerp, _presentsColorIdx),
        core_anim_new_keyframe_fire_once(8, function()
            _presents2Enabled = false
            _presents3Enabled = true
            _presentsColorIdx[1] = #_presentsFadeIndexes
        end),
        core_anim_new_keyframe_tween(8, 10, { 33 }, { 103 }, core_easing_lerp, _presentsHighlightLines[1].y),
        core_anim_new_keyframe_tween(8, 10, { 113 }, { 33 }, core_easing_lerp, _presentsHighlightLines[2].y),
        core_anim_new_keyframe_tween(8, 10, { 23 }, { 103 }, core_easing_lerp, _presentsHighlightLines[3].y),
        core_anim_new_keyframe_tween(8, 10, { -16 }, { 54 }, core_easing_lerp, _presentsClipPosY), -- Clip top down.
        core_anim_new_keyframe_tween(10.3, 11.65, { #_presentsFadeIndexes }, { 1 }, core_easing_lerp, _presentsColorIdx),

    },
    presentsSceneStarted,
    presentsSceneCompleted
))

-------------------------------------------------------------------------------
--- TITLE ANIM

_numTitleHighlightLines = { 0 }
_titlePalMaxFactor = { 1 }
_titlePalMixEnabled = true
_titleGridEnabled = false
_titleFlashFactor = { 0 }
_titleFlashEnabled = false
_titlePosXAmp = 0
_titleGridVerticalOnly = true
_pressStartEnabled = false

_blobBorderTopX = { 245 }
_blobBorderBottomX = { -245 }

function titleSceneStarted()
    vbank(0)
    core_pal_apply(_palBlob)
end

function titleSceneCompleted()
    -- Reset vbanks and start next anim.
    vbank(1)
    cls(0)
    vbank(0)
    core_anim_start(_schedule.anims[AnimNames.BLOB], _timer)
end

core_anim_add_anim(AnimNames.TITLE, _schedule, core_anim_new(false,
    {
        core_anim_new_keyframe_tween(0, 1, { 0 }, { 3 }, core_easing_lerp, _numTitleHighlightLines),
        core_anim_new_keyframe_tween(2, 5, { 3 }, { 160 }, core_easing_lerp, _numTitleHighlightLines),
        --
        core_anim_new_keyframe_tween(7.4, 10, { 160 }, { 5 }, core_easing_lerp, _numTitleHighlightLines),
        core_anim_new_keyframe_tween(10.5, 10.9, { 5 }, { 3 }, core_easing_lerp, _numTitleHighlightLines),
        core_anim_new_keyframe_tween(9.7, 10.8, { 1 }, { 0 }, core_easing_quadratic_out, _titlePalMaxFactor),
        --
        core_anim_new_keyframe_fire_once(10.95, function()
            _pressStartBlinkerEnabled = false
            _titlePalMixEnabled = false
            _titleGridEnabled = true
            _titleFlashEnabled = true
            _pressStartEnabled = true
        end),
        core_anim_new_keyframe_tween(10.95, 11.95, { 1 }, { 0 }, core_easing_lerp, _titleFlashFactor),
        --
        core_anim_new_keyframe_fire_once(21.08, function()
            _titlePosXAmp = 60
            _titleGridVerticalOnly = false
            _pressStartEnabled = false
            _blobBorderTopX[1] = 0
            _blobBorderBottomX[1] = 0
        end),
        core_anim_new_keyframe_tween(21.08, 22.08, { 1 }, { 0 }, core_easing_lerp, _titleFlashFactor),
        core_anim_new_keyframe_tween(23.22, 24.22, { 1 }, { 0 }, core_easing_lerp, _titleFlashFactor),
        core_anim_new_keyframe_tween(25.35, 26.35, { 1 }, { 0 }, core_easing_lerp, _titleFlashFactor),
        --
        core_anim_new_keyframe_nop(26.95),
    },
    titleSceneStarted,
    titleSceneCompleted
))

-------------------------------------------------------------------------------
--- BLOB ANIM

_blobEnabled              = true
_blobFlashEnabled         = true
_blobFlashFactor          = { 1 }
_blobPos                  = { 280, -180 }
_blobGridColorIdx         = { #_runTextFadeColors }
_blobGridColorFadeEnabled = false

function blobSceneStarted()
    core_pal_apply(_palBlob)
end

core_anim_add_anim(AnimNames.BLOB, _schedule, core_anim_new(false,
    {
        core_anim_new_keyframe_tween(0, 1, { 1 }, { 0 }, core_easing_lerp, _blobFlashFactor),
        core_anim_new_keyframe_fire_once(1, function() _blobFlashEnabled = false; end),
        core_anim_new_keyframe_tween(0, .8, { 280, 0 }, { -100, 0 }, core_easing_quadratic_out, _blobPos),
        core_anim_new_keyframe_tween(.8, 1.9, { -100, 0 }, { 0, 0 }, core_easing_quadratic_in_out, _blobPos),
        --
        core_anim_new_keyframe_tween(17, 18, { 0, 0 }, { 0, 150 }, core_easing_back_in, _blobPos),
        core_anim_new_keyframe_fire_once(18, function() _blobEnabled = false; end),

        core_anim_new_keyframe_tween(17.2, 18, { 0 }, { 245 }, core_easing_quadratic_out, _blobBorderTopX),
        core_anim_new_keyframe_tween(17.2, 18, { 0 }, { -245 }, core_easing_quadratic_out, _blobBorderBottomX),
        --
        core_anim_new_keyframe_fire_once(17.2, function() _blobGridColorFadeEnabled = true; end),
        core_anim_new_keyframe_tween(17.2, 18, { #_runTextFadeColors }, { 1 }, core_easing_lerp, _blobGridColorIdx),
    },
    blobSceneStarted,
    function() core_anim_start(_schedule.anims[AnimNames.CREDITS], _timer) end
))

-------------------------------------------------------------------------------
--- CREDITS ANIM

_creditsOlympianFadeIndex = { #_presentsFadeIndexes }
_creditsVirgillFadeIndex  = { #_presentsFadeIndexes }
_creditsOlympianPos       = { 0, 0 }
_creditsCodePos           = { 0, 0 }
_creditsVirgillPos        = { 0, 0 }
_creditsMusicPos          = { 0, 0 }

_creditsOlympianEnabled   = false
_creditsVirgillEnabled    = false

function creditsSceneStarted()
    core_pal_apply(_imgNeobyte.palette)
end

core_anim_add_anim(AnimNames.CREDITS, _schedule, core_anim_new(false,
    {
        core_anim_new_keyframe_fire_once(1.7267, function() _creditsOlympianEnabled = true; end),
        core_anim_new_keyframe_tween(1.96, 2.86, { #_presentsFadeIndexes }, { 1 }, core_easing_cubic_in,
            _creditsOlympianFadeIndex),
        core_anim_new_keyframe_tween(1.7267, 2.86, { 0, 0 }, { 20, 0 }, core_easing_lerp, _creditsOlympianPos),
        core_anim_new_keyframe_tween(1.7267, 2.86, { 0, 0 }, { -20, 0 }, core_easing_lerp, _creditsCodePos),
        --
        core_anim_new_keyframe_fire_once(3.8667, function() _creditsVirgillEnabled = true; end),
        core_anim_new_keyframe_tween(4.1, 5, { #_presentsFadeIndexes }, { 1 }, core_easing_cubic_in,
            _creditsVirgillFadeIndex),
        core_anim_new_keyframe_tween(3.8667, 5, { 0, 0 }, { -20, 0 }, core_easing_lerp, _creditsVirgillPos),
        core_anim_new_keyframe_tween(3.8667, 5, { 0, 0 }, { 20, 0 }, core_easing_lerp, _creditsMusicPos),
        --
        core_anim_new_keyframe_nop(5),
    },
    creditsSceneStarted,
    function() core_anim_start(_schedule.anims[AnimNames.TRANSIT2LISSA], _timer) end
))

-------------------------------------------------------------------------------
--- TANSIT2LISSA ANIM

_lissaCoverTilesProgress = { 2 }
_coverLinesFactor = { 1 }
_lissaBGFlickerStrength = { 10 }

function transit2LissaSceneStarted()
    vbank(0)
    core_pal_apply(_imgLissaBG.palette)
    core_pal_apply_color(13, 26, 29, 33) -- Perspective lines color at index 13.
    vbank(1)
    core_pal_apply(_palTransit2LissaFont)
end

function transit2LissaSceneCompleted()
    -- Reset vbanks and start next anim.
    vbank(1)
    cls(0)
    vbank(0)
    core_anim_start(_schedule.anims[AnimNames.LISSA], _timer)
end

core_anim_add_anim(AnimNames.TRANSIT2LISSA, _schedule, core_anim_new(false,
    {
        -- Wait for text fully scrolled in.
        core_anim_new_keyframe_nop(3),
        core_anim_new_keyframe_tween(1, 2.5, { 1 }, { 0 }, core_easing_lerp, _coverLinesFactor),
    },
    transit2LissaSceneStarted,
    transit2LissaSceneCompleted
))

-------------------------------------------------------------------------------
--- LISSA ANIM

_lissaCount = { 0 }
_lissaSpeed = { 0 }
_lissaTranslateZ = { 0 }

_lissaLogoLeftPosY = { 111 }
_lissaLogoRightPosY = { 111 }

function lissaSceneStarted()
    vbank(0)
    core_pal_apply(_imgLissaBG.palette)
    core_pal_apply_color(13, 26, 29, 33) -- Perspective lines color at index 13.
    vbank(1)
    core_pal_apply(_palLissa)
end

function lissaSceneCompleted()
    -- Reset vbanks and start next anim.
    vbank(1)
    cls(0)
    vbank(0)
    core_anim_start(_schedule.anims[AnimNames.TRANSIT2SHADOW], _timer)
end

core_anim_add_anim(AnimNames.LISSA, _schedule, core_anim_new(false,
    {
        core_anim_new_keyframe_tween(0, 10, { 0 }, { 5000 }, core_easing_quadratic_in, _lissaCount),
        core_anim_new_keyframe_tween(0, 10, { 0 }, { 200 }, core_easing_quadratic_in_out, _lissaSpeed),
        core_anim_new_keyframe_tween(18.5, 24.5, { 5000 }, { 0 }, core_easing_quadratic_in_out, _lissaCount),
        --
        core_anim_new_keyframe_tween(7.5, 10.5, { 0 }, { 20 }, core_easing_quadratic_in_out, _lissaTranslateZ),
        core_anim_new_keyframe_tween(18, 20.5, { 20 }, { -10 }, core_easing_quadratic_in_out, _lissaTranslateZ),
        core_anim_new_keyframe_tween(20.5, 24.5, { -10 }, { 0 }, core_easing_quadratic_in_out, _lissaTranslateZ),
        --
        core_anim_new_keyframe_tween(1, 3, { 10 }, { 3 }, core_easing_lerp, _lissaBGFlickerStrength),
    },
    lissaSceneStarted,
    lissaSceneCompleted
))

-------------------------------------------------------------------------------
--- TRANSIT2SHADOW ANIM

_shadowCubeScale = { 0 }
_cubeRandRange1 = { 0 }
_cubeRandRange2 = { 0 }
_shadowCubeAmpX = { 0 }
_shadowCubeAmpY = { 0 }
_cubeRenderOffY = { 0 }
_shadowCubeSineOffset = 0

function transit2ShadowSceneStarted()
    _lissaCoverTilesProgress[1] = 0 -- Only needed if scene is started directly.
    vbank(0)
    core_pal_apply(_imgLissaBG.palette)
    core_pal_apply_color(13, 26, 29, 33) -- Perspective lines color at index 13.
    vbank(1)
    core_pal_apply(_palShadow)
    core_pal_apply_color(10, 255, 255, 255) -- White index 10 for text color.
end

function lissaSceneCompleted()
    core_anim_start(_schedule.anims[AnimNames.SHADOW], _timer)
end

core_anim_add_anim(AnimNames.TRANSIT2SHADOW, _schedule, core_anim_new(false,
    {
        core_anim_new_keyframe_tween(0, 2, { 0 }, { 1.4 }, core_easing_back_out, _shadowCubeScale),
        core_anim_new_keyframe_tween(2, 5, { 1.4 }, { 1 }, core_easing_quadratic_out, _shadowCubeScale),
        --
        core_anim_new_keyframe_tween(4, 6, { 0 }, { 4 }, core_easing_quadratic_in, _shadowCubeAmpY), -- Must match with SHADOW anim.
        core_anim_new_keyframe_tween(4, 6, { 0 }, { 2 }, core_easing_quadratic_in, _shadowCubeAmpX), -- Must match with SHADOW anim.
        --
        core_anim_new_keyframe_tween(.1, .5, { 2 }, { 5 }, core_easing_lerp, _cubeRandRange1),
        core_anim_new_keyframe_tween(.1, .5, { 2 }, { 6 }, core_easing_lerp, _cubeRandRange2),
        --
        core_anim_new_keyframe_tween(1.25, 4.25, { 0 }, { 2 }, core_easing_lerp, _lissaCoverTilesProgress),
        --
        core_anim_new_keyframe_tween(3.2, 3.7, { 111 }, { 140 }, core_easing_cubic_in, _lissaLogoLeftPosY),
        core_anim_new_keyframe_tween(3.4, 3.9, { 111 }, { 180 }, core_easing_cubic_in, _lissaLogoRightPosY),
    },
    transit2ShadowSceneStarted,
    lissaSceneCompleted
))

-------------------------------------------------------------------------------
--- SHADOW ANIM

_shadowCubeOffX = { 0 }
_shadowCubeOffY = { 0 }
_shadowCubeOffZ = { 0 }
_shadowLightIntensity = { 0 }
_shadowFadeLight = { 0 }
_shadowFadeTexture = { 0 }

function shadowSceneStarted()
    vbank(1)
    core_pal_apply(_palShadow)
    vbank(0)
    core_pal_apply(_palShadow)
    cls(0)
end

function shadowSceneCompleted()
    -- Reset vbanks and start next anim.
    vbank(1)
    cls(0)
    vbank(0)
    core_anim_start(_schedule.anims[AnimNames.HAVE2RUN], _timer)
end

core_anim_add_anim(AnimNames.SHADOW, _schedule, core_anim_new(false,
    {
        core_anim_new_keyframe_tween(0, 1, { 0 }, { 1 }, core_easing_lerp, _shadowFadeTexture),
        core_anim_new_keyframe_tween(1, 2, { 0 }, { 1 }, core_easing_quadratic_in, _shadowFadeLight),
        --
        core_anim_new_keyframe_tween(0, 1.75, { 4 }, { 3 }, core_easing_lerp, _shadowCubeAmpY),
        core_anim_new_keyframe_tween(0, 1.75, { 2 }, { 4 }, core_easing_lerp, _shadowCubeAmpX),
        --
        core_anim_new_keyframe_tween(1, 5, { 0 }, { 1 }, core_easing_quadratic_out, _shadowLightIntensity),
        core_anim_new_keyframe_tween(5, 8, { 1 }, { 3 }, core_easing_quadratic_in_out, _shadowCubeScale),
        core_anim_new_keyframe_tween(9, 12, { 3 }, { .8 }, core_easing_quadratic_in_out, _shadowCubeScale),
        core_anim_new_keyframe_tween(12, 16, { .8 }, { 4 }, core_easing_quadratic_in_out, _shadowCubeScale),
        --
        core_anim_new_keyframe_tween(16, 20, { 4 }, { 2 }, core_easing_quadratic_in_out, _shadowCubeScale),
        core_anim_new_keyframe_tween(23, 25, { 2 }, { 1 }, core_easing_quadratic_in_out, _shadowCubeScale),
        core_anim_new_keyframe_tween(25, 27, { 0 }, { -10 }, core_easing_quadratic_in, _shadowCubeOffY),
        core_anim_new_keyframe_tween(25, 27, { 1 }, { 0 }, core_easing_quadratic_in, _shadowLightIntensity),
        core_anim_new_keyframe_tween(24, 27, { 4 }, { 6 }, core_easing_quadratic_in_out, _shadowCubeAmpX),
        --
        core_anim_new_keyframe_nop(29.4),
    },
    shadowSceneStarted,
    shadowSceneCompleted
))

-------------------------------------------------------------------------------
--- HAVE2RUN ANIM

_runnersOffX = { -200 }

_runLine1Pos = { 0, 0 }
_runText1ColorIdx = { 1 }
_runText2ColorIdx = { 1 }
_runText3ColorIdx = { 1 }

_runLine2Pos = { 0, 0 }
_runText4ColorIdx = { 1 }
_runText5ColorIdx = { 1 }
_runText6ColorIdx = { 1 }

_runLine3Pos = { 0, 0 }
_runText7ColorIdx = { 1 }
_runText8ColorIdx = { 1 }
_runText9ColorIdx = { 1 }

_runnerFloorLines = {
    { x = { -244 }, y = 88,  ts = 1,   te = 3 },
    { x = { -244 }, y = 92,  ts = 1.9, te = 3.2 },
    { x = { -244 }, y = 97,  ts = 1.8, te = 4.2 },
    { x = { -244 }, y = 100, ts = 1.7, te = 2.7 },
    { x = { -244 }, y = 102, ts = 2.2, te = 4.2 },
    --
    { x = { -244 }, y = 104, ts = .5,  te = 3 },
    { x = { -244 }, y = 105, ts = .5,  te = 3 },
    { x = { -244 }, y = 106, ts = .5,  te = 3 },
    { x = { -244 }, y = 107, ts = .5,  te = 3 },
    --
    { x = { -244 }, y = 109, ts = 2,   te = 4.3 },
    { x = { -244 }, y = 111, ts = 1.3, te = 3.4 },
    { x = { -244 }, y = 114, ts = .8,  te = 4.5 },
    { x = { -244 }, y = 117, ts = 1.5, te = 3.1 },
    { x = { -244 }, y = 122, ts = 1.6, te = 3.6 },
}

function have2runSceneStarted()
    core_pal_apply(_palRunner)
end

core_anim_add_anim(AnimNames.HAVE2RUN, _schedule, core_anim_new(false,
    {
        core_anim_new_keyframe_tween(2, 5, { 0, 0 }, { 0, 80 }, core_easing_quadratic_in, _runLine1Pos),
        core_anim_new_keyframe_tween(5.7, 8.7, { 0, 0 }, { 0, 80 }, core_easing_quadratic_in, _runLine2Pos),
        core_anim_new_keyframe_tween(10, 13, { 0, 0 }, { 0, 80 }, core_easing_quadratic_in, _runLine3Pos),
        --
        core_anim_new_keyframe_tween(.5, 2, { 1 }, { #_runTextFadeColors }, core_easing_lerp, _runText1ColorIdx),
        core_anim_new_keyframe_tween(0, 1.5, { 1 }, { #_runTextFadeColors }, core_easing_lerp, _runText2ColorIdx),
        core_anim_new_keyframe_tween(.7, 2.2, { 1 }, { #_runTextFadeColors }, core_easing_lerp, _runText3ColorIdx),
        --
        core_anim_new_keyframe_tween(4.2, 5.7, { 1 }, { #_runTextFadeColors }, core_easing_lerp, _runText4ColorIdx),
        core_anim_new_keyframe_tween(4.7, 6.2, { 1 }, { #_runTextFadeColors }, core_easing_lerp, _runText5ColorIdx),
        core_anim_new_keyframe_tween(4.4, 5.2, { 1 }, { #_runTextFadeColors }, core_easing_lerp, _runText6ColorIdx),
        --
        core_anim_new_keyframe_tween(2.2, 3.7, { #_runTextFadeColors }, { 1 }, core_easing_lerp, _runText1ColorIdx),
        core_anim_new_keyframe_tween(2.5, 4, { #_runTextFadeColors }, { 1 }, core_easing_lerp, _runText2ColorIdx),
        core_anim_new_keyframe_tween(2, 3.5, { #_runTextFadeColors }, { 1 }, core_easing_lerp, _runText3ColorIdx),
        --
        core_anim_new_keyframe_tween(6.1, 7.6, { #_runTextFadeColors }, { 1 }, core_easing_lerp, _runText4ColorIdx),
        core_anim_new_keyframe_tween(6.4, 7.9, { #_runTextFadeColors }, { 1 }, core_easing_lerp, _runText5ColorIdx),
        core_anim_new_keyframe_tween(5.9, 7.4, { #_runTextFadeColors }, { 1 }, core_easing_lerp, _runText6ColorIdx),
        --
        core_anim_new_keyframe_tween(8.5, 10, { 1 }, { #_runTextFadeColors }, core_easing_lerp, _runText7ColorIdx),
        core_anim_new_keyframe_tween(9, 10.5, { 1 }, { #_runTextFadeColors }, core_easing_lerp, _runText8ColorIdx),
        core_anim_new_keyframe_tween(8.7, 10.2, { 1 }, { #_runTextFadeColors }, core_easing_lerp, _runText9ColorIdx),
        --
        core_anim_new_keyframe_tween(10.2, 11.7, { #_runTextFadeColors }, { 1 }, core_easing_lerp, _runText7ColorIdx),
        core_anim_new_keyframe_tween(10.95, 12.45, { #_runTextFadeColors }, { 1 }, core_easing_lerp, _runText8ColorIdx),
        core_anim_new_keyframe_tween(10.7, 12.2, { #_runTextFadeColors }, { 1 }, core_easing_lerp, _runText9ColorIdx),

        -- Floor lines are added in init function later.

        core_anim_new_keyframe_tween(17, 20, { -200 }, { 0 }, core_easing_lerp, _runnersOffX),
    },
    have2runSceneStarted,
    function() core_anim_start(_schedule.anims[AnimNames.RUNNER], _timer) end
))

-------------------------------------------------------------------------------
--- RUNNER ANIM

_runnerGreetsFactor = 0
_runnerSwipesX = {}
_runnerSwipesEnabled = true
_runnerBlackBayerX = { 0 }

_runnerTransitionOutTopY = { -70 }
_runnerTransitionOutBottomY = { 136 }

_runnerBulkEnabled = false

_doggyPosX = { -60 }
_gradSunPosY = { -10 }

_runnerTreesClipPosX = { 0 }
_runnerSkylineFrontClipPosX = { 0 }
_runnerSkylineBackClipPosX = { 0 }

_floorFlashFactor = { 1 }
_runnerGradBGMixFactor = { 0 } -- 0: Bright, 1: Nighttime.
_runnerSunSpeed = { 9 }
_sunPosY = { 0 }

function runnerSceneStarted()
    core_pal_apply(_palRunner)
end

core_anim_add_anim(AnimNames.RUNNER, _schedule, core_anim_new(false,
    {
        -- Swipe anim is added in init function later.
        --
        core_anim_new_keyframe_fire_once(5, function()
            _runnerGreetsFactor = 1
        end),
        core_anim_new_keyframe_tween(2, 3, { 0 }, { -20 }, core_easing_lerp, _runnerBlackBayerX),
        core_anim_new_keyframe_fire_once(7, function() _runnerSwipesEnabled = false end),

        core_anim_new_keyframe_tween(0, 6, { 1 }, { .3 }, core_easing_lerp, _floorFlashFactor),
        --
        core_anim_new_keyframe_tween(33, 38, { 0 }, { 1 }, core_easing_lerp, _runnerGradBGMixFactor),
        core_anim_new_keyframe_tween(32, 37, { 9 }, { 20 }, core_easing_lerp, _runnerSunSpeed),
        core_anim_new_keyframe_tween(32, 39, { 0 }, { 40 }, core_easing_quadratic_in, _sunPosY),
        core_anim_new_keyframe_tween(32, 38, { -10 }, { -80 + 50 }, core_easing_quadratic_in, _gradSunPosY),
        --
        core_anim_new_keyframe_tween(38, 43, { 0 }, { 300 }, core_easing_quadratic_in, _runnersOffX),
        --
        core_anim_new_keyframe_tween(43.75, 46.5, { 0 }, { 280 }, core_easing_quadratic_out, _runnerTreesClipPosX),
        core_anim_new_keyframe_tween(44.25, 46.5, { 0 }, { 280 }, core_easing_quadratic_out, _runnerSkylineBackClipPosX),
        core_anim_new_keyframe_tween(44.75, 46.5, { 0 }, { 280 }, core_easing_quadratic_out, _runnerSkylineFrontClipPosX),
        --

        core_anim_new_keyframe_fire_once(38, function()
            _runnerBulkEnabled = true
        end),

        core_anim_new_keyframe_tween(40.7, 44.7, { -60 }, { 260 }, core_easing_quadratic_in, _doggyPosX),

        -- Stripes anims are added in init function. See `delaySwipeOut`.

        --
        core_anim_new_keyframe_tween(45, 46.323333, { -70 }, { 0 }, core_easing_quadratic_out, _runnerTransitionOutTopY),
        core_anim_new_keyframe_tween(45, 46.323333, { 136 }, { 69 }, core_easing_quadratic_out,
            _runnerTransitionOutBottomY),
    },
    runnerSceneStarted,
    function() core_anim_start(_schedule.anims[AnimNames.TRANSIT2VOX], _timer) end
))

-------------------------------------------------------------------------------
--- TRANSIT2VOX ANIM

_voxCoverTopY = { 0 }
_voxCoverBottomY = { 0 }
_voxLinesSpectroxFactor = { 1 }
_voxStripesPulseFactor = { 0 }

_stripes = {
    -- 1
    {
        yoff = { -50 },
        { y = -17, h = 2 * 17, c = { 62, 32, 89 }, cf = { 155, 40, 166 } },
        { y = 17,  h = 1,      c = { 52, 28, 79 }, cf = { 131, 34, 148 } },
    },
    -- 2
    {
        yoff = { -50 },
        { y = 19, h = 1, c = { 34, 22, 61 }, cf = { 87, 27, 114 } },
        { y = 20, h = 3, c = { 43, 25, 70 }, cf = { 109, 31, 131 } },
        { y = 23, h = 1, c = { 34, 22, 61 }, cf = { 87, 27, 114 } },
    },
    -- 3
    {
        yoff = { -50 },
        { y = 25, h = 1, c = { 16, 16, 43 }, cf = { 43, 20, 80 } },
        { y = 26, h = 2, c = { 25, 19, 52 }, cf = { 65, 24, 97 } },
        { y = 28, h = 1, c = { 16, 16, 43 }, cf = { 43, 20, 80 } },
    },
    -- 4
    {
        yoff = { -50 },
        { y = 31, h = 1, c = { 7, 13, 34 },  cf = { 13, 3, 64 } },
        { y = 32, h = 1, c = { 16, 16, 43 }, cf = { 43, 20, 80 } },
        { y = 33, h = 1, c = { 7, 13, 34 },  cf = { 13, 3, 64 } },
    },
    ---
    -- 8
    {
        yoff = { 50 },
        { y = 118, h = 1,      c = { 52, 28, 79 }, cf = { 155, 40, 166 } },
        { y = 119, h = 2 * 17, c = { 62, 32, 89 }, cf = { 131, 34, 148 } },
    },
    -- 7
    {
        yoff = { 50 },
        { y = 112, h = 1, c = { 34, 22, 61 }, cf = { 87, 27, 114 } },
        { y = 113, h = 3, c = { 43, 25, 70 }, cf = { 109, 31, 131 } },
        { y = 116, h = 1, c = { 34, 22, 61 }, cf = { 87, 27, 114 } },
    },
    -- 6
    {
        yoff = { 50 },
        { y = 107, h = 1, c = { 16, 16, 43 }, cf = { 43, 20, 80 } },
        { y = 108, h = 2, c = { 25, 19, 52 }, cf = { 65, 24, 97 } },
        { y = 110, h = 1, c = { 16, 16, 43 }, cf = { 43, 20, 80 } },
    },
    -- 5
    {
        yoff = { 50 },
        { y = 102, h = 1, c = { 7, 13, 34 },  cf = { 13, 3, 64 } },
        { y = 103, h = 1, c = { 16, 16, 43 }, cf = { 43, 20, 80 } },
        { y = 104, h = 1, c = { 7, 13, 34 },  cf = { 13, 3, 64 } },
    },
}

_voxTxts = {
    {
        txt = "days",
        x = { 54 },
        col = { 1 },
        amp = { 0 },
    },
    {
        txt = "the",
        x = { 154 },
        col = { 1 },
        amp = { 0 },
    },
    {
        txt = "you",
        x = { 90 },
        col = { 1 },
        amp = { 0 },
    },
    {
        txt = "some",
        x = { 18 },
        col = { 1 },
        amp = { 0 },
    },
    {
        txt = "feel",
        x = { 118 },
        col = { 1 },
        amp = { 0 },
    },
    {
        txt = "twist...",
        x = { 182 },
        col = { 1 },
        amp = { 0 },
    },
}

function transit2VoxSceneStarted()
    core_pal_apply(_imgSpectrox.palette)
end

core_anim_add_anim(AnimNames.TRANSIT2VOX, _schedule, core_anim_new(false,
    {
        core_anim_new_keyframe_tween(0, 2, { 0 }, { -65 }, core_easing_bounce_once_out, _voxCoverTopY),
        core_anim_new_keyframe_tween(0, 2, { 0 }, { 65 }, core_easing_bounce_once_out, _voxCoverBottomY),
        --
        core_anim_new_keyframe_tween(3, 4, { 1 }, { 0 }, core_easing_lerp, _voxLinesSpectroxFactor),
        --
        core_anim_new_keyframe_tween(1.3, 2.3, { -50 }, { 0 }, core_easing_back_out, _stripes[1].yoff),
        core_anim_new_keyframe_tween(1.4, 2.5, { -50 }, { 0 }, core_easing_back_out, _stripes[2].yoff),
        core_anim_new_keyframe_tween(1.5, 2.7, { -50 }, { 0 }, core_easing_back_out, _stripes[3].yoff),
        core_anim_new_keyframe_tween(1.6, 2.9, { -50 }, { 0 }, core_easing_back_out, _stripes[4].yoff),
        --
        core_anim_new_keyframe_tween(1.3, 2.3, { 50 }, { 0 }, core_easing_back_out, _stripes[5].yoff),
        core_anim_new_keyframe_tween(1.4, 2.5, { 50 }, { 0 }, core_easing_back_out, _stripes[6].yoff),
        core_anim_new_keyframe_tween(1.5, 2.7, { 50 }, { 0 }, core_easing_back_out, _stripes[7].yoff),
        core_anim_new_keyframe_tween(1.6, 2.9, { 50 }, { 0 }, core_easing_back_out, _stripes[8].yoff),

        -- Generated color fade keys come here (see `initTransit2VoxTextsAnims`). Last generated end time at 5.7s.
    },
    transit2VoxSceneStarted,
    function() core_anim_start(_schedule.anims[AnimNames.VOX], _timer) end
))


-------------------------------------------------------------------------------
--- VOX ANIM

function voxSceneStarted()
    vbank(1)
    core_pal_apply(_voxPal)
    vbank(0)
    core_pal_apply(_imgSpectrox.palette)

    _shadowCubeColors = { 14, 8, 3 }
    _voxLogoFlashFactor = .5
end

function voxSceneCompleted()
    -- Reset vbanks and start next anim.
    vbank(1)
    cls(0)
    vbank(0)
    core_anim_start(_schedule.anims[AnimNames.WATERFIELD], _timer)
end

_voxAng = { M_PI_4 }
_voxSpdX = { 20 }
_voxSpdY = { 20 }
_voxHoriz = { _voxCam.horizon }
_voxHeight = { _voxCam.height }
_voxScale = { _voxCam.scale }
_voxDitherDist = { 50 }
_voxDitherHoriz = { _voxCam.dist, _voxCam.dist } -- far, near
_voxYOff = { 8 }
_voxRectX = { 0 }
_voxRectAmp = { 0 }
_voxIsTwister = true
_voxGradStripedEnabled = true
_voxShadePulseEnabled = false
_voxLightAmp = { 15 }
_voxFlashFactor = { 0 }
_waterfieldBGSpeed = { 0 }
_cubeEnabled = false

_voxOrbitModeEnabled = false
_voxOrbitCamRadius = 20
_voxOrbitCamTargetPos = { 0, 0 }

_voxGradVbank0Mix = { 0 }
_voxGradVbank0Src = _voxGradStripes
_voxGradVbank0Dst = _voxGradLakes1

_voxGradVbank1Mix = { 1 }
_voxGradVbank1Src = _voxGradLakes1 -- Colorful.
_voxGradVbank1Dst = _voxGradLakes2 -- Blue.

_voxTimeSpeedX = { 0 }

function bdrVoxOffZero()
    return 0
end

function bdrVox0Waterfield()
    return (_waterBGPosY) // 1
end

function bdrVox1OffScroll()
    return (_voxCam.horizon + 100 * _localTimer.elapsed) // 1
end

function bdrVox1OffCube()
    return 30
end

_bdrVox0OffFunc = bdrVoxOffZero
_bdrVox1OffFunc = bdrVox1OffScroll

core_anim_add_anim(AnimNames.VOX, _schedule, core_anim_new(false,
    {
        core_anim_new_keyframe_tween(8.55, 8.6, { -6 }, { -6 }, core_easing_lerp, _voxTimeSpeedX),
        core_anim_new_keyframe_tween(8.7, 9.2, { -6 }, { -.5 }, core_easing_lerp, _voxTimeSpeedX),
        core_anim_new_keyframe_tween(11.2, 11.8, { -.5 }, { -6 }, core_easing_lerp, _voxTimeSpeedX),
        --
        core_anim_new_keyframe_tween(0, 1, { 0 }, { 40 }, core_easing_lerp, _voxRectAmp),
        core_anim_new_keyframe_tween(0, 3, { 0 }, { 240 }, core_easing_lerp, _voxRectX),
        --
        core_anim_new_keyframe_tween(12.71, 13.71, { 0 }, { 1 }, core_easing_lerp, _voxLinesSpectroxFactor),
        --
        core_anim_new_keyframe_tween(13.79, 14.85, { -65 }, { -75 }, core_easing_back_in, _voxCoverTopY),
        core_anim_new_keyframe_tween(13.79, 14.85, { 65 }, { 75 }, core_easing_back_in, _voxCoverBottomY),
        --
        core_anim_new_keyframe_tween(14.77, 17.77, { .2 }, { 1 }, core_easing_lerp, _voxStripesPulseFactor),
        --
        core_anim_new_keyframe_fire_once(21.3, function()
            _voxGradStripedEnabled = false
            _voxLogoFlashFactor = 0
        end),
        --
        core_anim_new_keyframe_tween(0.1, 1.1, { 18 }, { -50 }, core_easing_exponential_in, _voxTxts[4].x),
        core_anim_new_keyframe_tween(0.3, 1.3, { 54 }, { -50 }, core_easing_exponential_in, _voxTxts[1].x),
        core_anim_new_keyframe_tween(0.5, 1.5, { 90 }, { -50 }, core_easing_exponential_in, _voxTxts[3].x),
        core_anim_new_keyframe_tween(0.7, 1.7, { 118 }, { -50 }, core_easing_exponential_in, _voxTxts[5].x),
        core_anim_new_keyframe_tween(0.9, 1.9, { 154 }, { -50 }, core_easing_exponential_in, _voxTxts[2].x),
        core_anim_new_keyframe_tween(1.1, 2.1, { 182 }, { -50 }, core_easing_exponential_in, _voxTxts[6].x),
        --
        core_anim_new_keyframe_tween(0.1, 1.1, { 0 }, { 25 }, core_easing_lerp, _voxTxts[4].amp),
        core_anim_new_keyframe_tween(0.3, 1.3, { 0 }, { 18 }, core_easing_lerp, _voxTxts[1].amp),
        core_anim_new_keyframe_tween(0.5, 1.5, { 0 }, { 13 }, core_easing_lerp, _voxTxts[3].amp),
        core_anim_new_keyframe_tween(0.7, 1.7, { 0 }, { 18 }, core_easing_lerp, _voxTxts[5].amp),
        core_anim_new_keyframe_tween(0.9, 1.9, { 0 }, { 16 }, core_easing_lerp, _voxTxts[2].amp),
        core_anim_new_keyframe_tween(1.1, 2.1, { 0 }, { 16 }, core_easing_lerp, _voxTxts[6].amp),
        --
        core_anim_new_keyframe_tween(0.1, 1.1, { #_voxTextFadeColors }, { 1 }, core_easing_lerp, _voxTxts[4].col),
        core_anim_new_keyframe_tween(0.3, 1.3, { #_voxTextFadeColors }, { 1 }, core_easing_lerp, _voxTxts[1].col),
        core_anim_new_keyframe_tween(0.5, 1.5, { #_voxTextFadeColors }, { 1 }, core_easing_lerp, _voxTxts[3].col),
        core_anim_new_keyframe_tween(0.7, 1.7, { #_voxTextFadeColors }, { 1 }, core_easing_lerp, _voxTxts[5].col),
        core_anim_new_keyframe_tween(0.9, 1.9, { #_voxTextFadeColors }, { 1 }, core_easing_lerp, _voxTxts[2].col),
        core_anim_new_keyframe_tween(1.1, 2.1, { #_voxTextFadeColors }, { 1 }, core_easing_lerp, _voxTxts[6].col),
        ---
        -- Twister transformation.
        core_anim_new_keyframe_tween(14, 15, { 50 }, { 40 }, core_easing_quadratic_in_out, _voxDitherDist),
        core_anim_new_keyframe_tween(15, 16, { 40 }, { 50 }, core_easing_quadratic_in_out, _voxDitherDist),
        core_anim_new_keyframe_tween(19.1, 21.11, { 50 }, { 136 }, core_easing_quadratic_in, _voxDitherDist),
        core_anim_new_keyframe_fire_once(22, function() _voxIsTwister = false; end),
        core_anim_new_keyframe_tween(19.1, 21.11, { _voxCam.dist, _voxCam.dist }, { 2350, 2250 }, core_easing_lerp,
            _voxDitherHoriz),
        --
        core_anim_new_keyframe_tween(19.1, 21.11, { 8 }, { 0 }, core_easing_lerp, _voxYOff),
        --
        core_anim_new_keyframe_tween(18.5, 21.11, { -105 }, { 0 }, core_easing_quadratic_in_out, _voxHoriz),
        core_anim_new_keyframe_tween(18.5, 21.11, { 1475 }, { 600 }, core_easing_quadratic_in_out, _voxHeight),
        core_anim_new_keyframe_tween(18.5, 21.11, { 240 }, { 180 }, core_easing_quadratic_in_out, _voxScale),
        --
        core_anim_new_keyframe_tween(20.5, 23.11, { M_PI_4 }, { M_PI_2 }, core_easing_quadratic_in_out, _voxAng),
        core_anim_new_keyframe_tween(21.1, 23.11, { 20 }, { 0 }, core_easing_quadratic_in_out, _voxSpdX),
        core_anim_new_keyframe_tween(21.1, 23.11, { 20 }, { 40 }, core_easing_quadratic_in_out, _voxSpdY),
        --
        ---
        -- Tall mountains.
        core_anim_new_keyframe_fire_once(25.38, function()
            _voxAng[1] = M_PI_4
            _voxCam.pos.x = 0
            _voxCam.pos.y = 0
            _voxHeight[1] = 300
            _voxHoriz[1] = 0
            _voxScale[1] = 480
            _voxSpdX[1] = -30
            _voxSpdY[1] = 20
        end),
        core_anim_new_keyframe_tween(25.38, 28.2, { M_PI_4 }, { -M_PI_4 }, core_easing_quadratic_in_out, _voxAng),
        ---
        -- Cube enters.
        core_anim_new_keyframe_fire_once(29.65, function()
            _cubeEnabled = true
            _cubeRandRange1[1] = 5
            _cubeRandRange2[1] = 6
            _shadowCubeAmpX[1] = 0
            _shadowCubeAmpY[1] = 0
            _shadowCubeOffX[1] = 0
            _shadowCubeOffY[1] = 10
            _shadowCubeOffZ[1] = -2
            _shadowCubeScale[1] = 1.1
            _voxAng[1] = M_PI_4
            _voxHeight[1] = 500
            _voxHoriz[1] = 0
            _voxLightAmp[1] = 3
            _voxOrbitModeEnabled = true
            _voxScale[1] = 400
            _voxSpdX[1] = 30
            _voxSpdY[1] = 10
            _voxOrbitCamTargetPos = { 150, 123 }
        end),

        core_anim_new_keyframe_tween(30.8, 32.5, { 3 }, { 20 }, core_easing_cubic_in_out, _voxLightAmp),
        core_anim_new_keyframe_tween(32.5, 34, { 20 }, { 15 }, core_easing_cubic_in_out, _voxLightAmp),
        --
        core_anim_new_keyframe_tween(32, 35, { 500 }, { 300 }, core_easing_quadratic_in_out, _voxHeight),
        core_anim_new_keyframe_tween(30, 35, { 10 }, { 1 }, core_easing_quadratic_out, _shadowCubeOffY),
        core_anim_new_keyframe_tween(35, 37, { 1 }, { 2 }, core_easing_quadratic_in_out, _shadowCubeOffY),
        core_anim_new_keyframe_tween(32, 35, { 1.1 }, { 1.4 }, core_easing_quadratic_in_out, _shadowCubeScale),
        ---
        -- Cube enters 2.
        core_anim_new_keyframe_fire_once(33.92, function()
            _voxHeight[1] = 200
            _voxHoriz[1] = 50
            _voxOrbitCamTargetPos = { 30, 123 }
            _voxScale[1] = 250
            _voxShadePulseEnabled = true
        end),
        ---
        -- Cube in distance over lake.
        core_anim_new_keyframe_fire_once(38.18, function()
            _bdrVox1OffFunc = bdrVox1OffCube
            _cubeRandRange1[1] = 2
            _cubeRandRange2[1] = 2
            _shadowCubeAmpX[1] = 0
            _shadowCubeAmpY[1] = .5
            _shadowCubeOffX[1] = 2
            _shadowCubeOffY[1] = 9.5
            _shadowCubeOffZ[1] = -20
            _shadowCubeScale[1] = 1.1
            _voxAng[1] = M_PI_4
            _voxCam.pos.x = 489.6
            _voxCam.pos.y = 189.8
            _voxHeight[1] = 250
            _voxHoriz[1] = 30
            _voxLightAmp[1] = 3
            _voxOrbitModeEnabled = false
            _voxScale[1] = 200
            _voxShadePulseEnabled = false
            _voxSpdX[1] = 12
            _voxSpdY[1] = -12
        end),
        core_anim_new_keyframe_tween(39.31, 43.31, { 3 }, { 20 }, core_easing_cubic_in_out, _voxLightAmp),
        core_anim_new_keyframe_tween(45.81, 46.71, { 20 }, { 3 }, core_easing_cubic_in_out, _voxLightAmp),
        core_anim_new_keyframe_tween(38.18, 46.71, { 30 }, { -30 }, core_easing_lerp, _shadowCubeOffX),
        core_anim_new_keyframe_fire_once(41.31, function() _voxGradVbank1Src = _voxGradLakes1; end), -- Must not overlap with flash fade!
        core_anim_new_keyframe_tween(42.81, 44.31, { 1 }, { 0.2 }, core_easing_cubic_in_out, _voxGradVbank1Mix),
        core_anim_new_keyframe_tween(44.31, 45.81, { 0.2 }, { 1 }, core_easing_cubic_in_out, _voxGradVbank1Mix),
        ---
        -- Dive into lake.
        core_anim_new_keyframe_fire_once(46.71, function()
            _bdrVox1OffFunc = bdrVox0Waterfield
            _cubeRandRange1[1] = 5
            _cubeRandRange2[1] = 6
            _shadowCubeAmpX[1] = 2
            _shadowCubeAmpY[1] = 4
            _shadowCubeOffX[1] = 5
            _shadowCubeOffY[1] = -3
            _shadowCubeOffZ[1] = 0
            _shadowCubeScale[1] = 1.1
            _voxAng[1] = M_PI_4
            _voxCam.pos.x = -88.34
            _voxCam.pos.y = -141.34
            _voxHeight[1] = 300
            _voxHoriz[1] = 0
            _voxLightAmp[1] = 15
            _voxScale[1] = 300
            _voxShadePulseEnabled = false
            _voxSpdX[1] = 13.5
            _voxSpdY[1] = 13.5
            _shadowCubeSineOffset = -2.81
        end),

        core_anim_new_keyframe_tween(47.71, 49.71, { 5 }, { -1 }, core_easing_quadratic_in_out, _shadowCubeOffX),
        core_anim_new_keyframe_tween(53.52, 54.52, { -1 }, { 0 }, core_easing_quadratic_in_out, _shadowCubeOffX),
        core_anim_new_keyframe_tween(49.21, 51.41, { -3 }, { 2 }, core_easing_quadratic_in_out, _shadowCubeOffY),
        core_anim_new_keyframe_tween(54.22, 55.22, { 2 }, { 0 }, core_easing_quadratic_in_out, _shadowCubeOffY),
        core_anim_new_keyframe_tween(53.52, 55.22, { 2 }, { 0 }, core_easing_quadratic_in_out, _shadowCubeAmpX),
        core_anim_new_keyframe_tween(53.52, 55.22, { 4 }, { 0 }, core_easing_quadratic_in_out, _shadowCubeAmpY),
        core_anim_new_keyframe_tween(52.52, 55.22, { 1.1 }, { .6 }, core_easing_quadratic_in_out, _shadowCubeScale),
        --
        core_anim_new_keyframe_tween(53.52, 55.22, { 5 }, { 2 }, core_easing_lerp, _cubeRandRange1),
        core_anim_new_keyframe_tween(53.52, 55.22, { 6 }, { 2 }, core_easing_lerp, _cubeRandRange2),
        core_anim_new_keyframe_fire_once(52.52, function() _voxGradVbank1Src = _waterGradBG; end),
        core_anim_new_keyframe_tween(53.52, 54.02, { 0 }, { -60 }, core_easing_quadratic_in, _waterfieldBGSpeed),
        core_anim_new_keyframe_tween(52.52, 54.52, { 1 }, { 0 }, core_easing_lerp, _voxGradVbank1Mix),
        --
        core_anim_new_keyframe_tween(46.71, 53.52, { 0 }, { M_PI_4 }, core_easing_back_out, _voxAng),
        core_anim_new_keyframe_tween(54.72, 55.22, { 300 }, { 60 }, core_easing_quadratic_in, _voxHeight),
        core_anim_new_keyframe_tween(52.52, 55.22, { 0 }, { -150 }, core_easing_back_in, _voxHoriz),
    },
    voxSceneStarted,
    voxSceneCompleted
))

_voxFlashes = {
    { startTime = 21.11, endTime = 22.11 },
    { startTime = 25.38, endTime = 26.38 },
    { startTime = 29.65, endTime = 30.65 },
    { startTime = 33.92, endTime = 34.92 },
    { startTime = 38.18, endTime = 39.18 },
    { startTime = 46.71, endTime = 47.71 }, -- Must not overlap with palette change in anim.
}

-------------------------------------------------------------------------------
--- WATERFIELD ANIM

function startedWaterfieldScene()
    core_pal_apply(_waterPal)

    _cubeEnabled = true
    _shadowCubeColors = { 7, 6, 5 }
    _shadowCubeOffX[1] = 0
    _shadowCubeOffY[1] = 0
    _shadowCubeOffZ[1] = 0
    _shadowCubeScale[1] = .5
    _cubeRandRange1[1] = 5
    _cubeRandRange2[1] = 6
    _shadowCubeAmpX[1] = 0
    _shadowCubeAmpY[1] = 0
end

_waterBGParticlesSpawnEnabled = true
_waterGradVbank0Mix = { 1 }
_waterGradMixEnabled = true
_waterParticlesSpd = { 12 }
_waterTextPosSpd = 0
_waterCubeBubblesPosVarX = { 10 }
_waterCubeColors = {
    { 0, 0, 0 },
    { 0, 0, 0 },
    { 0, 0, 0 },
}
_underwaterPurpleMix = { 0 }

core_anim_add_anim(AnimNames.WATERFIELD, _schedule, core_anim_new(false,
    {
        core_anim_new_keyframe_tween(0, 1.5, { 8 }, { 20 }, core_easing_lerp, _waterCubeBubblesPosVarX),
        core_anim_new_keyframe_tween(0, 2.2, { 21.6, 113.4, 172.8 }, { 16.5, 29, 89 }, core_easing_lerp,
            _waterCubeColors[1]),
        core_anim_new_keyframe_tween(0, 2.2, { 21.6, 113.4, 172.8 }, { 141, 74, 155 }, core_easing_lerp,
            _waterCubeColors[2]),
        core_anim_new_keyframe_tween(0, 2.2, { 21.6, 113.4, 172.8 }, { 196, 224, 226 }, core_easing_lerp,
            _waterCubeColors[3]),

        core_anim_new_keyframe_tween(2.5, 4.2, { 16.5, 29, 89 }, { 196, 224, 226 }, core_easing_lerp, _waterCubeColors
            [1]),
        core_anim_new_keyframe_tween(2.5, 4.2, { 141, 74, 155 }, { 196, 224, 226 }, core_easing_lerp, _waterCubeColors
            [2]),

        core_anim_new_keyframe_tween(0, 2, { 2 }, { 5 }, core_easing_lerp, _cubeRandRange1),
        core_anim_new_keyframe_tween(0, 2, { 2 }, { 6 }, core_easing_lerp, _cubeRandRange2),

        core_anim_new_keyframe_tween(0, 2, { 0 }, { -2 }, core_easing_quadratic_out, _shadowCubeOffY),
        core_anim_new_keyframe_tween(2, 4, { -2 }, { 2.2 }, core_easing_quadratic_in_out, _shadowCubeOffY),
        core_anim_new_keyframe_tween(4, 8, { 2.2 }, { -8 }, core_easing_quadratic_in, _shadowCubeOffY),
        core_anim_new_keyframe_tween(2, 5, { 0 }, { 2 }, core_easing_lerp, _shadowCubeAmpX),
        core_anim_new_keyframe_tween(0, 2, { .5 }, { 1.1 }, core_easing_lerp, _shadowCubeScale),
        core_anim_new_keyframe_tween(2.5, 4.5, { 5 }, { 0 }, core_easing_lerp, _cubeRandRange1),
        core_anim_new_keyframe_tween(2.5, 4.5, { 6 }, { 0 }, core_easing_lerp, _cubeRandRange2),
        core_anim_new_keyframe_fire_once(3, function()
            _waterCubeBubblesEmitter.isParticleSpawnEnabled = false
            _waterCubeBubblesEmitter.isParticleRenderEnabled = false
        end),

        core_anim_new_keyframe_fire_once(8, function() _cubeEnabled = false end),

        core_anim_new_keyframe_fire_once(0, function() _waterBGPosY = 0 end),
        core_anim_new_keyframe_tween(0, 2.5, { -60 }, { -7.5 }, core_easing_quadratic_out, _waterfieldBGSpeed),
        core_anim_new_keyframe_tween(1, 2.7, { 12 }, { 2 }, core_easing_quadratic_out, _waterParticlesSpd),
        core_anim_new_keyframe_tween(6, 12, { 2 }, { 5 }, core_easing_quadratic_in, _waterParticlesSpd),
        core_anim_new_keyframe_fire_once(10, function()
            _waterTextPosSpd = -15 / 60 -- Move scroller by 15px/s.
        end),

        core_anim_new_keyframe_tween(0, 1.25, { .5 }, { 1 }, core_easing_cubic_in_out, _waterGradVbank0Mix),
        core_anim_new_keyframe_fire_once(1.25, function() _waterGradMixEnabled = false end),

        core_anim_new_keyframe_nop(63.73),

        core_anim_new_keyframe_tween(50, 60, { 0 }, { 1 }, core_easing_lerp, _underwaterPurpleMix),
    },
    startedWaterfieldScene,
    function() core_anim_start(_schedule.anims[AnimNames.UNDERWATER], _timer) end
))

-------------------------------------------------------------------------------
--- UNDERWATER ANIM

function underwaterSceneStarted()
    vbank(0)
    core_pal_apply(_waterPal)
    vbank(1)
    core_pal_apply(_imgSideRocksLeft.palette)

    _shadowCubeColors = { 13, 0, 0 }
    _shadowCubeOffX[1] = 0
    _shadowCubeOffY[1] = 1.3
    _shadowCubeOffZ[1] = 0
    _shadowCubeScale[1] = 1.1
    _cubeRandRange1[1] = 0
    _cubeRandRange2[1] = 0
    _shadowCubeAmpX[1] = 0
    _shadowCubeAmpY[1] = 0

    _waterParticlesSpd[1] = 5
    _coverLinesFactor[1] = 0
end

function underwaterSceneCompleted()
    -- Reset vbanks and start next anim.
    vbank(1)
    cls(0)
    vbank(0)
    cls(0)
    exit()
end

_underwaterBGPosY = { 136 }
_underwaterFGPosY = { 136 }
_underwaterSandPosY = { 136 }
_underwaterWaveyKelpPosY = { 136 }
_underwaterGradPosY = { 136 }
_underwaterLightMix = { 0 }
_underwaterEndMask = { 60 }
_underwaterLogoMaskPosY = { 0 } -- Note: Values "0" and "-120 are also used to enable/disable parts of the effect.
_underwaterEndLogoPosY = { 0 }
_underwaterEndLogoAmp = { 3 }
_underwaterLogoTopmost = false

_underwaterBGParticlesEnabled = true -- Enables/disables: BG water particles, end scroller, scroller BDR and cube flicker.

core_anim_add_anim(AnimNames.UNDERWATER, _schedule, core_anim_new(false,
    {
        core_anim_new_keyframe_tween(0, 15, { 136 }, { 0 }, core_easing_quadratic_out, _underwaterGradPosY),
        core_anim_new_keyframe_tween(5, 15, { 136 }, { 0 }, core_easing_quadratic_out, _underwaterFGPosY),
        core_anim_new_keyframe_tween(8, 15, { 136 }, { 0 }, core_easing_quadratic_out, _underwaterSandPosY),
        core_anim_new_keyframe_tween(9.2, 15, { 136 }, { 0 }, core_easing_quadratic_out, _underwaterWaveyKelpPosY),
        core_anim_new_keyframe_tween(7.4, 15, { 136 }, { 0 }, core_easing_quadratic_out, _underwaterBGPosY),
        core_anim_new_keyframe_fire_once(2.5, function() _waterBGParticlesSpawnEnabled = false; end),
        core_anim_new_keyframe_fire_once(8.9, function()
            vbank(0)
            core_pal_apply(_imgSand.palette)
            _underwaterBGParticlesEnabled = false
        end),

        core_anim_new_keyframe_tween(7, 15, { 0 }, { 1 }, core_easing_lerp, _underwaterLightMix),
        core_anim_new_keyframe_fire_once(18, function()
            resetEmitter(_underwaterCubeEmitter)
            _underwaterCubeEmitter.isParticleSpawnEnabled = true
            _underwaterCubeEmitter.isParticleRenderEnabled = true
        end),

        core_anim_new_keyframe_fire_once(7, function()
            resetEmitter(_underwaterEmitter)
            _underwaterEmitter.isParticleSpawnEnabled = true
            _underwaterEmitter.isParticleRenderEnabled = true
        end),

        core_anim_new_keyframe_tween(18.75, 21, { 0 }, { -120 }, core_easing_lerp, _underwaterLogoMaskPosY),
        core_anim_new_keyframe_tween(18.75, 21, { 0 }, { 3 }, core_easing_lerp, _underwaterEndLogoAmp),
        --
        core_anim_new_keyframe_fire_once(33, function() _underwaterLogoTopmost = true; end),
        core_anim_new_keyframe_tween(34, 36, { 3 }, { 0 }, core_easing_lerp, _underwaterEndLogoAmp),
        core_anim_new_keyframe_tween(35, 38, { 0 }, { 1 }, core_easing_lerp, _coverLinesFactor),
        core_anim_new_keyframe_tween(36, 39, { 0 }, { 30 }, core_easing_quadratic_in_out, _underwaterEndLogoPosY),
        core_anim_new_keyframe_nop(43),
    },
    underwaterSceneStarted,
    underwaterSceneCompleted
))

-------------------------------------------------------------------------------
--- HIDDEN ANIM

_hiddenCoverProgress = { 0 }

function hiddenSceneStarted()
    core_pal_apply_color(0, 0, 0, 0)        -- 0: BG.

    core_pal_apply_color(12, 28, 27, 31)    -- 12: Scroll font BG color.
    core_pal_apply_color(13, 179, 185, 209) -- 13: Scroll font highlight.
    core_pal_apply_color(14, 0, 0, 0)       -- 14: Scroll font cut pixels.
    core_pal_apply_color(15, 139, 147, 175) -- 15: Scroll font color.


    -- Set font highlights in sprite sheet.
    for i = 65, 90 do
        if i ~= 74 and i ~= 84 and i ~= 90 and i ~= 72 and i ~= 82 and i ~= 68 then -- J, T, Z, H, R, D
            core_sprite_pix(256 + i, 0, 0, 14)

            if i ~= 73 and i ~= 70 then -- I, F
                core_sprite_pix(256 + i, 6, 0, 14)
            end

            core_sprite_pix(256 + i, 0, 1, 13)
            core_sprite_pix(256 + i, 1, 0, 13)
        end
    end

    core_sprite_pix(256 + 66, 5, 0, 14) -- B
    core_sprite_pix(256 + 33, 0, 0, 14) -- !
    core_sprite_pix(256 + 74, 0, 2, 14) -- J
    core_sprite_pix(256 + 68, 6, 4, 14) -- D

    core_sprite_pix(256 + 37, 1, 1, 13) -- %
    core_sprite_pix(256 + 37, 2, 0, 13) -- %
    core_sprite_pix(256 + 39, 0, 0, 13) -- '
    core_sprite_pix(256 + 44, 0, 4, 13) -- ,
    core_sprite_pix(256 + 46, 0, 4, 13) -- .
    core_sprite_pix(256 + 68, 0, 0, 13) -- D

    -- Modify R.
    core_sprite_pix(256 + 82, 6, 1, 15)
    core_sprite_pix(256 + 82, 6, 3, 0)
    core_sprite_pix(256 + 82, 4, 3, 15)
    core_sprite_pix(256 + 82, 4, 1, 0)
    core_sprite_pix(256 + 82, 0, 0, 13) -- Highlight.
end

core_anim_add_anim(AnimNames.HIDDEN, _schedule, core_anim_new(false,
    {
        core_anim_new_keyframe_nop(604800),
        core_anim_new_keyframe_tween(1, 4, { 0 }, { 1 }, core_easing_lerp, _hiddenCoverProgress),
    },
    hiddenSceneStarted,
    function() exit() end
))

-------------------------------------------------------------------------------

-------------------------------------------------------------------------------
--- INIT
-------------------------------------------------------------------------------

function initFont()
    _imgFont = core_pcx_decode(RES_PCX_FONT)
    core_sprite_transfer_sheet(_imgFont, 1)
end

--- SCREEN_FADER INIT

function initScreenFaderScene()
    _palTic = core_pal_grab()
    _screenTic = core_image_from_canvas(0, 0, 240, 136)
end

--- PRESENTS INIT

function initPresentsScene()
end

--- TITLE INIT

function initTitleScene()
    _imgPrelude = core_pcx_decode(RES_PCX_PRELUDE)
    _imgNeobyte = core_pcx_decode(RES_PCX_NEOBYTE)

    _palTitleHighlightOnly = core_util_deep_copy(_imgNeobyte.palette)
    -- Set all palette colors to black but index 12 (1-based).
    -- Index 12 is used to highlight portions of the logo in the `highlightLine` function.
    for i = 1, 16 do
        if i ~= 12 then
            core_pal_set_color(_palTitleHighlightOnly, i, 0, 0, 0)
        end
    end
end

--- BLOB INIT

function createCirclePoints(cnt, r, hasCenter)
    local points = {}
    for i = 1, cnt do
        local angle = (M_TAU / cnt) * (i - 1)
        points[i] = { 0, r * sin(angle), r * cos(angle) }
    end

    if hasCenter then
        table_insert(points, { 0, 0, 0 })
    end

    return points
end

function createIcosahedron(s, hasCenter)
    local points = {}
    -- See: https://math.stackexchange.com/a/2174667
    local phi = (1 + math.sqrt(5)) / 2
    table_insert(points, { -s, phi * s, 0 })
    table_insert(points, { s, phi * s, 0 })
    table_insert(points, { -s, -phi * s, 0 })
    table_insert(points, { s, -phi * s, 0 })

    table_insert(points, { 0, -s, phi * s })
    table_insert(points, { 0, s, phi * s })
    table_insert(points, { 0, -s, -phi * s })
    table_insert(points, { 0, s, -phi * s })

    table_insert(points, { phi * s, 0, -s })
    table_insert(points, { phi * s, 0, s })
    table_insert(points, { -phi * s, 0, -s })
    table_insert(points, { -phi * s, 0, s })

    if hasCenter then
        table_insert(points, { 0, 0, 0 })
    end

    return points
end

function createCubePoints(s, hasCenter)
    points = {
        { -s, s,  -s },
        { -s, -s, -s },
        { s,  -s, -s },
        { s,  s,  -s },
        { -s, s,  s },
        { -s, -s, s },
        { s,  -s, s },
        { s,  s,  s },
    }

    if hasCenter then
        table_insert(points, { 0, 0, 0 })
    end

    return points
end

function initBlobsScene()
    table_insert(_blobShapes, createIcosahedron(1.6, false))   --1
    table_insert(_blobShapes, createIcosahedron(2, true))      --2
    table_insert(_blobShapes, createCubePoints(2, false))      --3
    table_insert(_blobShapes, createCirclePoints(12, 3))       --4
    table_insert(_blobShapes, createCubePoints(2, true))       --5
    table_insert(_blobShapes, createCirclePoints(16, 3, true)) --6
    table_insert(_blobShapes, createCirclePoints(5, 3))        --7
    table_insert(_blobShapes, createCirclePoints(3, 3, true))  --8
    table_insert(_blobShapes, createCirclePoints(3, 3))        --9
    table_insert(_blobShapes, createCirclePoints(4, 3))        --10

    _blobList = { 9, 10, 3, 1, 2, 4, 5, 3, 2, 6, 7, 8, 6, 1, 8, 4, 7, 4 }

    -- Create sprites on canvas and grab the pixels.
    for i = 1, 10 do
        local r = 8 + i * 2
        cls(0)
        circ(r, r, r, 4)
        circ(r, r, r - 2, 3)
        circ(r, r, r - 4, 2)
        circ(r, r, r - 6, 1)
        _imgBlobSprites[i] = core_image_from_canvas(0, 0, r * 2 + 1, r * 2 + 1)
    end
end

--- CREDITS INIT

function initCreditsScene()
end

--- TRANSIT2LISSA INIT

function initTransit2LissaScene()
    insertShuffled(_coverLines, 0, 135, 1338)

    local globalDelay = 2
    local elementDelay = .1
    local dur = 1.2
    for i = 1, 4 do
        table_insert(_schedule.anims[AnimNames.TRANSIT2LISSA].keyframes,
            core_anim_new_keyframe_tween(
                globalDelay + (i) * elementDelay,
                globalDelay + (i) * elementDelay + dur,
                { 1 }, { #_lissaLogoFadeIndexes1 },
                core_easing_lerp,
                _lissaLogoChars[i].colIdx)
        )
    end

    globalDelay = globalDelay + 4 * elementDelay
    for i = #_lissaLogoChars, 5, -1 do
        table_insert(_schedule.anims[AnimNames.TRANSIT2LISSA].keyframes,
            core_anim_new_keyframe_tween(
                globalDelay + (5 - i) * elementDelay,
                globalDelay + (5 - i) * elementDelay + dur,
                { 1 }, { #_lissaLogoFadeIndexes2 },
                core_easing_lerp,
                _lissaLogoChars[i].colIdx)
        )
    end
end

--- LISSA INIT

--- @param cnt: Number of points.
--- @param ax, ay, az: Amplitudes.
--- @param fx, fy, fz: Frequencies.
--- @param px, py, pz: Phase shifts (optional).
--- Example 2D: createLissajousPoints(100, 5, 5, 0, 3, 2, 0, 0, math.pi/2)
--- Example 3D: createLissajousPoints(100, 5, 5, 5, 3, 2, 4, 0, math.pi/2, math.pi/4)
function createLissajousPoints(cnt, ax, ay, az, fx, fy, fz, px, py, pz)
    local points = {}
    for i = 1, cnt do
        local mx = 1 + 1 * sin((i + 0) * .05)
        local my = 1 + 1 * sin(1 + (i + 0) * .05)
        local mz = 1 + 1 * sin(2 + (i + 0) * .05)
        local t = (i - 1) / (cnt - 1) * 30
        local x = mx * ax * sin(fx * t + (px or 0))
        local y = my * ay * sin(fy * t + (py or 0))
        local z = mz * az * sin(fz * t + (pz or 0))
        points[i] = { x, y, z }
    end
    return points
end

function initLinePoints()
    for i = 1, _numLissaLines do
        _lissaLinePoints[i] = { 0, 0, -(i - 1) * _lissaLineDist }
    end
end

function initLissajousScene()
    _imgLissaBG = core_pcx_decode(RES_PCX_LISSA_BG)
    _lissaPoints = createLissajousPoints(10000, 5, 5, 5, sqrt(2), sqrt(5), sqrt(2), M_PI_2 * .5, M_PI_2 * 2, M_PI_2 * 1.5)
    initLinePoints()
end

--- TRANSIT2SHADOW INIT

function initTransit2ShadowScene()
    -- Nothing.
end

--- SHADOW INIT

function precalcLightmap(lightMap, lightMapSize)
    for y = 0, lightMapSize - 1 do
        for x = 0, lightMapSize - 1 do
            local d = math.sqrt(
                (lightMapSize / 2 - x) * (lightMapSize / 2 - x) +
                (lightMapSize / 2 - y) * (lightMapSize / 2 - y)) / (lightMapSize / 2)

            local color = 255 - 255 * d
            color = color < 0 and 0 or color
            lightMap[x + y * lightMapSize] = color // 1
        end
    end
end

function initRays(rays, segments)
    for i = 1, #segments * 3 do
        rays[i] = {
            a = {},
            b = {}
        }
    end
end

function initUniqueRays(uniqueRays, rays)
    for i = 1, #rays do
        uniqueRays[i] = {}
    end
end

function polysToSegments(polys, segments)
    local i = 1
    for p = 1, #polys do
        if _shadowCastEnabled[p] then
            local poly = polys[p]
            for q = 1, #poly do
                local j1 = q % #poly + 1
                segments[i] = {
                    a = { x = poly[q][X], y = poly[q][Y] },
                    b = { x = poly[j1][X], y = poly[j1][Y] }
                }
                i = i + 1
            end
        end
    end
end

function initShadowScene()
    _imgBricks = core_pcx_decode(RES_PCX_BRICKS)
    _polys = core_util_deep_copy(_polysTemplate)
    polysToSegments(_polys, _segments)
    initRays(_rays, _segments)
    initUniqueRays(_uniqueRays, _rays)
    precalcLightmap(_lightMap, _lightMapSize)
end

--- HAVE2RUN INIT

function initHave2RunScene()
    local delaySwipeIn = 12.3
    for i = 1, #_runnerFloorLines do
        table_insert(_schedule.anims[AnimNames.HAVE2RUN].keyframes,
            core_anim_new_keyframe_tween(
                _runnerFloorLines[i].ts + delaySwipeIn, _runnerFloorLines[i].te + delaySwipeIn + .2,
                { -244 }, { 0 },
                core_easing_quadratic_out,
                _runnerFloorLines[i].x)
        )
    end
end

--- RUNNER INIT

function initRunnerBulk()
    math.randomseed(1)

    for i = 1, 4 do
        _runnerBulk[i] = {
            sprIdx = core_math_randf(1, 9) // 1,
            speed = core_math_randf(70, 90) // 1,
            x = i * -30 + core_math_randf(-200, -60) // 1,
            y = (60 - 5 + i * 1.5) // 1,
        }
    end
end

function initRunnerScene()
    _imgSkylineFront = core_pcx_decode(RES_PCX_SKYLINE_FRONT)
    _imgSkylineBack = core_pcx_decode(RES_PCX_SKYLINE_BACK)
    _imgSkylineTrees = core_pcx_decode(RES_PCX_SKYLINE_TREES)
    _imgRunnerSheet = core_pcx_decode(RES_PCX_RUNNER_SHEET)
    _imgDoggySheet = core_pcx_decode(RES_PCX_DOGGY)
    _imgHeli = core_pcx_decode(RES_PCX_HELI)

    _imgSkylineBackShort = core_image_crop(_imgSkylineBack, 0, 15, _imgSkylineBack.width, 29)

    for i = 0, 3 do
        _imgDoggySprites[i + 1] = core_image_crop(_imgDoggySheet, i * 38, 0, 38, 29)
    end

    core_sprite_transfer_sheet(_imgRunnerSheet, 0)
    _runnerSprites[1] = core_sprite_def(0, 0, 0, 5, 6, 0, 38, 44)
    _runnerSprites[1].offX = 0
    _runnerSprites[2] = core_sprite_def(0, 5, 0, 6, 6, 0, 41, 44)
    _runnerSprites[2].offX = 0
    _runnerSprites[3] = core_sprite_def(0, 11, 0, 5, 6, 0, 37, 46)
    _runnerSprites[3].offX = 3
    _runnerSprites[4] = core_sprite_def(0, 0, 6, 4, 6, 0, 31, 47)
    _runnerSprites[4].offX = 5
    _runnerSprites[5] = core_sprite_def(0, 9, 13, 7, 3, 0, 24, 49) -- rot
    _runnerSprites[5].offX = 8
    _runnerSprites[5].rot = 3                                      -- Rotate 270 degrees CW.
    _runnerSprites[6] = core_sprite_def(0, 9, 6, 3, 7, 0, 24, 50)
    _runnerSprites[6].offX = 12
    _runnerSprites[7] = core_sprite_def(0, 12, 6, 4, 7, 0, 31, 50)
    _runnerSprites[7].offX = 7
    _runnerSprites[8] = core_sprite_def(0, 0, 12, 7, 4, 0, 32, 49) -- rot
    _runnerSprites[8].offX = 4
    _runnerSprites[8].rot = 3                                      -- Rotate 270 degrees CW.

    initRunnerBulk()

    -- Actually only 17 are used...
    for i = 1, 136 do
        table_insert(_runnerSwipesX, { CORE_WIDTH })
    end

    math.randomseed(13)
    for i = 1, #_runnerSwipesX do
        local st = 1 + core_math_randf(0, 2)
        local dur = st + core_math_randf(3, 4)
        table_insert(_schedule.anims[AnimNames.RUNNER].keyframes,
            core_anim_new_keyframe_tween(st, dur, { CORE_WIDTH }, { -20 },
                core_easing_cubic_out,
                _runnerSwipesX[i])
        )
    end

    local delaySwipeOut = 47 + 2.5 - 7
    for i = 1, #_runnerFloorLines do
        table_insert(_schedule.anims[AnimNames.RUNNER].keyframes,
            core_anim_new_keyframe_tween(
                _runnerFloorLines[i].ts + delaySwipeOut, _runnerFloorLines[i].te + delaySwipeOut,
                { 0 }, { 244 },
                core_easing_quadratic_out,
                _runnerFloorLines[i].x)
        )
    end
end

--- TRANSIT2VOX INIT

function initTransit2VoxTextsAnims()
    local globalDelay = 3.4
    for i = 1, #_voxTxts do
        local elementDelay = i * .1
        table_insert(_schedule.anims[AnimNames.TRANSIT2VOX].keyframes,
            core_anim_new_keyframe_tween(globalDelay + elementDelay, globalDelay + elementDelay + 1.7,
                { 1 }, { #_voxTextFadeColors },
                core_easing_lerp,
                _voxTxts[i].col)
        )
    end
end

function insertShuffled(table, min, max, seed)
    for i = min, max do
        table_insert(table, i)
    end
    math.randomseed(seed)
    core_util_shuffle(table)
end

function initTransit2VoxScene()
    _imgSpectrox = core_pcx_decode(RES_PCX_SPECTROX)
    initTransit2VoxTextsAnims()
    insertShuffled(_voxLinesSpectrox, 1, _imgSpectrox.height, 12345)
end

--- VOX INIT

function addFlashKeyframes()
    local keys = _schedule.anims[AnimNames.VOX].keyframes
    for i = 1, #_voxFlashes do
        local s = _voxFlashes[i].startTime
        local e = _voxFlashes[i].endTime
        table_insert(keys, core_anim_new_keyframe_fire_once(s, function() _voxGradVbank0Src = _voxGradBlack; end))
        table_insert(keys, core_anim_new_keyframe_tween(s, e, { 0 }, { 1 }, core_easing_lerp, _voxGradVbank0Mix))

        table_insert(keys, core_anim_new_keyframe_fire_once(s, function() _voxGradVbank1Src = _voxGradBlack; end))
        table_insert(keys, core_anim_new_keyframe_tween(s, e, { 0 }, { 1 }, core_easing_lerp, _voxGradVbank1Mix))
        table_insert(keys, core_anim_new_keyframe_tween(s, e, { 1 }, { 0 }, core_easing_lerp, _voxFlashFactor))
    end
end

--- Resize heightmap's grayscale values.
--- See: https://rosettacode.org/wiki/Bilinear_interpolation
---@return table Contains resized heightmap. Not integers, but can contain decimals.
function bilinearResize(src, srcW, srcH, dstW, dstH)
    local dst = {}
    local xr = (srcW - 1) / (dstW - 1)
    local yr = (srcH - 1) / (dstH - 1)

    local val = function(xx, yy)
        return src[min(yy, srcH - 1) * srcW + min(xx, srcW - 1) + 1]
    end

    for j = 0, dstH - 1 do
        local y = yr * j
        local yi = y // 1
        local yf = y - yi

        for i = 0, dstW - 1 do
            local x = xr * i
            local xi = x // 1
            local xf = x - xi

            local v00 = val(xi, yi)
            local v10 = val(xi + 1, yi)
            local v01 = val(xi, yi + 1)
            local v11 = val(xi + 1, yi + 1)

            local top = core_math_lerp(v00, v10, xf)
            local bottom = core_math_lerp(v01, v11, xf)
            local result = core_math_lerp(top, bottom, yf)

            dst[j * dstW + i + 1] = result
        end
    end

    return dst
end

function initVoxScene()
    _imgTime = core_pcx_decode(RES_PCX_TIME)

    insertShuffled(_voxLinesTime, 1, _imgTime.height + 10, 12345)

    _voxMapW = 256
    _voxMapH = 256
    _voxMap = bilinearResize(RES_HMAP, RES_HMAP_WIDTH, RES_HMAP_HEIGHT, _voxMapW, _voxMapH)
    core_grad_mix(_voxGradLakes1, _voxGradLakes2, _voxGradVbank1, 1)
    addFlashKeyframes()
end

--- WATERFIELD INIT

function initWaterfieldScene()
    math.randomseed(12345)

    for i = 1, 200 do
        _waterParticles[i] = {
            core_math_randf(-_waterParticlesDim, _waterParticlesDim) * 1.25,
            core_math_randf(-_waterParticlesDim, _waterParticlesDim) - 19,
            core_math_randf(-_waterParticlesDim, _waterParticlesDim) }
    end

    -- Extend water BG gradient.
    local extendLinesCnt = 1000
    for i = 1, extendLinesCnt * 3 do
        table_insert(_waterGradBG, 0)
    end

    _waterGradVbank0 = core_grad_new(#_waterGradBG)
    _waterGradSplash = core_grad_new(#_waterGradBG)
    core_grad_clear(_waterGradSplash, 40, 92, 196)

    for i = 1, #_waterGradSplash, 6 do
        _waterGradSplash[i] = 36
        _waterGradSplash[i + 1] = 159
        _waterGradSplash[i + 2] = 222
    end

    _waterCubeBubblesEmitter = newEmitter(800,
        { x = 120, y = 0 },
        3, .001,
        { x = 8, y = 10 },
        { min = 80, max = 100 },
        { min = 5, max = 10 },
        { min = 4, max = 8 }
    )
end

--- UNDERWATER INIT

function initUnderwaterScene()
    _underwaterEmitter = newEmitter(30,
        { x = 215, y = 70 },
        3, .08,
        { x = 4, y = 10 },
        { min = 20, max = 30 },
        { min = 3, max = 3 },
        { min = 2, max = 4 }
    )
    _underwaterEmitter.isParticleSpawnEnabled = false
    _underwaterEmitter.isParticleRenderEnabled = false

    _underwaterCubeEmitter = newEmitter(100,
        { x = 100, y = 80 },
        8, .005,
        { x = 20, y = 10 },
        { min = 30, max = 50 },
        { min = 2, max = 5 },
        { min = 4, max = 8 }
    )
    _underwaterCubeEmitter.isParticleSpawnEnabled = false
    _underwaterCubeEmitter.isParticleRenderEnabled = false


    -- Extend underwater BG gradient.
    local extendLinesCnt = 1000
    for i = 1, extendLinesCnt * 3 do
        table_insert(_gradUnderwater, 0)
        table_insert(_gradUnderwaterBrightBase, 0)
    end

    _imgSand = core_pcx_decode(RES_PCX_SAND)
    _imgSandFG = core_pcx_decode(RES_PCX_SAND_FG)
    _imgKelpBG = core_pcx_decode(RES_PCX_KELP_BG)
    _imgKelp1 = core_pcx_decode(RES_PCX_KELP1)
    _imgKelp2 = core_pcx_decode(RES_PCX_KELP2)
    _imgKelp3 = core_pcx_decode(RES_PCX_KELP3)
    _imgKelpFGLeft1 = core_pcx_decode(RES_PCX_KELP_FG_LEFT_1)
    _imgKelpFGLeft2 = core_pcx_decode(RES_PCX_KELP_FG_LEFT_2)
    _imgKelpFGLeft3 = core_pcx_decode(RES_PCX_KELP_FG_LEFT_3)
    _imgKelpFGLeft4 = core_pcx_decode(RES_PCX_KELP_FG_LEFT_4)
    _imgKelpFGRight1 = core_pcx_decode(RES_PCX_KELP_FG_RIGHT_1)
    _imgKelpFGRight2 = core_pcx_decode(RES_PCX_KELP_FG_RIGHT_2)
    _imgKelpFGRight3 = core_pcx_decode(RES_PCX_KELP_FG_RIGHT_3)
    _imgKelpFGRight4 = core_pcx_decode(RES_PCX_KELP_FG_RIGHT_4)
    _imgKelpFGRight5 = core_pcx_decode(RES_PCX_KELP_FG_RIGHT_5)
    _imgKelpFGRight6 = core_pcx_decode(RES_PCX_KELP_FG_RIGHT_6)
    _imgSideRocksLeft = core_pcx_decode(RES_PCX_SIDE_ROCKS_LEFT)
    _imgSideRocksRight = core_pcx_decode(RES_PCX_SIDE_ROCKS_RIGHT)
    _imgThe = core_pcx_decode(RES_PCX_THE)
    _imgEnd = core_pcx_decode(RES_PCX_END)
end

--- HIDDEN INIT

function initHiddenScene()
end

-------------------------------------------------------------------------------
--- EXECUTE
-------------------------------------------------------------------------------

--- STARTUP_DELAY

function renderStartupDelayScene(t)
    cls(0)
    core_blit_render(_screenTic, 0, 0, 0)
end

--- SCREEN_FADER EXECUTE

function renderScreenFaderCovers(height, color)
    local h = height // 1
    rect(0, 0, 240, h, color)
    rect(0, 136 - h, 240, h + 1, color)
    if h <= 68 then
        line(0, h, 240, h, 14)
        line(0, 136 - h, 240, 136 - h, 14)
    end
end

function renderScreenFaderScene(t)
    vbank(0)
    cls(0)

    local palFactor = _faderPalFactor[1]

    core_pal_apply_color(0,
        core_math_lerp(_palTic[1], 0, palFactor),
        core_math_lerp(_palTic[2], 0, palFactor),
        core_math_lerp(_palTic[3], 0, palFactor)
    )

    core_blit_render(_screenTic, 0, 0, 0)

    vbank(1)
    cls(0)

    core_pal_apply_color(15,
        core_math_lerp(_palTic[1], 0, palFactor),
        core_math_lerp(_palTic[2], 0, palFactor),
        core_math_lerp(_palTic[3], 0, palFactor)
    )

    renderScreenFaderCovers(_coverHeight[1], 15)
end

--- PRESENTS EXECUTE

function renderPresentsHighlightLines()
    for i = 1, #_presentsHighlightLines do
        highlightLine(_presentsHighlightLines[i].y[1] // 1)
    end
end

function renderPresentsScene(t)
    vbank(0)
    cls(0)

    core_blit_render(_imgPrelude, 22, 41, 0)
    core_blit_render(_imgNeobyte, 10, 62, 0)
    renderPresentsHighlightLines()

    ---
    vbank(1)
    cls(0)
    clip(0, _presentsClipPosY[1] // 1, 240, 50)
    core_pal_swap(15, _presentsFadeIndexes[_presentsColorIdx[1] // 1])
    if _presents1Enabled then
        font("a t   d e a d l i n e   2 0 2 5", 120 - 180 // 2, 66, 0, 4, 8, false, 1)
    end

    if _presents2Enabled then
        font("s  *  p  *  e  *  c  *  t  *  r  *  o  *  x", 120 - 190 // 2, 66, 0, 4, 8, false, 1)
    end

    if _presents3Enabled then
        font("p r e s e n t s", 120 - 92 // 2, 66, 0, 4, 8, false, 1)
    end
    core_pal_swap(15, 15)
    clip()
end

--- TITLE EXECUTE

function renderTitleBorder()
    line(_blobBorderTopX[1], 0, _blobBorderTopX[1] + CORE_WIDTH_1, 0, 2)
    line(_blobBorderTopX[1], 1, _blobBorderTopX[1] + CORE_WIDTH_1, 1, 4)

    line(_blobBorderBottomX[1], 134, _blobBorderBottomX[1] + CORE_WIDTH_1, 134, 2)
    line(_blobBorderBottomX[1], 135, _blobBorderBottomX[1] + CORE_WIDTH_1, 135, 4)
end

function hightlightPix(x, y, minC1, maxC1, minC2, maxC2, add)
    local c = pix(x, y)
    local r1 = (c > minC1 and c <= maxC1)
    local r2 = (c > minC2 and c <= maxC2)

    if not (r1 or r2) then
        return
    end

    c = c + add
    local max = r1 and maxC1 or maxC2

    pix(x, y, c > max and max or c)
end

function highlightLine(y)
    if y < 40 or y > 93 then
        return
    end

    for x = 10, 229 do
        hightlightPix(x, y, 4, 11, 0, 4, 16)
        hightlightPix(x, y + 1, 4, 10, 0, 3, 2)
    end
end

function renderHighlightLines(t)
    for i = 1, _numTitleHighlightLines[1] do
        local y = (65 + 38 * sin(i * 41.35 + t.elapsed)) // 1
        highlightLine(y)
    end
end

function checkHidenPart()
    return key(19) -- "S", you cheater! XD
end

function startHiddenPart()
    vbank(1)
    cls(0)
    vbank(0)
    cls(0)
    core_sound_stop()
    _musicStarted = false
    _schedule.anims[AnimNames.TITLE].state = 2 -- COMPLETED.
    core_anim_start(_schedule.anims[AnimNames.HIDDEN], _timer)
end

function renderTitleScene(t)
    vbank(0)
    cls(0)
    if _titleGridEnabled then
        local xm, ym
        if _titleGridVerticalOnly then
            _titleGridY = _titleGridY + t.delta * 60
            xm = -10
            ym = _titleGridY
        else
            xm = cos(t.elapsed * 1) * 100
            ym = sin(t.elapsed * .6) * 130
        end

        renderChecker(xm, ym)
    end

    renderTitleBorder()

    if _titleFlashEnabled then
        core_pal_apply_white_fade(_palBlob, _titleFlashFactor[1])
    end

    vbank(1)
    cls(0)

    if _titlePalMixEnabled then
        core_pal_apply_mix(_imgNeobyte.palette, _palTitleHighlightOnly, _titlePalMaxFactor[1])
    end
    if _titleFlashEnabled then
        core_pal_apply_white_fade(_imgNeobyte.palette, _titleFlashFactor[1])
    end

    core_blit_render(_imgPrelude, (22 + _titlePosXAmp * sin(M_PI * t.elapsed)) // 1, 41, 0)
    core_blit_render(_imgNeobyte, (10 + _titlePosXAmp * sin(.375 + M_PI * t.elapsed)) // 1, 62, 0)

    renderHighlightLines(t)

    if _pressStartEnabled then
        if checkHidenPart() then
            startHiddenPart()
            return
        end
        if _syncKickSnare4 == 1 then
            _pressStartBlinkerEnabled = not _pressStartBlinkerEnabled
        end
        if _pressStartBlinkerEnabled then
            core_pal_swap(15, 8)
            font("press start", 120 - 84 * .5, 105, 0, 4, 8, false, 1)
            core_pal_swap(15, 15)
        end
    end
end

--- BLOB EXECUTE

function blitBlob(image, x, y, colorTransparent)
    for iy = 0, image.height_1 do
        local stride = iy * image.width + 1
        local ty = (iy + y) // 1
        for ix = 0, image.width_1 do
            local c = image.pixels[ix + stride]
            local tx = (ix + x) // 1
            if c ~= colorTransparent and (ty) & 1 == 0 then
                local tc = pix(tx, ty)

                -- Overwrite colors from inside to outside of the circle.
                if tc == 0 or tc == 5 or tc == 6 or tc == 7 or c == 1 then
                    -- If dest is BG color then overwrite with any src color.
                    -- Also if src color is 1 then overwrite any other color.
                    pix(tx, ty, c)
                elseif c == 2 and (tc == 3 or tc == 4) then
                    -- Src color 2 overwrites dest colors 3 and 4.
                    pix(tx, ty, 2)
                elseif c == 3 and tc == 4 then
                    -- Src color 3 overwrites dest color 4.
                    pix(tx, ty, 3)
                end
            end
        end
    end
end

function updateBlobs(t)
    local sc = core_mat4_scale(.65 + _blobSyncSnare * 1.25, .65 + _blobSyncSnare * 1.25, .65 + _blobSyncSnare * 1.25)

    local t2 = t.elapsed * .5

    local xoff = sin(t.elapsed) * .2
    local yoff = sin(1 + t.elapsed * 3) * .2
    local zoff = sin(2 + t.elapsed * 5) * .2

    local tr0 = core_mat4_translate(xoff, yoff, zoff)
    local rx = core_mat4_rot_x(2 + sin(t.elapsed * .05 + 1) * 6 + t2)
    local ry = core_mat4_rot_y(sin(5 + t.elapsed * .08) * 50 + sin(2 + t.elapsed * .04) * 20 + t2)
    local rz = core_mat4_rot_z(sin(3 + t.elapsed * .05) * 60 + t2)
    local tr = core_mat4_translate(-xoff, -yoff, -zoff - 10)

    core_mat4_set_identity(_modelMatBlobs)
    core_mat4_mul(_modelMatBlobs, tr0, _modelMatBlobs)
    core_mat4_mul(_modelMatBlobs, sc, _modelMatBlobs)
    core_mat4_mul(_modelMatBlobs, rx, _modelMatBlobs)
    core_mat4_mul(_modelMatBlobs, ry, _modelMatBlobs)
    core_mat4_mul(_modelMatBlobs, rz, _modelMatBlobs)
    core_mat4_mul(_modelMatBlobs, tr, _modelMatBlobs)
end

function renderBlobs()
    local points = _blobShapes[_blobList[_blobListIdx]]
    for i = 1, #points do
        local out = { 0, 0, 0 }
        core_mat4_mul_vec3(out, _modelMatBlobs, points[i])
        core_mat4_mul_vec3_perspective(out, _matProj, out)

        if out[Z] < 0 then
            local sprIdx = ((10 + out[Z] + 1) / 3 * #_imgBlobSprites) // 1
            if sprIdx < 1 then
                sprIdx = 1
            elseif sprIdx > #_imgBlobSprites then
                sprIdx = #_imgBlobSprites
            end

            local spr = _imgBlobSprites[sprIdx]
            blitBlob(spr, _blobPos[1] + out[X] - spr.width_half, _blobPos[2] + out[Y] - spr.width_half, 0)
        end
        ::continue::
    end
end

function renderChecker(xm, ym)
    for x = -24, 24 do
        local xx = xm + x * 20
        if xx >= 0 and xx < CORE_WIDTH then
            for y = 0, CORE_HEIGHT_1 do
                pix(xx, y, 5)
            end
        end
    end

    for y = -20, 13 do
        local yy = ym + y * 20
        if yy >= 0 and yy < CORE_HEIGHT then
            for x = 0, CORE_WIDTH_1 do
                if pix(x, yy) ~= 0 then
                    pix(x, yy, 6)
                else
                    pix(x, yy, 5)
                end
            end
        end
    end

    xm = xm * 1.5
    ym = ym * 1.5

    for x = -24, 24 do
        local xx = xm + x * 30
        if xx >= 0 and xx < CORE_WIDTH then
            for y = 0, CORE_HEIGHT_1 do
                if pix(xx, y) ~= 0 then
                    pix(xx, y, 7)
                else
                    pix(xx, y, 6)
                end
            end
        end
    end

    for y = -20, 13 do
        local yy = ym + y * 30
        if yy >= 0 and yy < CORE_HEIGHT then
            for x = 0, CORE_WIDTH_1 do
                if pix(x, yy) ~= 0 then
                    pix(x, yy, 7)
                else
                    pix(x, yy, 6)
                end
            end
        end
    end
end

function renderBlobBGCircles(t)
    local cr = 50 + 10 * sin(t.elapsed * 4)
    local xoff, yoff = 0, 0
    circ(xoff + _blobPos[1] + 120, yoff + _blobPos[2] + 68, cr, 0)
end

function renderBlobScene(t)
    cls(0)

    if _blobFlashEnabled then
        core_pal_apply_white_fade(_palBlob, _blobFlashFactor[1])
    else
        core_pal_apply_mix(_palBlob, _palBlobBright, _blobSyncSnare)
    end

    local xm = cos(t.elapsed * 1) * 100
    local ym = sin(t.elapsed * .6) * 130
    renderChecker(xm, ym)

    renderBlobBGCircles(t)

    if _blobEnabled then
        updateBlobs(t)
        renderBlobs()
    end

    renderTitleBorder()

    if _blobGridColorFadeEnabled then
        local cidx = _runTextFadeColors[_blobGridColorIdx[1] // 1] + 1
        local r = _palRunner[(cidx - 1) * 3 + 1]
        local g = _palRunner[(cidx - 1) * 3 + 2]
        local b = _palRunner[(cidx - 1) * 3 + 3]

        core_pal_apply_color(5, r, g, b)
        core_pal_apply_color(6, r, g, b)
        core_pal_apply_color(7, r, g, b)
    end
end

--- CREDITS EXECUTE

function renderCreditsScene(t)
    cls(0)

    if _creditsOlympianEnabled then
        core_pal_swap(15, _presentsFadeIndexes[_creditsOlympianFadeIndex[1] // 1])
        font("code gfx", 90 + _creditsCodePos[1], 56 + _creditsCodePos[2], 0, 4, 8, false, 1)
        font("olympian", 57 + _creditsOlympianPos[1], 43 + _creditsOlympianPos[2], 0, 4, 8, false, 2)
    end

    if _creditsVirgillEnabled then
        core_pal_swap(15, _presentsFadeIndexes[_creditsVirgillFadeIndex[1] // 1])
        font("music", 100 + _creditsMusicPos[1], 94 + _creditsMusicPos[2], 0, 4, 8, false, 1)
        font("virgill", 65 + _creditsVirgillPos[1], 81 + _creditsVirgillPos[2], 0, 4, 8, false, 2)
    end

    core_pal_swap(15, 15)
end

--- TRANSIT2LISSA EXECUTE

_lissaBGCol2 = { 12, 19, 31 }
_lissaBGCol7 = { 64, 51, 83 }

function updateLissaBGColors(t)
    if core_timer_current_frame() % 3 == 0 then
        local r = random(-_lissaBGFlickerStrength[1] // 1, 0)
        _lissaBGCol2 = { 12 + r, 19 + r, 31 + r }
        _lissaBGCol7 = { 64 + r, 51 + r, 83 + r }
    end

    core_pal_apply_color(2, _lissaBGCol2[1], _lissaBGCol2[2], _lissaBGCol2[3])
    core_pal_apply_color(7, _lissaBGCol7[1], _lissaBGCol7[2], _lissaBGCol7[3])
end

function renderCoverLines(col)
    local max = 136 * _coverLinesFactor[1]
    for i = 1, max do
        line(0, _coverLines[i], 240, _coverLines[i], col)
    end
end

function renderTransit2LissaFont(t)
    vbank(1)
    cls(0)

    local char
    local idxTable
    for i = 1, #_lissaLogoChars do
        idxTable = i <= 4 and _lissaLogoFadeIndexes1 or _lissaLogoFadeIndexes2
        char = _lissaLogoChars[i]
        core_pal_swap(15, idxTable[char.colIdx[1] // 1])
        font(char.v, char.x, 111)
    end
    core_pal_swap(15, 15)
end

function renderTransit2LissaScene(t)
    vbank(0)
    cls(0)

    updateLissaLines(t)
    renderLissaLines(t)
    core_blit_render(_imgLissaBG, 0, 0, 0)
    updateLissaBGColors(t)
    renderCoverLines(0)
    renderTransit2LissaFont(t)
end

--- LISSA EXECUTE

function updateLissaLines(t)
    for i = 1, #_lissaLinePoints do
        _lissaLinePoints[i][Z] = _lissaLinePoints[i][Z] + t.delta * 3
        if _lissaLinePoints[i][Z] > 0 then
            _lissaLinePoints[i][Z] = _lissaLinePoints[i][Z] - _lissaLinesTotalDist
        end
    end

    local tr = core_mat4_translate(0, -1, -1)
    core_mat4_set_identity(_modelMatLissaLines)
    core_mat4_mul(_modelMatLissaLines, tr, _modelMatLissaLines)
end

function renderLissaLines(t)
    line(0, 84, CORE_WIDTH_1, 84, 13) -- Top line.
    for i = 1, #_lissaLinePoints do
        local out = { 0, 0, 0 }
        core_mat4_mul_vec3(out, _modelMatLissaLines, _lissaLinePoints[i])
        core_mat4_mul_vec3_perspective(out, _matProj, out)

        if out[Z] < 0 then
            line(0, out[Y], CORE_WIDTH_1, out[Y], 13)
        end
    end
end

_lissaRotVelocity = 0
_lissaRotAng = 0
_lissaRotDamp = 1
_lissaRotImpulse = -4

_lissaSyncColor = 0

function updateLissa(t)
    _lissaRotVelocity = core_math_lerp(_lissaRotVelocity, 0, _timer.delta * _lissaRotDamp)
    _lissaRotAng = _lissaRotAng + t.delta * _lissaRotVelocity

    local rx = core_mat4_rot_x(2 + sin((t.elapsed - 2.) * .25 + 1) * 6)
    local ry = core_mat4_rot_y(-(t.elapsed - 1.) + _lissaRotAng)
    local tr = core_mat4_translate(0, 0, -40 + _lissaTranslateZ[1])

    core_mat4_set_identity(_modelMatLissa)
    core_mat4_mul(_modelMatLissa, rx, _modelMatLissa)
    core_mat4_mul(_modelMatLissa, ry, _modelMatLissa)
    core_mat4_mul(_modelMatLissa, tr, _modelMatLissa)
end

function renderLissa(t)
    local count = _lissaCount[1]
    if count <= 0 then
        return
    end

    local startIdx = (1 + (t.elapsed - 5.5) * _lissaSpeed[1]) // 1
    if startIdx < 0 then
        startIdx = 0
    end
    for i = startIdx, startIdx + count do
        local out = { 0, 0, 0 }
        core_mat4_mul_vec3(out, _modelMatLissa, _lissaPoints[i % (#_lissaPoints) + 1])
        core_mat4_mul_vec3_perspective(out, _matProj, out)

        if out[Z] < 0 then
            local c = pix(out[X], out[Y])
            if c == 0 then
                c = 5
            elseif c >= 5 and c < 11 then
                c = c + 1
            else
                c = 11
            end

            c = c + _lissaSyncColor * 4
            if c > 11 then
                c = 11
            end

            local ch = c + 1
            if ch > 11 then
                ch = 11
            end

            if i > startIdx + count - 100 then
                pix(out[X], out[Y], ch)
                pix(out[X] + 1, out[Y], ch)
            else
                pix(out[X], out[Y], c)
            end
        end
    end
end

function renderLissaSceneLogoText(col1, col2)
    core_pal_swap(15, col1)
    font("spec", 89, _lissaLogoLeftPosY[1], 0)
    core_pal_swap(15, col2)
    font("trox", 120, _lissaLogoRightPosY[1], 0)
    core_pal_swap(15, 15)
end

function renderLissaScene(t)
    vbank(0)
    cls(0)
    updateLissaLines(t)
    renderLissaLines(t)
    core_blit_render(_imgLissaBG, 0, 0, 0)
    updateLissaBGColors(t)

    ---
    vbank(1)
    cls(0)
    renderLissaSceneLogoText(4, 2)
    updateLissa(t)
    renderLissa(t)
end

--- TRANSIT2SHADOW EXECUTE

function renderLissaCoverRects()
    -- 12x8 tiles fit perfectly.
    -- Alternative: 15x9 tiles of 16x15px. Results in just 1 pixel row missing at bottom.
    local numW = 12
    local numH = 8
    local w = 20
    local h = 17
    local cnt = 1
    for j = 1, numH do
        for i = 1, numW, 2 do
            core_dither_rect((i - 1) * w, (j - 1) * h, w, h, 0, 2 * sin(-i * .05 + -j * .1 + _lissaCoverTilesProgress[1]),
                CoreDitherMatrixWildBayer8x8)
        end

        for i = 2, numW, 2 do
            core_dither_rect((i - 1) * w, (j - 1) * h, w, h, 0, 2 * sin(-i * .1 + -j * .05 + _lissaCoverTilesProgress[1]),
                CoreDitherMatrixWildBayer8x8)
        end
    end
end

function renderTransit2ShadowScene(t)
    vbank(0)
    cls(0)
    updateLissaLines(t)
    renderLissaLines(t)
    core_blit_render(_imgLissaBG, 0, 0, 0)
    renderLissaCoverRects()

    ---
    vbank(1)
    cls(0)
    renderLissaSceneLogoText(14, 9)
    updateCube(t)
    renderCube()
end

--- SHADOW EXECUTE
--- See: https://ncase.me/sight-and-light/
--- See: https://www.redblobgames.com/articles/visibility/

-- Function to find the intersection of an infinite ray and a finite segment.
function raySegmentIntersection(ray, seg)
    -- Compute direction vectors.
    local dSegX, dSegY = seg.b.x - seg.a.x, seg.b.y - seg.a.y     -- Segment direction.
    local dRayX, dRayY = ray.b[X] - ray.a[X], ray.b[Y] - ray.a[Y] -- Ray direction.

    -- Compute determinant (denominator).
    local denom = (dSegX * -dRayY) - (-dRayX * dSegY)

    if denom == 0 then
        return nil -- Parallel or collinear, no unique intersection.
    end
    local invDenom = 1 / denom

    -- Solve for t and u
    local a = (ray.a[X] - seg.a.x)
    local b = (ray.a[Y] - seg.a.y)
    local t = (a * -dRayY - -dRayX * b) * invDenom
    local u = (dSegX * b - a * dSegY) * invDenom

    -- Check if the intersection lies within the segment (0 <= t <= 1) and on the ray (u >= 0).
    if t < 0 or t > 1 or u < 0 then
        -- No valid intersection.
        return nil
    end

    -- Compute intersection point.
    local ix = seg.a.x + t * dSegX
    local iy = seg.a.y + t * dSegY
    return {
        point = { ix, iy },
        distance = u
    }
end

function getAngle(ray)
    local dRayX, dRayY = ray.b[X] - ray.a[X], ray.b[Y] - ray.a[Y] -- Ray direction.
    local angle = atan(dRayY, dRayX)
    return angle >= 0 and angle or angle + M_TAU
end

function compareAngles(a, b)
    return a.angle < b.angle
end

function updateRays(rays, segments)
    local segIdx = 1
    local epsilon = .00001
    local lightX = 120 + 20 * _shadowCubePos[1] // 1
    local lightY = 68 - 20 * _shadowCubePos[2] // 1
    -- For each segment start point, cast 3 rays.
    for i = 1, #rays, 3 do
        rays[i].a[X], rays[i].a[Y] = lightX, lightY
        rays[i].b[X], rays[i].b[Y] = segments[segIdx].a.x, segments[segIdx].a.y
        rays[i].angle = getAngle(rays[i])

        local i1 = i + 1
        rays[i1].a[X], rays[i1].a[Y] = rays[i].a[X], rays[i].a[Y]
        rays[i1].angle = rays[i].angle - epsilon
        rays[i1].b[X], rays[i1].b[Y] = rays[i].a[X] + cos(rays[i1].angle), rays[i].a[Y] + sin(rays[i1].angle)

        local i2 = i + 2
        rays[i2].a[X], rays[i2].a[Y] = rays[i].a[X], rays[i].a[Y]
        rays[i2].angle = rays[i].angle + epsilon
        rays[i2].b[X], rays[i2].b[Y] = rays[i].a[X] + cos(rays[i2].angle), rays[i].a[Y] + sin(rays[i2].angle)

        segIdx = segIdx + 1
    end

    table_sort(rays, compareAngles)
end

function closestIntersect(ray, segments)
    local found = nil
    for i = 1, #segments do
        local ip = raySegmentIntersection(ray, segments[i])
        if not ip then
            goto continue
        end
        if not found or ip.distance < found.distance then
            found = ip
        end
        ::continue::
    end
    return found
end

function updatIntersects(rays, segments)
    for i = 1, #rays do
        rays[i].intersect = closestIntersect(rays[i], segments)
    end
end

function approxEq(p1, p2)
    if p1 == nil or p2 == nil then
        return false
    end
    return abs(p1[X] - p2[X]) < _epsilon and abs(p1[Y] - p2[Y]) < _epsilon
end

function eqIntersect(i1, i2)
    if i1 == nil or i2 == nil then
        return false
    end
    return approxEq(i1.point, i2.point)
end

function updateUniqueRays(uniqueRays, rays)
    uniqueRays[1] = rays[1]
    local count = 1

    for i = 2, #rays do
        if not eqIntersect(rays[i].intersect, rays[i - 1].intersect) then
            count = count + 1
            uniqueRays[count] = rays[i]
        end
    end
    return count
end

function renderTris(rays, count)
    clip(_shadowClips[_shadowClipsIdx].xs,
        _shadowClips[_shadowClipsIdx].ys,
        _shadowClips[_shadowClipsIdx].xe - _shadowClips[_shadowClipsIdx].xs + 1,
        _shadowClips[_shadowClipsIdx].ye - _shadowClips[_shadowClipsIdx].ys + 1
    )
    local color = 15
    for i = 1, count do
        local i1 = 1 + i % count
        if rays[i].intersect and rays[i1].intersect then
            tri(rays[i].a[X], rays[i].a[Y],
                rays[i].intersect.point[X], rays[i].intersect.point[Y],
                rays[i1].intersect.point[X] + .5, rays[i1].intersect.point[Y],
                color)
            tri(rays[i].a[X], rays[i].a[Y],
                rays[i1].intersect.point[X], rays[i1].intersect.point[Y],
                rays[i].intersect.point[X] + .5, rays[i].intersect.point[Y],
                color)
        end
    end
    clip()
end

function renderLightMap(lightMap, lightMapSize, centerX, centerY, t)
    local mx, my = lightMapSize / 2 - centerX, lightMapSize / 2 - centerY
    local di = 8 + 6 * sin(t.elapsed * 40) + 4 * sin(1 + t.elapsed * 30)
    local db = 2.5 + 10 * _syncSnare8

    local im = (1 / 255) * (_shadowFadeLight[1] * (5 + db))

    -- Determine how dark/transparent the shadow is.
    local ss = (3 + sin(t.elapsed)) // 1

    local oldColor, intensity, color, frac

    for y = _shadowClips[_shadowClipsIdx].ys, _shadowClips[_shadowClipsIdx].ye do
        for x = _shadowClips[_shadowClipsIdx].xs, _shadowClips[_shadowClipsIdx].xe do
            if not (x + mx < 0 or x + mx > lightMapSize - 1 or y + my < 0 or y + my > lightMapSize - 1) then
                oldColor = pix(x, y)
                intensity = lightMap[x + mx + (y + my) * lightMapSize]
                color = (intensity - di) * im

                color = color * 1.5 - _shadowFadeTexture[1] * (12 - 1.5 * core_image_get(_imgBricks, x, y))

                -- Hack: Special treatment for some of the white bricks when shadowed.
                if not (oldColor == 15 or
                        (x >= 58 and y >= 69 and x <= 82 and y <= 78) or
                        (x >= 50 and y >= 23 and x <= 66 and y <= 33) or
                        (x >= 108 and y >= 12 and x <= 125 and y <= 21) or
                        (x >= 158 and y >= 35 and x <= 174 and y <= 44) or
                        (x >= 175 and y >= 102 and x <= 192 and y <= 111) or
                        (x >= 131 and y >= 91 and x <= 148 and y <= 100) or
                        (x >= 138 and y >= 102 and x <= 154 and y <= 111) or
                        (x >= 128 and y >= 113 and x <= 144 and y <= 123) or
                        (x >= 2 and y >= 102 and x <= 19 and y <= 111)
                    )
                then
                    -- Shadow intensity. Affects everything but highlighted bricks.
                    color = color - ss
                end

                -- Global shadow modifier from 0..1.
                color = color * _shadowLightIntensity[1]

                frac = color - (color // 1)

                if core_dither(frac, x, y, CoreDitherMatrixVertBayer8x8) then
                    color = color + 1
                end

                if color > 9 then
                    color = 9
                end
                if color < 0 then
                    color = 0
                end

                pix(x, y, color)
            else
                pix(x, y, 0)
            end
        end
    end
end

function updateShadowEntityPositions(t)
    for j = 2, #_polys do
        for i = 1, #_polys[j] do
            _polys[j][i][X] = _polysTemplate[j][i][X] + _shadowCastPos[j][X]
            _polys[j][i][Y] = _polysTemplate[j][i][Y] + _shadowCastPos[j][Y]
        end
    end
end

function updateCube(t)
    _shadowCubePos[1] = _shadowCubeOffX[1] + _shadowCubeAmpX[1] * sin(2 * t.elapsed + _shadowCubeSineOffset)
    _shadowCubePos[2] = _shadowCubeOffY[1] + _shadowCubeAmpY[1] * sin(t.elapsed + _shadowCubeSineOffset)

    local s = _shadowCubeScale[1]
    local sc = core_mat4_scale(s, s, s)
    local ry = core_mat4_rot_y(sin(t.elapsed * 1.5))
    local rx = core_mat4_rot_x(t.elapsed)
    local tr = core_mat4_translate(_shadowCubePos[1], _shadowCubePos[2], -10 + _shadowCubeOffZ[1])

    core_mat4_set_identity(_modelMatShadowCube)
    core_mat4_mul(_modelMatShadowCube, sc, _modelMatShadowCube)
    core_mat4_mul(_modelMatShadowCube, ry, _modelMatShadowCube)
    core_mat4_mul(_modelMatShadowCube, rx, _modelMatShadowCube)
    core_mat4_mul(_modelMatShadowCube, tr, _modelMatShadowCube)

    -- Projected cube points.
    local points = {}

    for i = 1, #_shadowCubeVerts do
        local out = { 0, 0, 0 }
        core_mat4_mul_vec3(out, _modelMatShadowCube, _shadowCubeVerts[i])
        core_mat4_mul_vec3_perspective(out, _matProj, out)

        if out[Z] < 0 then
            table_insert(points, { out[X], out[Y] })
        end
    end

    _polysTemplate[_shadowCubeIdx] = core_geom2d_convex_hull(points)
    _polys[_shadowCubeIdx] = _polysTemplate[_shadowCubeIdx]
end

_cubeMaxY = 0 -- Stores the lowest-most Y screen coordinate of the cube. Used for the bubbles emitter.

function renderCube()
    _cubeMaxY = 0
    for i = 1, #_polysTemplate[_shadowCubeIdx] do
        local p = _polysTemplate[_shadowCubeIdx][i]
        local n = _polysTemplate[_shadowCubeIdx][i % #_polysTemplate[_shadowCubeIdx] + 1]


        if p[Y] > _cubeMaxY then
            _cubeMaxY = p[Y]
        end

        line(p[X], p[Y] + _cubeRenderOffY[1], n[X], n[Y] + _cubeRenderOffY[1], _shadowCubeColors[1])

        local r1 = _cubeRandRange1[1]
        if r1 ~= 0 then
            line(
                p[X] + core_math_randf(-r1, r1),
                p[Y] + core_math_randf(-r1, r1) + _cubeRenderOffY[1],
                n[X] + core_math_randf(-r1, r1),
                n[Y] + core_math_randf(-r1, r1) + _cubeRenderOffY[1],
                _shadowCubeColors[2])
        end

        local r2 = _cubeRandRange1[1]
        if r2 ~= 0 then
            line(
                p[X] + core_math_randf(-r2, r2),
                p[Y] + core_math_randf(-r2, r2) + _cubeRenderOffY[1],
                n[X] + core_math_randf(-r2, r2),
                n[Y] + core_math_randf(-r2, r2) + _cubeRenderOffY[1],
                _shadowCubeColors[3])
        end
    end
end

function renderShadowScene(t)
    _shadowClipsIdx = (_shadowClipsIdx % #_shadowClips) + 1

    -- Cube position is also the light position.
    updateCube(t)
    updateShadowEntityPositions(t)

    -- Ugly: re-init everything because segments might have been skipped.
    _segments = {}
    _rays = {}
    _uniqueRays = {}

    polysToSegments(_polys, _segments) -- Always needed, regardless of re-init.
    initRays(_rays, _segments)
    initUniqueRays(_uniqueRays, _rays)

    updateRays(_rays, _segments)
    updatIntersects(_rays, _segments)
    _uniqueRaysCount = updateUniqueRays(_uniqueRays, _rays)
    renderTris(_uniqueRays, _uniqueRaysCount)
    renderLightMap(_lightMap, _lightMapSize, _uniqueRays[1].a[X], _uniqueRays[1].a[Y], t)

    ---
    vbank(1)
    cls(0)
    renderCube()

    ---
    vbank(0)
end

--- HAVE2RUN EXECUTE

function renderHave2RunTexts()
    local colIdx = _runText1ColorIdx[1] // 1

    if colIdx > 1 then
        -- 1. SOME DAYS YOU
        core_pal_swap(15, _runTextFadeColors[colIdx])
        font(_runTexts[1], 25 + _runLine1Pos[1], 65 + _runLine1Pos[2], 0, 6, 8, false, 1)
    end

    colIdx = _runText2ColorIdx[1] // 1
    if colIdx > 1 then
        -- LOVE
        core_pal_swap(15, _runTextFadeColors[colIdx])
        font(_runTexts[2], 131 + _runLine1Pos[1], 65 + _runLine1Pos[2], 0, 6, 8, false, 1)
    end

    colIdx = _runText3ColorIdx[1] // 1
    if colIdx > 1 then
        -- TO RUN
        core_pal_swap(15, _runTextFadeColors[colIdx])
        font(_runTexts[3], 169 + _runLine1Pos[1], 65 + _runLine1Pos[2], 0, 6, 8, false, 1)
    end

    colIdx = _runText4ColorIdx[1] // 1
    if colIdx > 1 then
        -- 2. SOME DAYS
        core_pal_swap(15, _runTextFadeColors[colIdx])
        font(_runTexts[4], 25 + _runLine2Pos[1], 65 + _runLine2Pos[2], 0, 6, 8, false, 1)
    end

    colIdx = _runText5ColorIdx[1] // 1
    if colIdx > 1 then
        -- HATE
        core_pal_swap(15, _runTextFadeColors[colIdx])
        font(_runTexts[5], 131 + _runLine2Pos[1], 65 + _runLine2Pos[2], 0, 6, 8, false, 1)
    end

    colIdx = _runText6ColorIdx[1] // 1
    if colIdx > 1 then
        -- TO RUN
        core_pal_swap(15, _runTextFadeColors[colIdx])
        font(_runTexts[6], 169 + _runLine2Pos[1], 65 + _runLine2Pos[2], 0, 6, 8, false, 1)
    end

    colIdx = _runText7ColorIdx[1] // 1
    if colIdx > 1 then
        -- 3. EVERY DAY YOU
        core_pal_swap(15, _runTextFadeColors[colIdx])
        font(_runTexts[7], 25 + _runLine3Pos[1], 65 + _runLine3Pos[2], 0, 6, 8, false, 1)
    end

    colIdx = _runText8ColorIdx[1] // 1
    if colIdx > 1 then
        -- HAVE
        core_pal_swap(15, _runTextFadeColors[colIdx])
        font(_runTexts[8], 131 + _runLine3Pos[1], 65 + _runLine3Pos[2], 0, 6, 8, false, 1)
    end

    colIdx = _runText9ColorIdx[1] // 1
    if colIdx > 1 then
        -- TO RUN
        core_pal_swap(15, _runTextFadeColors[colIdx])
        font(_runTexts[9], 169 + _runLine3Pos[1], 65 + _runLine3Pos[2], 0, 6, 8, false, 1)
    end
    core_pal_swap(15, 15)
end

function renderHave2RunScene(t)
    cls(0)
    renderRunnerFloor()
    core_dither_render_horizontal(0, 91, 20, 45, 0, false, CoreDitherMatrixBayer4x4)
    renderHave2RunTexts()
    renderRunners(t)
    core_grad_mix(_gradSkylineAndFloor, _gradSkylineAndFloorFlash, _gradSkylineAndFloorMix, _runnerSyncSnare)
end

function bdrHave2RunScene(scanline)
    core_grad_bdr(scanline, _gradSkylineAndFloorMix, 12)
end

--- RUNNER EXECUTE

_sunPosX = _runnerSunOffX + 330

function renderSun(t)
    _sunPosX = _sunPosX - _runnerSunSpeed[1] / 60
    local posX = _sunPosX // 1

    circ(posX, 55 + _sunPosY[1], 45, 11)
    pix(posX + 8, 10 + _sunPosY[1], 11) -- A single dither pixel XD
    rect(0, 26 + _sunPosY[1], CORE_WIDTH, 1, 10)
    rect(0, 32 + _sunPosY[1], CORE_WIDTH, 2, 10)
    rect(0, 38 + _sunPosY[1], CORE_WIDTH, 3, 10)
    rect(0, 45 + _sunPosY[1], CORE_WIDTH, 4, 10)
    rect(0, 55 + _sunPosY[1], CORE_WIDTH, 6, 10)
    rect(0, 63 + _sunPosY[1], CORE_WIDTH, 40, 10)
    rect(0, 63, CORE_WIDTH, 68, 10)
end

function renderHeli(xoff, t)
    local x = (480 + xoff) // 1

    if x > 240 or x < -(_imgHeli.width + 10) then
        return
    end

    local y = (15 + 2 * sin(t.elapsed * 2) + 2 * cos(t.elapsed * 3)) // 1
    local freq = .1
    local blinker = (t.elapsed % freq) < freq * .5

    core_blit_render(_imgHeli, x, y, 0, 12)

    if blinker then
        line(x + 2, y + 1, x + 6, y + 1, 12)
        pix(x + 1, y + 5, 12)
    else
        line(x + 8, y + 1, x + 12, y + 1, 12)
        pix(x + 1, y + 3, 12)
    end
end

_treePosX = _runnerBGOffX + 288
_backPosX = _runnerBGOffX + 240
_frontPosX = _runnerBGOffX + 240

function renderSkyline(t)
    local yBack = 2
    local yFront = 10
    local yTrees = 57

    _backPosX = _backPosX - 20 / 60
    local xBack = _backPosX // 1

    _frontPosX = _frontPosX - 24 / 60
    local xFront = _frontPosX // 1

    _treePosX = _treePosX - 30 / 60
    local xTrees = _treePosX // 1

    clip(_runnerSkylineBackClipPosX[1], 0, 240, 80)
    core_blit_render(_imgSkylineBack, xBack, yBack, 0, 12)
    core_blit_render(_imgSkylineBackShort, (xBack + _imgSkylineBack.width) // 1, yBack + 34, 0, 12)
    core_blit_render(_imgSkylineBack, xBack + 2 * _imgSkylineBack.width, yBack, 0, 12)
    core_blit_render(_imgSkylineBack, xBack + 3 * _imgSkylineBack.width, yBack, 0, 12)
    clip()

    clip(_runnerSkylineFrontClipPosX[1], 0, 240, 80)
    core_blit_render(_imgSkylineFront, xFront, yFront, 0)
    core_blit_render(_imgSkylineFront, xFront + 1 + _imgSkylineFront.width, yFront, 0)
    core_blit_render(_imgSkylineFront, xFront + 1 + 2 * _imgSkylineFront.width, yFront, 0)
    core_blit_render(_imgSkylineFront, xFront + 1 + 3 * _imgSkylineFront.width, yFront, 0)
    clip()

    clip(_runnerTreesClipPosX[1], 0, 240, 80)
    core_blit_render(_imgSkylineTrees, xTrees, yTrees, 0, 14)
    core_blit_render(_imgSkylineTrees, xTrees + _imgSkylineTrees.width, yTrees, 0, 14)
    core_blit_render(_imgSkylineTrees, xTrees + 2 * _imgSkylineTrees.width, yTrees, 0, 14)
    core_blit_render(_imgSkylineTrees, xTrees + 3 * _imgSkylineTrees.width, yTrees, 0, 14)
    clip()

    renderHeli(xFront, t)
end

function renderGreetsBG(t)
    _runnerGreetsBGPosX = (_runnerGreetsBGPosX - 93.5 / 60 * _runnerGreetsFactor)

    local xpos = _runnerGreetsBGPosX
    local ypos = 73

    core_pal_swap(15, 13)
    font(_txtGreets1, xpos, ypos, 0, 8, 8, false, 4)
    font(_txtGreets1, xpos - 1, ypos + 1, 0, 8, 8, false, 4)

    core_pal_swap(15, 14)
    font(_txtGreets1, xpos - 2, ypos + 2, 0, 8, 8, false, 4)

    core_pal_swap(15, 15)
end

function renderGreetsFG(t)
    _runnerGreetsFGPosX = (_runnerGreetsFGPosX - 120 / 60 * _runnerGreetsFactor)
    local xpos = _runnerGreetsFGPosX
    local ypos = 93

    core_pal_swap(15, 13)
    font(_txtGreets2, xpos, ypos, 0, 8, 8, false, 7)
    font(_txtGreets2, xpos - 1, ypos + 1, 0, 8, 8, false, 7)
    font(_txtGreets2, xpos - 2, ypos + 2, 0, 8, 8, false, 7)
    font(_txtGreets2, xpos - 3, ypos + 3, 0, 8, 8, false, 7)

    core_pal_swap(15, 14)
    font(_txtGreets2, xpos - 4, ypos + 4, 0, 8, 8, false, 7)

    core_pal_swap(15, 15)
end

function renderRunnerFloor()
    local offY = 2
    for i = 1, #_runnerFloorLines do
        local f = _runnerFloorLines[i]
        line(f.x[1], f.y + offY, f.x[1] + 240, f.y + offY, 12)
        if f.x[1] < 0 then
            pix(f.x[1] + 241, f.y + offY, 12)
            pix(f.x[1] + 242, f.y + offY, 3)
            pix(f.x[1] + 243, f.y + offY, 2)
        elseif f.x[1] < 244 then
            pix(f.x[1] - 1, f.y + offY, 12)
            pix(f.x[1] - 2, f.y + offY, 3)
            pix(f.x[1] - 3, f.y + offY, 2)
        end
    end
end

function renderDoggy(t)
    local idx = (20 * t.elapsed % #_imgDoggySprites) // 1 + 1
    core_blit_render(_imgDoggySprites[idx], (_doggyPosX[1]) // 1, 79, 0)
end

function renderRunnerBulk(t)
    local idx = (20 * t.elapsed % #_runnerSprites) // 1 + 1

    for i = 1, #_runnerBulk do
        local b = _runnerBulk[i]

        b.x = b.x + b.speed * t.delta

        local s = _runnerSprites[(idx + b.sprIdx) % #_runnerSprites + 1]
        spr(s.idx,
            s.offX + b.x,
            b.y,
            s.bgCol, 1, 0, s.rot, s.sprW, s.sprH)
    end
end

function renderRunners(t)
    local idx = (20 * t.elapsed % #_runnerSprites) // 1 + 1
    local offX = _runnersOffX[1]

    local s = _runnerSprites[(idx + 0) % #_runnerSprites + 1]
    spr(s.idx,
        s.offX + (offX + 50 + 40 * sin(.3 + .5 * t.elapsed)) // 1,
        55,
        s.bgCol, 1, 0, s.rot, s.sprW, s.sprH)

    s = _runnerSprites[(idx + 2) % #_runnerSprites + 1]
    spr(s.idx,
        s.offX + (offX + 80 + 70 * sin(.6 + .4 * t.elapsed)) // 1,
        61,
        s.bgCol, 1, 0, s.rot, s.sprW, s.sprH)

    s = _runnerSprites[(idx + 4) % #_runnerSprites + 1]
    spr(s.idx,
        s.offX + (offX + 40 + 30 * sin(.4 + .6 * t.elapsed)) // 1,
        70,
        s.bgCol, 1, 0, s.rot, s.sprW, s.sprH)
end

function renderRunnerSwipe()
    -- In fact only 17 swipes are used. Atm 136 with step size 8. 136/8 = 17
    for i = 1, #_runnerSwipesX, 8 do
        rect(-2, i - 1, _runnerSwipesX[i][1] + 2, 8, 0)
    end
end

function renderRunnerTransitionOut()
    rect(0, _runnerTransitionOutTopY[1], 240, 68, 0)
    rect(0, _runnerTransitionOutBottomY[1], 240, 68, 0)
end

function renderRunnerScene(t)
    -- Use color 10 as BG clear so palette can be modified in BDR function without affecting border color (0).
    cls(10)

    core_grad_mix(_gradSkylineAndFloor, _gradSkylineAndFloorFlash, _gradSkylineAndFloorMix,
        _runnerSyncSnare * _floorFlashFactor[1])

    if _runnerGradBGMixFactor[1] > 0 then
        local step = .05 -- Lerp in steps of 0.05.
        core_grad_mix(_gradBGPink, _gradRunnerBGNight, _gradRunnerBGMix, ((_runnerGradBGMixFactor[1] / step + .5) // 1) *
            step)
    end

    renderSun(t)
    renderSkyline(t)

    if _runnerSwipesEnabled then
        renderRunnerSwipe()
    end

    renderRunnerFloor()
    renderGreetsBG(t)

    if _runnerBulkEnabled then
        renderRunnerBulk(t)
    end

    renderRunners(t)

    if _doggyPosX[1] > -40 then
        renderDoggy(t)
    end

    renderGreetsFG(t)

    core_dither_render_horizontal(0, 91, 20, 45, 10, false, CoreDitherMatrixBayer4x4)

    if _runnerBlackBayerX[1] > -20 then
        core_dither_render_horizontal(_runnerBlackBayerX[1] // 1, 91, 20, 45, 0, false, CoreDitherMatrixBayer4x4)
    end

    if _runnerTransitionOutBottomY[1] < 136 then
        renderRunnerTransitionOut()
    end
end

function bdrGreetsLettersFront(scanline, gradient, colorIdx, off)
    if scanline >= 58 and scanline < 140 then
        scanline = (scanline + off - 1) % #gradient + 1
        local i = 1 + (scanline - 4) * 3
        core_pal_apply_color(colorIdx, gradient[i], gradient[i + 1], gradient[i + 2])
    end
end

function bdrRunnerScene(scanline)
    if scanline > 68 and scanline < 136 then
        -- Letters top.
        local pos = 1 + 3 * scanline - 4
        _gradLettersTop[pos] = _gradLettersTopTemplate[pos] + _runnerSyncSnare * (255 - _gradLettersTopTemplate[pos])
        _gradLettersTop[pos + 1] = _gradLettersTopTemplate[pos + 1] +
            _runnerSyncSnare * (255 - _gradLettersTopTemplate[pos + 1])
        _gradLettersTop[pos + 2] = _gradLettersTopTemplate[pos + 2] +
            _runnerSyncSnare * (255 - _gradLettersTopTemplate[pos + 2])
        core_grad_bdr(scanline, _gradLettersTop, 13)
    end

    -- Letters front.
    bdrGreetsLettersFront(scanline, _gradLettersFront, 14, -5 + (15 * sin(_localTimer.elapsed + scanline * .1)) // 1)

    -- Pink BG.
    core_grad_bdr(scanline, _gradRunnerBGMix, 10)


    if scanline < 68 then
        -- Sun.
        core_grad_bdr_offset(scanline, _gradSun, 11, (_gradSunPosY[1]) // 1)

        -- Windows.
        core_grad_bdr_offset(scanline, _gradWindows, 15,
            (68 * sin(_localTimer.elapsed + 10 * sin(scanline * .05 + .05 * _localTimer.elapsed))) // 1)
    end

    -- Color 12: Shared between BG layer and foreground floor stripes.
    core_grad_bdr(scanline, _gradSkylineAndFloorMix, 12)
end

--- TRANSIT2VOX EXECUTE

function renderVoxTransitionIn()
    -- Top cover.
    rect(0, 0 + _voxCoverTopY[1], 240, 68, 0)
    line(0, 68 + _voxCoverTopY[1], 240, 68 + _voxCoverTopY[1], 2)

    -- Bottom cover.
    rect(0, 68 + _voxCoverBottomY[1], 240, 68, 0)
    line(0, 68 + _voxCoverBottomY[1], 240, 68 + _voxCoverBottomY[1], 2)
end

function renderTransit2VoxTexts(t)
    if _voxTxts[6].x[1] <= -50 then
        return
    end

    for i = 1, #_voxTxts do
        local colIdx = _voxTxts[i].col[1] // 1
        local yOff = sin(2.14 + t.elapsed * 4) * _voxTxts[i].amp[1]
        core_pal_swap(15, _voxTextFadeColors[colIdx])
        font(_voxTxts[i].txt, _voxTxts[i].x[1], 65 + yOff, 0, 4, 8, false, 1)
    end

    core_pal_swap(15, 15)
end

function updateVoxStripes()
    core_grad_clear(_voxGradStripes, 0, 0, 0)

    local pulse = _voxKickSnare4 * _voxStripesPulseFactor[1]

    for i = #_stripes, 1, -1 do
        for j = 1, #_stripes[i] do
            local s = _stripes[i][j]
            for y = s.y, s.y + s.h - 1 do
                if _voxStripesModeBlackEnables then
                    core_grad_pix(_voxGradStripes, (y + _stripes[i].yoff[1]) // 1,
                        0 + (s.cf[1] - 0) * pulse,
                        0 + (s.cf[2] - 0) * pulse,
                        0 + (s.cf[3] - 0) * pulse)
                else
                    core_grad_pix(_voxGradStripes, (y + _stripes[i].yoff[1]) // 1,
                        s.c[1] + (s.cf[1] - s.c[1]) * pulse,
                        s.c[2] + (s.cf[2] - s.c[2]) * pulse,
                        s.c[3] + (s.cf[3] - s.c[3]) * pulse)
                end
            end
        end
    end
end

_voxTimePosX = 240

function renderVoxLogos(t)
    local yy, max
    if _voxLinesSpectroxFactor[1] < 1 then
        -- Top logo.
        core_blit_render(_imgSpectrox, 120 - 177 // 2, 8, 0)

        max = #_voxLinesSpectrox * _voxLinesSpectroxFactor[1]
        for i = 1, max do
            yy = 7 + _voxLinesSpectrox[i]
            -- Color 1 is used for BDR.
            line(0, yy, 240, yy, 1)
        end
    end

    if _voxTimePosX >= -_imgTime.width then
        _voxTimePosX = _voxTimePosX + _voxTimeSpeedX[1]
        _voxTimePosX = _voxTimePosX
        -- Reflection.
        core_blit_scale(_imgTime, 0, 0, _imgTime.width, _imgTime.height, _voxTimePosX // 1, 114 + 2 * _imgTime.height,
            _imgTime.width, -_imgTime.height, 0, 9)
        -- Bottom logo.
        core_blit_render(_imgTime, _voxTimePosX // 1, 114, 0)
    end
end

--- White flash the palette.
--- @param srcPal any
--- @param p any
--- @param excludeMap any Color indices to exclude, e.g. excludeMap[0] = true
function logoFlash(srcPal, p, excludeMap)
    p = p < 0 and 0 or p
    p = p > 1 and 1 or p
    local dstPal = {}
    for i = 1, #srcPal do
        if not excludeMap[(i - 1) // 3] then
            dstPal[i] = srcPal[i] + p * (255 - srcPal[i])
        else
            dstPal[i] = srcPal[i]
        end
    end
    core_pal_apply(dstPal)
end

-- Determine which colors to exclude from the logo flash. See palette of the Spectrox logo pcx/gif.
_voxLogoFlashExcludedColorMap = {}
_voxLogoFlashExcludedColorMap[0] = true
_voxLogoFlashExcludedColorMap[3] = true
_voxLogoFlashExcludedColorMap[4] = true
_voxLogoFlashExcludedColorMap[5] = true
_voxLogoFlashExcludedColorMap[6] = true
_voxLogoFlashExcludedColorMap[7] = true
_voxLogoFlashExcludedColorMap[8] = true
_voxLogoFlashExcludedColorMap[9] = true

function renderTransit2VoxScene(t)
    cls(1)
    updateVoxStripes()
    renderVoxLogos(t)
    renderTransit2VoxTexts(t)
    renderVoxTransitionIn()
    logoFlash(_imgSpectrox.palette, _voxLogoSyncSnare8 * _voxLogoFlashFactor, _voxLogoFlashExcludedColorMap)
end

function bdrTransit2VoxScene(scanline)
    core_grad_bdr_offset(scanline, _voxGradStripes, 1, 0)
end

--- VOX EXECUTE
--- See: https://github.com/s-macke/VoxelSpace
--- See: https://github.com/gustavopezzi/voxelspace

function voxMap(x, y)
    return _voxMap[1 + x + y * _voxMapW]
end

function updateVoxMap(t)
    if _voxOrbitModeEnabled then
        local r = _voxOrbitCamRadius + 5 + 5 * sin(t.elapsed)
        local tx, ty = _voxOrbitCamTargetPos[1], _voxOrbitCamTargetPos[2]
        local angle = -t.elapsed

        _voxCam.pos.x = tx + cos(angle) * r
        _voxCam.pos.y = ty + sin(angle) * r
        _voxCam.angle = atan(ty - _voxCam.pos.y, tx - _voxCam.pos.x)
    else
        _voxCam.pos.x = _voxCam.pos.x + _voxSpdX[1] * t.delta
        _voxCam.pos.y = _voxCam.pos.y + _voxSpdY[1] * t.delta
        _voxCam.angle = _voxAng[1]
    end

    _voxCam.scale = _voxScale[1]
    _voxCam.horizon = _voxHoriz[1]
    _voxCam.height = _voxHeight[1]
end

function renderVoxMap(t)
    _voxClipIdx = (_voxClipIdx % #_voxClips) + 1

    -- Clear screen in the clip region that is being rendered.
    clip(_voxClips[_voxClipIdx][1], 0, _voxClips[_voxClipIdx][2] - _voxClips[_voxClipIdx][1] + 1, CORE_HEIGHT)
    cls(0)
    clip()

    -- Adjust dithering at the center seam. Makes sure that two dither pixels are not adjacent.
    local ditherOffset = _voxClipIdx - 1

    local camSin = sin(_voxCam.angle)
    local camCos = cos(_voxCam.angle)

    local pl = {
        x = camCos * _voxCam.dist + camSin * _voxCam.dist,
        y = camSin * _voxCam.dist - camCos * _voxCam.dist
    }
    local pr = {
        x = camCos * _voxCam.dist - camSin * _voxCam.dist,
        y = camSin * _voxCam.dist + camCos * _voxCam.dist
    }

    local invCamDist = 1 / _voxCam.dist
    local invW = 1 / CORE_WIDTH

    local b = {
        x = pl.x * invCamDist,
        y = pl.y * invCamDist
    }

    local s = {
        x = (pr.x - pl.x) * invW * invCamDist,
        y = (pr.y - pl.y) * invW * invCamDist
    }

    for i = _voxClips[_voxClipIdx][1], _voxClips[_voxClipIdx][2], 2 do
        local d = {
            x = b.x + s.x * i,
            y = b.y + s.y * i
        }

        local r = {
            x = _voxCam.pos.x,
            y = _voxCam.pos.y
        }

        local maxH = CORE_HEIGHT - 1
        local lastY = 0

        if _voxIsTwister then
            -- Find topmost pixel per x and z.
            for z = 1, _voxCam.dist, 24 do
                r.x = r.x + d.x * .5
                r.y = r.y + d.y * .5

                local hv = voxMap(r.x // 1 % _voxMapW, r.y // 1 % _voxMapH)
                local y = (((_voxCam.height - hv) / z * _voxCam.scale) + _voxCam.horizon) // 1

                if y < maxH then
                    for j = y, maxH do
                        lastY = y
                    end
                    maxH = y
                end
            end

            -- Reset.
            r = {
                x = _voxCam.pos.x,
                y = _voxCam.pos.y
            }
            maxH = CORE_HEIGHT - 1
        end

        for z = 1, _voxCam.dist, 24 do
            r.x = r.x + d.x * .5
            r.y = r.y + d.y * .5

            local hv = voxMap(r.x // 1 % _voxMapW, r.y // 1 % _voxMapH)
            local y = (((_voxCam.height - hv) / z * _voxCam.scale) + _voxCam.horizon) // 1

            if y < maxH then
                local shade
                if _voxShadePulseEnabled then
                    shade = 1 + ((5 + (8 + 8 * sin(t.elapsed * 2))) * hv // 255)
                else
                    shade = 1 + (_voxLightAmp[1] * hv) // 255
                end
                for j = y, maxH do
                    local col = shade
                    if (0 + (30 * hv // 255)) & 1 == 1 then
                        col = shade + (i ~ j) % 2
                    end
                    if col > 15 then
                        col = 15
                    end

                    local yoff = _voxYOff[1]
                    local dstY = yoff + j

                    if dstY >= -1 and dstY < 136 then
                        if j < _voxDitherDist[1] + abs(68 - lastY) then
                            if _voxDitherHoriz[1] >= _voxCam.dist then
                                line(i, dstY, i + 1, dstY + 1, col)
                            else
                                if z > _voxDitherHoriz[1] then
                                    -- Far horizon dither.
                                    if j & 1 == 0 then
                                        pix(i + ditherOffset, dstY, col)
                                    end
                                elseif z > _voxDitherHoriz[2] then
                                    -- Near horiozon dither.
                                    if j & 1 == 0 then
                                        line(i + ditherOffset, dstY, i + 1 + ditherOffset, dstY + 1, col)
                                    end
                                else
                                    line(i, dstY, i + 1, dstY + 1, col)
                                end
                            end
                        end
                    end
                end
                maxH = y
            end
        end
    end
end

function updateVoxGradients(t)
    -- VBank0
    if _voxGradStripedEnabled then
        updateVoxStripes()
    end

    core_grad_mix(_voxGradVbank0Src, _voxGradVbank0Dst, _voxGradVbank0, _voxGradVbank0Mix[1])

    -- VBank1
    core_grad_mix(_voxGradVbank1Src, _voxGradVbank1Dst, _voxGradVbank1, _voxGradVbank1Mix[1])
end

function renderVoxScene(t)
    updateVoxGradients(t)

    vbank(0)
    cls(1)

    renderTransit2VoxTexts(t)
    renderVoxLogos(t)

    -- Dither in front of incoming twister.
    local coverEnabled = _voxRectX[1] < 240
    local coverPosX
    if coverEnabled then
        coverPosX = (_voxRectX[1] + _voxRectAmp[1] * sin((2 + t.elapsed) * 3)) // 1
        core_dither_render_horizontal(coverPosX - 20, 50, 50, 40, 0, false, CoreDitherMatrixBayer4x4)
    end

    renderVoxTransitionIn()
    updateVoxMap(t)

    if _voxLogoFlashFactor > 0 then
        logoFlash(_imgSpectrox.palette, _voxLogoSyncSnare8 * _voxLogoFlashFactor, _voxLogoFlashExcludedColorMap)
    end

    ---
    vbank(1)
    -- No cls as we only render alternating clips to gain better perfomance.
    renderVoxMap(t)

    -- Used for fade-to-black scene cuts.
    core_pal_apply_black_fade(_voxPal, _voxFlashFactor[1]) -- vbank1

    -- Rect to cover voxels at the beginning.
    if coverEnabled then
        rect(coverPosX, 0, 240, 136, 0)
    end

    if _cubeEnabled then
        updateCube(t)
        renderCube()
    end

    updateWaterfieldPos(t)
end

function bdrVoxScene(scanline)
    vbank(0)
    core_grad_bdr_offset(scanline, _voxGradVbank0, 1, _bdrVox0OffFunc())
    vbank(1)
    core_grad_bdr_offset(scanline, _voxGradVbank1, 1, _bdrVox1OffFunc())
end

--- WATERFIELD EXECUTE

function zComparator(a, b)
    return a[Z] < b[Z]
end

function updateWaterParticles(t)
    local dy = t.delta * _waterParticlesSpd[1]
    for i = 1, #_waterParticles do
        local particle = _waterParticles[i]
        particle[Y] = particle[Y] + dy

        if _waterBGParticlesSpawnEnabled then
            if particle[Y] > _waterParticlesDim then
                particle[Y] = particle[Y] - _waterParticlesDim2
            end
        end
    end

    local tr = core_mat4_translate(0, 0, -_waterParticlesDim2)

    core_mat4_set_identity(_matModelWaterParticles)
    core_mat4_mul(_matModelWaterParticles, tr, _matModelWaterParticles)

    for i = 1, #_waterParticles do
        local out = { 0, 0, 0 }
        core_mat4_mul_vec3(out, _matModelWaterParticles, _waterParticles[i])
        core_mat4_mul_vec3_perspective(out, _matProj, out)
        _waterParticlesTransformed[i] = { out[X], out[Y], out[Z] }
    end

    table_sort(_waterParticlesTransformed, zComparator)
end

function renderWaterParticle(i, isFlickering, t)
    local p = _waterParticlesTransformed[i]
    local c, n, p1x, p2x
    if p[Z] < 0 then
        c = 8 + 5 * (_waterParticlesDim2 + p[Z] + _waterParticlesDim) / _waterParticlesDim2
        n = t.elapsed * 8 + i * .1

        p1x = p[X] + cos(n) * 3
        p2x = p[X] + cos(1 + n) * 2

        if not isFlickering or (p1x // 1) & 1 == 0 then
            pix(p1x, p[Y] + sin(n) * 3, c)
        end

        if not isFlickering or (p2x // 1) & 1 == 0 then
            pix(p2x, p[Y] + sin(2 + n) * 4, c)
        end
    end
end

function renderWaterParticles(filterFunc, isFlickering, t)
    for i = 1, #_waterParticlesTransformed do
        if filterFunc(_waterParticlesTransformed[i][Z]) then
            renderWaterParticle(i, isFlickering, t)
        end
    end
end

function waterFilterFuncBack(a)
    return a <= -20
end

function waterFilterFuncFront(a)
    return a > -20
end

function updateWaterfieldPos(t)
    _waterBGPosY = _waterBGPosY + _waterfieldBGSpeed[1] * t.delta
end

function updateWaterfieldGradients()
    if _waterGradMixEnabled then
        core_grad_mix(_waterGradSplash, _waterGradBG, _waterGradVbank0, _waterGradVbank0Mix[1])
    end
end

function updateCubeColors()
    core_pal_apply_color(5, _waterCubeColors[1][1], _waterCubeColors[1][2], _waterCubeColors[1][3]) -- Glitch line 1.
    core_pal_apply_color(6, _waterCubeColors[2][1], _waterCubeColors[2][2], _waterCubeColors[2][3]) -- Glitch line 2.
    core_pal_apply_color(7, _waterCubeColors[3][1], _waterCubeColors[3][2], _waterCubeColors[3][3]) -- White main line of cube.
end

--- Create new particle emitter.
--- @param numParticles any
--- @param pos any e.g. { x = 0, y = 0 }
--- @param maxAge any max particle age in seconds
--- @param spawnFreq any particle spawning frequency in seconds
--- @param posVariance any particle position variance, e.g. { x = 25, y = 10 }
--- @param speedVariance any particle speed variance, e.g. { min = 80, max = 100 }
--- @param ampVariance any particle sine amp variance, e.g. { min = 5, max = 10 }
--- @param freqVariance any particle frequency variance, e.g. { min = 4, max = 8 }
function newEmitter(numParticles, pos, maxAge, spawnFreq, posVariance, speedVariance, ampVariance, freqVariance)
    local e = {
        pos = pos,
        offset = { x = 0, y = 0 }, -- Offset used for rendering. No impact on actual particle calculations.
        particleMaxAge = maxAge,
        particleSpawnFreq = spawnFreq,
        particleSpawnPosVariance = posVariance,
        particleSpawnSpeedVariance = speedVariance,
        particleSpawnAmpVariance = ampVariance,
        particleSpawnFreqVariance = freqVariance,
        timeSinceLastSpawn = 0,
        isParticleRenderEnabled = true,
        isParticleSpawnEnabled = true,
    }

    local particles = {}
    for i = 1, numParticles do
        table_insert(particles,
            {
                isAlive = false,
                age = 0,
                pos = { x = 0, y = 0 },
                speed = 0,
                amp = { x = 0, y = 0 },
                freq = { x = 0, y = 0 },
                phase = { x = 0, y = 0 },
            }
        )
    end
    e.particles = particles

    return e
end

function resetEmitter(e)
    e.timeSinceLastSpawn = 0

    local p
    for i = 1, #e.particles do
        p = e.particles[i]
        p.age = 0
        p.isAlive = false
        p.pos.x = 0
        p.pos.y = 0
        p.pos.speed = 0
    end
end

function initParticle(e, p)
    p.age = 0
    p.pos.x = e.pos.x + core_math_randf(-e.particleSpawnPosVariance.x, e.particleSpawnPosVariance.x)
    p.pos.y = e.pos.y + core_math_randf(-e.particleSpawnPosVariance.y, e.particleSpawnPosVariance.y)
    p.speed = core_math_randf(e.particleSpawnSpeedVariance.min, e.particleSpawnSpeedVariance.max)
    p.amp.x = core_math_randf(e.particleSpawnAmpVariance.min, e.particleSpawnAmpVariance.max)
    p.freq.x = core_math_randf(e.particleSpawnFreqVariance.min, e.particleSpawnFreqVariance.max)
    p.phase.x = core_math_randf(0, 10)
end

function emitParticle(e, t)
    e.timeSinceLastSpawn = e.timeSinceLastSpawn + t.delta

    if (e.isParticleSpawnEnabled and e.timeSinceLastSpawn > e.particleSpawnFreq) then
        local desiredSpawns = (e.timeSinceLastSpawn / e.particleSpawnFreq) // 1
        e.timeSinceLastSpawn = 0

        local spawnCnt = 0
        for i = 1, #e.particles do
            if not e.particles[i].isAlive then
                e.particles[i].isAlive = true
                initParticle(e, e.particles[i])

                spawnCnt = spawnCnt + 1
                if spawnCnt >= desiredSpawns then
                    break
                end
            end
        end
    end
end

function updateParticleProc(p, e, t)
    p.pos.y = p.pos.y - p.speed * t.delta
end

function updateEmitter(e, t)
    emitParticle(e, t)

    local p
    for i = 1, #e.particles do
        p = e.particles[i]

        if not p.isAlive then
            goto continue
        end

        p.age = p.age + t.delta
        if p.age > e.particleMaxAge then
            p.isAlive = false
        end

        updateParticleProc(p, e, t)

        ::continue::
    end
end

function renderEmitter(e, t, start, col)
    if not e.isParticleRenderEnabled then
        return
    end

    local p, x, y
    for i = start, #e.particles, 2 do
        p = e.particles[i]
        if p.isAlive then
            x = e.offset.x + p.pos.x + p.amp.x * sin(t.elapsed * p.freq.x + p.phase.x)
            y = e.offset.y + p.pos.y

            if p.age < .05 then
                pix(x, y, col)
            elseif p.age < .1 then
                rect(x, y, 2, 2, col)
            elseif p.age < .3 then
                circb(x, y, 2, col)
            elseif p.age < .5 then
                circb(x, y, 1, col)
            elseif p.age < .6 then
                rect(x, y, 2, 2, col)
            else
                pix(x, y, col)
            end
        end
    end
end

_waterTextPosY = 136

function renderCredits()
    _waterTextPosY = _waterTextPosY + _waterTextPosSpd
    local yoff = _waterTextPosY // 1
    local yoff1 = yoff + 1

    core_pal_swap(15, 13)
    for i = 1, #_txtCredits do
        font(_txtCredits[i].txt, _txtCredits[i].x + 1, _txtCredits[i].y + yoff1, 0, 4, 12, false, _txtCredits[i].s)
    end

    core_pal_swap(15, 2)
    for i = 1, #_txtCredits do
        font(_txtCredits[i].txt, _txtCredits[i].x, _txtCredits[i].y + yoff, 0, 4, 12, false, _txtCredits[i].s)
    end

    core_pal_swap(15, 15)
end

function renderWaterfieldScene(t)
    cls(1)

    updateWaterfieldGradients()
    core_pal_apply_mix(_waterPal, _waterPurplePal, _underwaterPurpleMix[1])
    updateWaterParticles(t)
    renderWaterParticles(waterFilterFuncBack, true, t)
    renderCredits()

    if _cubeEnabled then
        updateCubeColors()
        updateCube(t)
        _waterCubeBubblesEmitter.pos.y = _cubeMaxY
        _waterCubeBubblesEmitter.particleSpawnPosVariance.x = _waterCubeBubblesPosVarX[1]
        updateEmitter(_waterCubeBubblesEmitter, t)
        renderEmitter(_waterCubeBubblesEmitter, t, 1, 3)
        renderCube()
        renderEmitter(_waterCubeBubblesEmitter, t, 2, 12)
    end

    renderWaterParticles(waterFilterFuncFront, false, t)
    updateWaterfieldPos(t)
end

function bdrWaterfieldScene(scanline)
    core_grad_bdr_offset(scanline, _waterGradVbank0, 1, (_waterBGPosY) // 1)
    core_grad_bdr_offset(scanline, _voxGradLakes1, 2, (50 * _localTimer.elapsed) // 1)
    core_grad_bdr_offset(scanline, _waterBubblesGrad, 3, 0)
end

--- UNDERWATER EXECUTE

function blitWave(image, x, y, colorTransparent, colorOverride, amp, phase, phaseFactor, t)
    local max = math.max
    local min = math.min
    local sx0 = max(0, -x)                 -- Source X start.
    local sx1 = min(image.width, 240 - x)  -- Source X end.
    local sy0 = max(0, -y)                 -- Source Y start.
    local sy1 = min(image.height, 136 - y) -- Source Y end.

    if sx0 >= sx1 or sy0 >= sy1 then return end

    local tx, ty, idx, c
    for iy = sy0, sy1 - 1 do
        tx = (x + sx0 + (image.height - iy) / image.height * sin(phase + 2 * t.elapsed + phaseFactor * iy) * amp) // 1
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

function renderEndLogoBack(t)
    if _underwaterLogoMaskPosY[1] < 0 then
        local a = sin(_localTimer.elapsed * 2 - 1) * _underwaterEndLogoAmp[1]
        blitWave(_imgThe, 108, 14 + a, 0, 2, 6, 0, .5, t)
        blitWave(_imgThe, 108, 16 + a, 0, 2, 6, M_PI_4, .5, t)

        blitWave(_imgEnd, 76, 26 + a, 0, 2, 12, 0, 1.2, t)
        blitWave(_imgEnd, 76, 30 + a, 0, 11, 12, M_PI_4, 1.2, t)

        core_dither_rect(65, 9, 110, 51, 0, .5, CoreDitherMatrixBayer4x4)
    end
end

function renderEndLogo()
    if _underwaterLogoMaskPosY[1] < 0 then
        local a = sin(_localTimer.elapsed * 2) * _underwaterEndLogoAmp[1]

        core_blit_render(_imgThe, 108, (15 + _underwaterEndLogoPosY[1] + a) // 1, 0)
        core_blit_render(_imgEnd, 76, (28 + _underwaterEndLogoPosY[1] + a) // 1, 0)
    end

    if _underwaterLogoMaskPosY[1] < 0 and _underwaterLogoMaskPosY[1] > -120 then
        rect(65, _underwaterLogoMaskPosY[1] // 1, 110, 55, 0)
        core_dither_render_vertical(65, 55 + (_underwaterLogoMaskPosY[1]) // 1, 110, 20, 0, false,
            CoreDitherMatrixBayer4x4)
    end
end

function renderUnderwaterScene(t)
    local yOff

    vbank(0)
    cls(15)

    -- Flicker cube colors.
    if not _underwaterBGParticlesEnabled then
        local a = -100 + 230 + 4 + 4 * sin(48 * t.elapsed) + 6 + 6 * sin(16 * t.elapsed)
        a = a / (-100 + 250)
        core_pal_apply_color(13, (255 * a) // 1, (255 * a) // 1, (255 * a) // 1)
        core_pal_apply_color(11, (189 * a) // 1, (226 * a) // 1, (234 * a) // 1)
        core_pal_apply_color(6, (124 * a) // 1, (197 * a) // 1, (214 * a) // 1)
    end

    -- Light beam.
    core_grad_mix(_gradUnderwater, _gradUnderwaterBrightBase, _gradUnderwaterBrightMix, _underwaterLightMix[1])
    local triXoff = (sin(t.elapsed * 2) * 3) // 1
    tri(120, -100, 120 - 80 + triXoff, 120, 120 + 80 + triXoff, 120, 14)

    if _underwaterBGParticlesEnabled then
        core_pal_apply_mix(_waterPal, _waterPurplePal, _underwaterPurpleMix[1])

        updateWaterParticles(t)
        renderWaterParticles(waterFilterFuncBack, true, t)
        renderCredits()
        renderWaterParticles(waterFilterFuncFront, false, t)
    end

    yOff = _underwaterBGPosY[1]
    core_blit_render(_imgKelpBG, 0, (45 + yOff) // 1, 0)
    rect(0, 45 + yOff + _imgKelpBG.height, 240, 30, 3)

    yOff = _underwaterSandPosY[1]
    core_blit_render(_imgSand, 0, (54 + yOff) // 1, 0)

    updateEmitter(_underwaterCubeEmitter, t)
    renderEmitter(_underwaterCubeEmitter, t, 1, 6)


    _underwaterEmitter.offset.y = yOff
    updateEmitter(_underwaterEmitter, t)
    renderEmitter(_underwaterEmitter, t, 1, 6)
    renderEmitter(_underwaterEmitter, t, 2, 13)

    ---
    vbank(1)
    cls(0)

    yOff = _underwaterSandPosY[1]
    core_blit_render(_imgSideRocksLeft, 0, (69 + yOff) // 1, 0)
    core_blit_render(_imgSideRocksRight, 186, (72 + yOff) // 1, 0)

    renderEndLogoBack(t)
    if not _underwaterLogoTopmost then
        renderEndLogo()
    end

    renderEmitter(_underwaterCubeEmitter, t, 2, 15)

    yOff = _underwaterWaveyKelpPosY[1]
    blitWave(_imgKelp1, 42, (36 + yOff) // 1, 0, nil, 4, 0, .08, t)
    blitWave(_imgKelp2, 59, (40 + yOff) // 1, 0, nil, 8, 1.5, .08, t)
    blitWave(_imgKelp3, 166, (45 + yOff) // 1, 0, nil, 4, 1, .08, t)

    yOff = _underwaterFGPosY[1]
    core_blit_render(_imgSandFG, 0, (72 + yOff) // 1, 0)
    blitWave(_imgKelpFGLeft1, 0, (6 + yOff) // 1, 0, nil, 4, 0, .08, t)
    blitWave(_imgKelpFGLeft2, 9, (20 + yOff) // 1, 0, nil, 8, 1.5, .08, t)
    blitWave(_imgKelpFGLeft3, 13, (40 + yOff) // 1, 0, nil, 8, 0, .08, t)
    blitWave(_imgKelpFGLeft4, 19, (68 + yOff) // 1, 0, nil, 8, 2.5, .08, t)
    blitWave(_imgKelpFGRight1, 209, (81 + yOff) // 1, 0, nil, 4, 0, .08, t)
    blitWave(_imgKelpFGRight2, 209, (55 + yOff) // 1, 0, nil, 8, 1.5, .08, t)
    blitWave(_imgKelpFGRight3, 213, (37 + yOff) // 1, 0, nil, 8, 0.5, .08, t)
    blitWave(_imgKelpFGRight4, 220, (13 + yOff) // 1, 0, nil, 8, 0, .08, t)
    blitWave(_imgKelpFGRight5, 228, (25 + yOff) // 1, 0, nil, 8, 2, .08, t)
    blitWave(_imgKelpFGRight6, 232, (69 + yOff) // 1, 0, nil, 4, 0, .08, t)

    if _coverLinesFactor[1] > 0 then
        renderCoverLines(1)
    end

    if _underwaterLogoTopmost then
        renderEndLogo()
    end
end

function bdrUnderwaterScene(scanline)
    vbank(0)
    if _underwaterBGParticlesEnabled then
        core_grad_bdr_offset(scanline, _voxGradLakes1, 2, (50 * _localTimer.elapsed) // 1)
    end
    core_grad_bdr_offset(scanline, _gradUnderwater, 15, _underwaterGradPosY[1] // 1)
    core_grad_bdr_offset(scanline, _gradUnderwaterBrightMix, 14, _underwaterGradPosY[1] // 1)
end

--- HIDDEN EXECUTE


function renderHiddenBG(yoff)
    line(0, 32 + yoff, 240, 32 + yoff, 6)
    line(0, 103 + yoff, 240, 103 + yoff, 6)
    line(0, 121 + yoff, 240, 121 + yoff, 6)
end

function renderHiddenTitle(x, y, colOutline, colBody)
    core_pal_swap(13, colOutline)
    core_pal_swap(14, colOutline)
    core_pal_swap(15, colOutline)

    for j = -4, 8, 4 do
        for i = -1, 1, 1 do
            if i ~= 0 and j ~= 4 then
                font(_hiddenTitle, x + i, y + j, 0, 8, 8, false, 8)
            end
        end
    end

    core_pal_swap(13, colBody)
    core_pal_swap(14, colBody)
    core_pal_swap(15, colBody)

    for j = -3, 3, 3 do
        font(_hiddenTitle, x, y + j, 0, 8, 8, false, 8)
    end

    core_pal_swap(13, 13)
    core_pal_swap(14, 14)
    core_pal_swap(15, 15)
end

function renderHiddenDelayRects()
    -- 12x8 tiles fit perfectly.
    -- Alternative: 15x9 tiles of 16x15px. Results in just 1 pixel row missing at bottom.
    local numW, numH, w, h, cnt = 12, 8, 20, 17, 1
    for j = 1, numH do
        for i = 1, numW do
            core_dither_rect((i - 1) * w, (j - 1) * h, w, h, 0,
                1 -
                (_hiddenCoverProgress[1] * 2 * _hiddenDelayFactor - _hiddenDelayMap[cnt] / (_hiddenDelayMapMaxVal / _hiddenDelayFactor)),
                CoreDitherMatrixWildBayer8x8)
            cnt = cnt + 1
        end
    end
end

function renderHiddenScroller(y)
    core_pal_swap(13, 12)
    core_pal_swap(14, 14)
    core_pal_swap(15, 12)
    font(_hiddenScrollText, _hiddenScrollX, y + 1, 0, 4, 8, false, 1)

    core_pal_swap(13, 13)
    core_pal_swap(14, 14)
    core_pal_swap(15, 15)
    local txtWidth = font(_hiddenScrollText, _hiddenScrollX, y, 0, 4, 8, false, 1)

    _hiddenScrollX = _hiddenScrollX - 1
    if _hiddenScrollX < -txtWidth - 20 then
        _hiddenScrollX = 400
    end
end

function renderHiddenScene(t)
    core_sound_play(_hiddenTracklist, true)
    core_sound_update(_soundState)

    cls(0)
    local x = -132
    local xoff = 150 * sin(t.elapsed * 1.5)
    local yoff = -10

    renderHiddenBG(yoff)
    renderHiddenTitle(x + xoff, 46 + yoff, 6, 0)

    if _hiddenCoverProgress[1] < 1 then
        renderHiddenDelayRects()
    end

    renderHiddenScroller(110 + yoff)
end

function bdrHiddenScene(scanline)
    local off = (_localTimer.elapsed * 100) // 1
    core_grad_bdr_offset(scanline, _gradHidden, 6, off)
end

-------------------------------------------------------------------------------
--- SOUND SYNC
-------------------------------------------------------------------------------

function handleSyncKick(soundState)
    _syncKick4 = 1
end

function handleVoxLogoSyncSnare(soundState)
    if soundState.pattern == 0 then
        if soundState.row == 28
            or soundState.row == 44
            or soundState.row == 60
        then
            _voxLogoSyncSnare8 = 1
        end
    elseif soundState.pattern == 1 then
        if soundState.row == 12
            or soundState.row == 28
            or soundState.row == 44
        then
            _voxLogoSyncSnare8 = 1
        end
    end
end

function handleSyncSnare(soundState)
    _syncSnare4 = 1
    _syncSnare8 = 1

    if core_anim_is_running(_schedule, AnimNames.BLOB) then
        if soundState.pattern >= 2 and soundState.pattern <= 3 then
            if (soundState.pattern == 2 and soundState.row >= 4)
                or (soundState.pattern == 3 and soundState.row <= 58)
            then
                _blobSyncSnare = 1
                _blobListIdx = (_blobListIdx % #_blobList) + 1
            end
        end
    end

    if core_anim_is_running(_schedule, AnimNames.LISSA) then
        if soundState.pattern >= 6 then
            _lissaSyncColor = 1
        end
    end

    if core_anim_is_running(_schedule, AnimNames.TRANSIT2VOX) or core_anim_is_running(_schedule, AnimNames.VOX) then
        handleVoxLogoSyncSnare(soundState)
    end
end

function handleVoxSyncKickOrSnare(soundState)
    if soundState.pattern > 2 then
        return
    end

    if soundState.pattern == 2 then
        if soundState.row <= 48 then
            _voxKickSnare4 = 1
        end

        if soundState.row >= 48 then
            _voxStripesModeBlackEnables = true
        end
    end
end

function handleSyncKickOrSnare(soundState)
    _syncKickSnare4 = 1

    if core_anim_is_running(_schedule, AnimNames.VOX) then
        handleVoxSyncKickOrSnare(soundState)
    end
end

function handleRunnerSyncCustom(soundState)
    if soundState.track ~= 2 then
        return
    end

    if
        soundState.row == 2
        or soundState.row == 6
        or soundState.row == 10
        or soundState.row == 14
        or soundState.row == 18
        or soundState.row == 22
        or soundState.row == 26
        or soundState.row == 30
        or soundState.row == 34
        or soundState.row == 38
        or soundState.row == 42
        or soundState.row == 46
        or soundState.row == 50
        or soundState.row == 54
        or soundState.row == 58
        or soundState.row == 62
    then
        _runnerSyncSnare = 1
    end
end

function handleLissaSyncCustom(soundState)
    if soundState.pattern >= 5 and soundState.pattern <= 7 then
        if soundState.row == 12
            or soundState.row == 28
            or soundState.row == 44
            or soundState.row == 60
        then
            _lissaRotVelocity = _lissaRotImpulse
            _lissaSyncColor = 1
        end
    end

    if soundState.pattern == 5 then
        if soundState.row == 60 then
            _lissaRotVelocity = -_lissaRotImpulse * 1.5
            _lissaSyncColor = 1
        end
        if soundState.row == 62 then
            _lissaRotVelocity = _lissaRotImpulse
            _lissaSyncColor = 1
        end
    end
end

function handleSync(soundState)
    local isKick = (soundState.row) % 8 == 0 and core_sound_not_fired(soundState)
    local isSnare = (soundState.row + 4) % 8 == 0 and core_sound_not_fired(soundState)


    if core_anim_is_running(_schedule, AnimNames.LISSA) then
        handleLissaSyncCustom(soundState)
    end

    if core_anim_is_running(_schedule, AnimNames.HAVE2RUN) or core_anim_is_running(_schedule, AnimNames.RUNNER) then
        handleRunnerSyncCustom(soundState)
    end

    if isKick then
        core_sound_fire(soundState)
        handleSyncKick(soundState)
    end

    if isSnare then
        core_sound_fire(soundState)
        handleSyncSnare(soundState)
    end

    if isKick or isSnare then
        -- `core_sound_fire` not needed as it is executed before for sure.
        handleSyncKickOrSnare(soundState)
    end
end

function updateSyncVariables()
    local d4 = _timer.delta * 4
    local d8 = _timer.delta * 8
    _syncKick4 = core_math_lerp(_syncKick4, 0, d4)
    _syncSnare4 = core_math_lerp(_syncSnare4, 0, d4)
    _syncSnare8 = core_math_lerp(_syncSnare8, 0, d8)
    _syncKickSnare4 = core_math_lerp(_syncKickSnare4, 0, d4)


    if core_anim_is_running(_schedule, AnimNames.BLOB) then
        _blobSyncSnare = core_math_lerp(_blobSyncSnare, 0, d4)
    end
    if core_anim_is_running(_schedule, AnimNames.LISSA) then
        _lissaSyncColor = core_math_lerp(_lissaSyncColor, 0, d4)
    end
    if core_anim_is_running(_schedule, AnimNames.HAVE2RUN) or core_anim_is_running(_schedule, AnimNames.RUNNER) then
        _runnerSyncSnare = core_math_lerp(_runnerSyncSnare, 0, d4)
    end
    if core_anim_is_running(_schedule, AnimNames.TRANSIT2VOX) or core_anim_is_running(_schedule, AnimNames.VOX) then
        _voxKickSnare4 = core_math_lerp(_voxKickSnare4, 0, d4)
        _voxLogoSyncSnare8 = core_math_lerp(_voxLogoSyncSnare8, 0, d8)
    end
end

-------------------------------------------------------------------------------
--- TIC FUNCTIONS
-------------------------------------------------------------------------------

function BOOT()
    -- Hide mouse.
    poke(0x7FC3F, 1, 1)

    -- Border color
    poke(0x3FF8, 0)

    initFont()
    initScreenFaderScene()
    initPresentsScene()
    initTitleScene()
    initBlobsScene()
    initCreditsScene()
    initTransit2LissaScene()
    initLissajousScene()
    initTransit2ShadowScene()
    initShadowScene()
    initHave2RunScene()
    initRunnerScene()
    initTransit2VoxScene()
    initVoxScene()
    initWaterfieldScene()
    initUnderwaterScene()
    initHiddenScene()
end

function BDR(scanline)
    if core_anim_is_running(_schedule, AnimNames.HAVE2RUN) then
        bdrHave2RunScene(scanline)
    elseif core_anim_is_running(_schedule, AnimNames.RUNNER) then
        bdrRunnerScene(scanline)
    elseif core_anim_is_running(_schedule, AnimNames.TRANSIT2VOX) then
        bdrTransit2VoxScene(scanline)
    elseif core_anim_is_running(_schedule, AnimNames.VOX) then
        bdrVoxScene(scanline)
    elseif core_anim_is_running(_schedule, AnimNames.WATERFIELD) then
        bdrWaterfieldScene(scanline)
    elseif core_anim_is_running(_schedule, AnimNames.UNDERWATER) then
        bdrUnderwaterScene(scanline)
    elseif core_anim_is_running(_schedule, AnimNames.HIDDEN) then
        bdrHiddenScene(scanline)
    end
end

-- Initial anim.
_START_AT = AnimNames.STARTUP_DELAY
--_START_AT = AnimNames.BLOB
--_START_AT = AnimNames.LISSA
--_START_AT = AnimNames.TRANSIT2SHADOW
--_START_AT = AnimNames.HAVE2RUN
--_START_AT = AnimNames.TRANSIT2VOX
--_START_AT = AnimNames.WATERFIELD

_schedule.anims[_START_AT].isAutostart = true

-- Configure directly startable scenes. For debugging only.
if _START_AT == AnimNames.BLOB then
    _schedule.anims[AnimNames.BLOB].isAutostart = true
    _tracklist.tracks[2].bank = 0
    _tracklist.tracks[2].pattern = 1
    _tracklist.tracks[2].row = 55
    _tracklist.current = 2 - 1
    _musicStarted = true
elseif _START_AT == AnimNames.LISSA then
    _schedule.anims[AnimNames.LISSA].isAutostart = true
    _tracklist.tracks[2].bank = 0
    _tracklist.tracks[2].pattern = 4
    _tracklist.tracks[2].row = 63
    _tracklist.current = 2 - 1
    _musicStarted = true
elseif _START_AT == AnimNames.TRANSIT2SHADOW then
    _schedule.anims[AnimNames.TRANSIT2SHADOW].isAutostart = true
    _tracklist.tracks[2].bank = 0
    _tracklist.tracks[2].pattern = 7
    _tracklist.tracks[2].row = 54
    _tracklist.current = 2 - 1
    _musicStarted = true
elseif _START_AT == AnimNames.HAVE2RUN then
    _schedule.anims[AnimNames.HAVE2RUN].isAutostart = true
    _tracklist.tracks[2].bank = 0
    _tracklist.tracks[2].pattern = 12
    _tracklist.tracks[2].row = 0
    _tracklist.current = 2 - 1
    _musicStarted = true
elseif _START_AT == AnimNames.TRANSIT2VOX then
    _schedule.anims[AnimNames.TRANSIT2VOX].isAutostart = true
    _tracklist.tracks[3].bank = 0
    _tracklist.tracks[3].pattern = 2
    _tracklist.tracks[3].row = 59
    _tracklist.current = 3 - 1
    _musicStarted = true
elseif _START_AT == AnimNames.WATERFIELD then
    _schedule.anims[AnimNames.WATERFIELD].isAutostart = true
    _tracklist.tracks[4].bank = 0
    _tracklist.tracks[4].pattern = 6
    _tracklist.tracks[4].row = 63
    _tracklist.current = 4 - 1
    _musicStarted = true
end

function TIC()
    core_timer_start(_timer)
    core_timer_update(_timer)
    core_anim_process_schedule(_schedule, _timer)

    if _musicStarted then
        core_sound_play(_tracklist, false)
        core_sound_update(_soundState)
    end

    handleSync(_soundState)

    if core_anim_is_running(_schedule, AnimNames.STARTUP_DELAY) then
        core_anim_update_local_timer(_schedule, AnimNames.STARTUP_DELAY, _localTimer, _timer)
        renderStartupDelayScene(_localTimer)
    elseif core_anim_is_running(_schedule, AnimNames.SCREEN_FADER) then
        core_anim_update_local_timer(_schedule, AnimNames.SCREEN_FADER, _localTimer, _timer)
        renderScreenFaderScene(_localTimer)
    elseif core_anim_is_running(_schedule, AnimNames.PRESENTS) then
        core_anim_update_local_timer(_schedule, AnimNames.PRESENTS, _localTimer, _timer)
        renderPresentsScene(_localTimer)
    elseif core_anim_is_running(_schedule, AnimNames.TITLE) then
        core_anim_update_local_timer(_schedule, AnimNames.TITLE, _localTimer, _timer)
        renderTitleScene(_localTimer)
    elseif core_anim_is_running(_schedule, AnimNames.BLOB) then
        core_anim_update_local_timer(_schedule, AnimNames.BLOB, _localTimer, _timer)
        renderBlobScene(_localTimer)
    elseif core_anim_is_running(_schedule, AnimNames.CREDITS) then
        core_anim_update_local_timer(_schedule, AnimNames.CREDITS, _localTimer, _timer)
        renderCreditsScene(_localTimer)
    elseif core_anim_is_running(_schedule, AnimNames.TRANSIT2LISSA) then
        core_anim_update_local_timer(_schedule, AnimNames.TRANSIT2LISSA, _localTimer, _timer)
        renderTransit2LissaScene(_localTimer)
    elseif core_anim_is_running(_schedule, AnimNames.LISSA) then
        core_anim_update_local_timer(_schedule, AnimNames.LISSA, _localTimer, _timer)
        renderLissaScene(_localTimer)
    elseif core_anim_is_running(_schedule, AnimNames.TRANSIT2SHADOW) then
        core_anim_update_local_timer(_schedule, AnimNames.TRANSIT2SHADOW, _localTimer, _timer)
        renderTransit2ShadowScene(_localTimer)
    elseif core_anim_is_running(_schedule, AnimNames.SHADOW) then
        -- Hack: Shadow scene uses local timer of previous scene,
        --       so that shadow cube sine positions transition continuously.
        core_anim_update_local_timer(_schedule, AnimNames.TRANSIT2SHADOW, _localTimer, _timer)
        renderShadowScene(_localTimer)
    elseif core_anim_is_running(_schedule, AnimNames.HAVE2RUN) then
        core_anim_update_local_timer(_schedule, AnimNames.HAVE2RUN, _localTimer, _timer)
        renderHave2RunScene(_localTimer)
    elseif core_anim_is_running(_schedule, AnimNames.RUNNER) then
        -- Hack: Runner scene uses local timer of previous scene,
        --       so that runner sine positions transition continuously.
        core_anim_update_local_timer(_schedule, AnimNames.HAVE2RUN, _localTimer, _timer)
        renderRunnerScene(_localTimer)
    elseif core_anim_is_running(_schedule, AnimNames.TRANSIT2VOX) then
        core_anim_update_local_timer(_schedule, AnimNames.TRANSIT2VOX, _localTimer, _timer)
        renderTransit2VoxScene(_localTimer)
    elseif core_anim_is_running(_schedule, AnimNames.VOX) then
        core_anim_update_local_timer(_schedule, AnimNames.VOX, _localTimer, _timer)
        renderVoxScene(_localTimer)
    elseif core_anim_is_running(_schedule, AnimNames.WATERFIELD) then
        core_anim_update_local_timer(_schedule, AnimNames.WATERFIELD, _localTimer, _timer)
        renderWaterfieldScene(_localTimer)
    elseif core_anim_is_running(_schedule, AnimNames.UNDERWATER) then
        -- Hack: Underwater scene uses local timer of previous scene,
        --       so that BG bubbles sine positions transition continuously.
        core_anim_update_local_timer(_schedule, AnimNames.WATERFIELD, _localTimer, _timer)
        renderUnderwaterScene(_localTimer)
    elseif core_anim_is_running(_schedule, AnimNames.HIDDEN) then
        core_anim_update_local_timer(_schedule, AnimNames.HIDDEN, _localTimer, _timer)
        renderHiddenScene(_localTimer)
    end

    updateSyncVariables()
    core_timer_update_global_frame_counter()
end

-- <WAVES>
-- 000:0123456789abcdef0123456789abcdef
-- 001:001234566789abcd1234567889abcdef
-- 002:00122345567889ab345667899abccdef
-- 003:002233445566779955778899aabbccee
-- 004:00112233445566778899aabbccddeeff
-- 005:00112233445566778877665544332211
-- 006:489abcdeffedcba98765432100123456
-- 007:789abcdeeeedcba98765432111123456
-- 008:789abcddddddcba98765432222223456
-- 009:789abccccccccba98765433333333456
-- 010:789abbbbbbbbbba98765444444444456
-- 011:789aaaaaaaaaaaa98765555555555556
-- 012:78999999999999998766666666666666
-- 013:ffffffffffffffffffffffffffffffff
-- 015:eeeeeeeeeeeeeee000000000000eeeee
-- </WAVES>

-- <SFX>
-- 000:00001100220033004400540064009400c400e400f400f400f400f400f400f400f400f400f400f400f400f400f400f400f400f400f400f400f400f400102000000000
-- 001:04001300240033004400530064009300c400e400f400f400f400f400f400f400f400f400f400f400f400f400f400f400f400f400f400f400f400f400102000000000
-- 002:00001100220033004400540064009400c400e400f400f400f400f400f400f400f400f400f400f400f400f400f400f400f400f400f400f400f400f400102000000000
-- 003:04001300220031004200530064009400c400e400f400f400f400f400f400f400f400f400f400f400f400f400f400f400f400f400f400f400f400f400267000000000
-- 004:0ff006d006b016a0368056606650764086409630b63fc62cd619e608e608f600f600f600f600f600f600f600f600f600f600f600f600f600f600f600204000000000
-- 005:0ef02ee01ed0fed04ec0feb05ea0fe906e80fe708e60fe50ae40fe30ce20fe20de00fe00ee00fe00ee00fe00ee00fe00fe00fe00fe00fe00fe00fe00302000000000
-- 006:0df006d01df016a0368056606650764086409630b63fc62cd619e608f6f8f6f0fef0fef0fef0fef0fef0fef0fef0fef0fef0fef0fef0fef0fef0fef0209000000000
-- 007:b6c0770088c09900aac0ba00c9c0d800d2c0c300b400b400b200b200b400b400b400b400b400b400b400b400b400b400b400b400b400b400b400b400369026710200
-- 008:60c0710082c09300a4c0b400c3c0d200d2c0c300b400b400b200b200b400b400b400b400b400b400b400b400b400b400b400b400b400b400b400b400363026710200
-- 009:7000710082009300a400b400b300b200b200b300b400b400b200b200b400b400b400b400b400b400b400b400b400b400b400b400b400b400b400b40036b02601000a
-- 010:35f045d055a0757085409520b500c500f500f500f500f500f500f500f500f500f500f500f500f500f500f500f500f500f500f500f500f500f500f500402000000000
-- 011:950045210532356355a585e6b5f7e517f510f500f500f500f500f500f500f500f500f500f500f500f500f500f500f500f500f500f500f500f500f500404000090600
-- 012:000001000200030004000400030002000100000000000000000000000000000000000000000000000000000000000000000000000000000000000000152009000000
-- 013:000031007200a300f400f400f300f200f100f000f000f000f000f000f000f000f000f000f000f000f000f000f000f000f000f000f000f000f000f00015200c040000
-- 014:e000d100c200b300a4009400830072006100500040003000200010000000000000000000000000000000000000000000000000000000000000000000350009000000
-- 015:af00ff00ff00ff00ff00ff00ff00ff00ff00ff00ff00ff00ff00ff00ff00ff00ff00ff00ff00ff00ff00ff00ff00ff00ff00ff00ff00ff00ff00ff0022a000020000
-- 016:90207040707071c07200730074007400740003000200010002000200010001000000000000000000000000000000000000000000000000000000000035200c010405
-- 017:90207030707071c07200730074007400740003000200010002000200010001000000000000000000000000000000000000000000000000000000000025200c010405
-- 018:90107050707071a07200730074007400740003000200010002000200010001000000000000000000000000000000000000000000000000000000000035200c010405
-- 019:90007030708071a07200730074007400740003000200010002000200010001000000000000000000000000000000000000000000000000000000000035200c010405
-- 020:e020d040c070b1c0a2009300840074006400530042003100220012000100110020003000400050006000700080009000a000b000c000d000e000f00036500c000405
-- 025:0e000e000e000e000e000e000e000e000e000e000e000e000e000e000e000e000e000e000e000e000e000e000e000e000e000e000e000e000e000e0040b000000000
-- 027:7f00ef000f000f000f000f000f000f000f000f000f000f000f000f000f000f000f000f000f000f000f000f000f000f000f000f000f000f000f000f00363000020000
-- 028:def0ede0cedfeecebebeeeafae90ee819e72ee618e50ee4f7e30ee206e10ee006e00ee000e00ee000e00ee000e00ee000e00ee000e00ee000e00ee00301002f2000b
-- 029:0ef71ed61ed52eb53eb43ea44ea34e935e825e816e716e717e607e508e5f8e4f9e4e9e4eae3ebe2dbe2dbe2dce1dce1cde0cde0cde0bee0aee0aee09302000000000
-- 030:bef0e6e08ed0eecf6ebdeeab6e99ee8a6e7cee6d6e5eee4f6e30ee206e10ee006e00ee000e00ee000e00ee000e00ee000e00ee000e00ee000e00ee00275002f2000b
-- 031:e610d690c659beffae5d9ea28e277e8a7ed77e6d7e1e7eaf7e507e207ed07e007e007e007e007e007e007e007e007e007e007e007e007e007e007e003d2002f20f0b
-- 032:06c00700080009000a000b000c000b000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000362062000000
-- 036:7df076d07dc086a09d80a660bd50c640dd30d620dd1fe60ced09e608fd08f600ee00ee00ee00ee00ee00ee00ee00ee00ee00ee00ee00ee00ee00ee00219002b80000
-- 040:a6008700680059006a008b00ac00cb00e000e000e000e000e000e000e000e000e000e000e000e000e000e000e000e000e000e000e000e000e000e000504062000000
-- 048:00001100220033004400540064009400c400e400f400f400f400f400f400f400f400f400f400f400f400f400f400f400f400f400f400f400f400f400379000000000
-- 049:0d000d000d000d000d000d000d000d000d000d000d000d000d000d000d000d000d000d000d000d000d000d000d000d000d000d000d000d000d000d00307000000000
-- 063:060006000600060006000600060006000600060006000600060006000600060006000600060006000600060006000600060006000600060006000600307000000000
-- </SFX>

-- <PATTERNS>
-- 000:d00042600012600022600002d00042600012600022600004d00042600012600022600002d00042600012600022600004d00042600004600022600002d00042600012600022600004d00042600012600022600002d00042600012600022600002d00042900012900022900002d00042900012900022900004d00042900012900022900002d00042900012900022900004d00042900004900022900002d00042900012900022900004d00042900012900022900002d00042900012900022900002
-- 001:6ff1170ff0000881000000000000000000000ff1000ff0000881000000000000000000000ff1000000000881000000006ff1190000000ee1000000000dd1000000000cc1000000000bb1000000000aa1000000000991000000000881000000006ff1270ff0000881000000000000000000000ff1000ff0000881000000000000000000000ff1000000000881000000006ff1290000000ee1000000000dd1000000000cc1000000000bb1000000000aa100000000099100000000088100000000
-- 002:6ff1370ff0000881000000000000000000000ff1000ff0000881000000000000000000000ff1000000000881000000006ff1390000000ee1000000000dd1000000000cc1000000000bb1000000000aa1000000000991000000000881000000006ff1070ff0000881000000000000000000000ff1000ff0000881000000000000000000000ff1000000000881000000006ff1090000000ee1000000000dd1000000000cc1000000000bb1000000000aa100000000099100000000088100000000
-- 003:d00042e00012e00022e00002d00042e00012e00022e00004d00042e00012e00022e00002d00042e00012e00022e00004d00042e00004e00022e00002d00042e00012e00022e00004d00042e00012e00022e00002d00042e00012e00022e00002d00042600012600022600002d00042600012600022600004d00042600012600022600002d00042600012600022600004d00042600004600022600002d00042600012600022600004d00042600012600022600002d00042600012600022600002
-- 004:0000006ff1fa00210000f10068815a0a01006ff1fa00210000f10002010000000000000068815a0a01006ff1fa0021000000006ff1fa00210000f10068815a0a01006ff1fa00210000f10002010000000000000068815a0a01006ff1fa0021000000004ff1fa00210000f10068815a0a01004ff1fa00210000f10002010000000000000068815a0a01004ff1fa0021000000004ff1fa00210000f10068815a0a01004ff1fa00210000f10002010000000000000068815a0a01004ff1fa002100
-- 005:0000006ff1fa00210000f10068815a0a01006ff1fa00210000f10002010000000000000068815a0a01006ff1fa0021000000006ff1fa00210000f10068815a0a01006ff1fa00210000f10002010000000000000068815a0a01006ff1fa0021000000006ff1fa00210000f10068815a0a01006ff1fa00210000f10002010000000000000068815a0a01006ff1fa0021000000006ff1fa00210000f10068815a0a01006ff1fa00210000f10002010000000000000068815a0a01006ff1fa002100
-- 006:000000680117000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000680127000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
-- 007:6231c2000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000021100000000000000000000011100000000000000000000
-- 008:d00042600012600022600002d00042600012600022600004d00042600012600022600002d00042600012600022600004d00042600004600022600002d00042600012600022600004d00042600012600022600002d00042600012600022600002d00042600012600022600002d00042600012600022600004d00042600012600022600002d00042600012600022600004d00042600004600022600002d00042600012600022600004d00042600012600022600002d00042600012600022600002
-- 009:0000006ff1fa00210000f1000201000000006ff1fa00210000f10002010000000000000068815a0a01006ff1fa0021000000006ff1fa00210000f1000201000000006ff1fa00210000f10002010000000000000068815a0a01006ff1fa0021000000006ff1fa00210000f1000201000000006ff1fa00210000f10002010000000000000068815a0a01006ff1fa0021000000006ff1fa00210000f1000201000000006ff1fa00210000f10002010000000000000068815a0a01006ff1fa002100
-- 010:0000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000006ff1fc5404f0000000000000000000000000000000000000
-- 011:d00042600012600022600002d00042600012600022600004d00042600012600022600002d00042600012600022600004d00042600004600022600002d00042600012600022600004d00042600012600022600002d00042600012600022600002d00042600012600022600002d00042600012600022600004d00042600012600022600002d00042600012600022600004d00042600004600022600002d00042600012600022600004d000420000000000000000006aa146604146f4c1449c4144
-- 012:6321c40000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000004006881f26784fa0aa1000bb1000cc1000dd1000ee1000ff100000000000000000000000000000000000000000000000000
-- 013:0000006ff1fa00210000f1000201000000006ff1fa00210000f10002010000000000000068815a0a01006ff1fa0021000000006ff1fa00210000f1000201000000006ff1fa00210000f10002010000000000000068815a0a01006ff1fa0021000000006ff1fa00210000f1000201000000006ff1fa00210000f10002010000000000000068815a0a01006ff1fa0021000000006ff1fa00210000f1000201000000006ff1fa00210000f100020100000100000000000000000000000000000000
-- 014:6231e2000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
-- 015:0000006801370000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000006801070000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000006ff1fc5404f0000000000000000000000000000000000000
-- 016:00000068011700000001f6006fa1980000005000986084980004000000008000980000000000000441009fa19804f60000000000000061f698000000b00098d1049800040004f60000000000000000000000000008610006410004210001f600dfa19800000010000000000040009a000000b00098d04498100000000400b0009800000000000000000091f69800000002f60000000003f60000000004f6000000000e91000c81000a7100086100064100043100032100021100011100000000
-- 017:efa19801f600043100000000dfa1980000009000980000000441000000006fa198000000000000000000d00096e0849600040001f60001f600000000000000000000086100000000000000000000042100000000efa196000000043100000000dfa1960000000431000000006fa198000000a04498000000100000000400b00098000000000000000000a1f69800000002f60000000003f60000000004f6000000000e91000c81000a7100086100064100043100032100021100011100000000
-- 018:00000068011700000001f6006fa1980000005000986084980004000000008000980000000000000441009fa19804f60000000000000041f69a000000b00098d1049800040004f60000000000000000000000000008610006410004210001f600efa19800000010000000000040009a000000b00098d04498100000000400b00098000000000000000000d1f69800000002f60000000003f60000000004f6000000000e91000c81000a71000751000531000321000211001ff10061f698900098
-- 019:efa19801f600043100000000dfa1980000009000980000000431000000006fa198000000000000000000d00096e0849600040001f60001f600000000000000000000086100000000000000000000043100000000efa196000000043100000000dfa196000000043100000000afa198000000a04498000000000000010400b10098000000000000020400d1f69800000002f60000000003f60000000004f6000000000e91000c81000a7100086100064100043100032100021100011100000000
-- 020:6a4109020000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000800039000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
-- 021:d00042600012600022600002d00042600012600022600004d00042600012600022600002d00042600012600022600004d00042600004600022600002d00042600012600022600004d00042600012600022600002d00042600012600022600002d00042600012600022600002d00042600012600022600004d00042600012600022600002d00042600012600022600004d00042600004600022600002d00042600012600022600004d00042600012600022600002d00042600012600022600002
-- 022:6ff1866ff19800010000f10068818a0a01006ff1fa00010000f1000001006f018600f10068815a0a01006ff1fa0001000000006ff18660019800f10068818a0a01006ff1fa00010000f1000001006f018600f10068815a0a01006ff1fa0001006ff1866ff19800010000f10068818a0a01006ff1fa00010000f1000001006f018600f10068815a0a01006ff1fa0001000000006ff18660019800f10068818a0a01006ff1fa00010000f1000001006f018600f10068815a0a01006ff1fa000100
-- 023:6f119601f1000ff1000f110001f1000ff1000f110001f1000181006181090000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000006f119601f1000ff1000f110001f1000ff1000f110001f100018100818139000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
-- 024:6321e4000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
-- 025:6000d2000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
-- 026:6f01070f11000f21000f31000f41000f51000f61000f71000f81000f91000fa1000fb1000fc1000fd1000fe1000ff1008ff1370ef1000df1000cf1000bf1000af10009f10008f10007f10006f10005f10004f10003f10002f10001f10000f100b0f10701f10002f10003f10004f10005f10006f10007f10008f10009f1000af1000bf1000cf1000df1000ef1000ff1006ff1090fe1000fd1000fc1000fb1000fa1000f91000f81000f71000f61000f51000f41000f31000f21000f11000f0100
-- 027:6000426000426000646181d80000006ff1fa6ff1646000f860000b60004260006468810b0000006ff1fa6000640000006000426000426000646181d80000006ff1fa6ff1646000f880003b60004260006488813b0000006ff1fa6000640000006000426000426000646181d80000006ff1fa6ff1646000f8b0000b600042600064b8810b0000006ff1fa6000640000006000426000426000646181d80000006ff1fa6ff1646000f860000b60004260006468810b0000006ff1fa600064000000
-- 028:6ff18800000062a1888ff18800000082a188aff188000000a2a188dff188000000d2a1886ff18a000000dff18862a18a4ff18a00000042a18afff188000000f2a188dff188000000d2a188000000d161880000000000000000000000000000006ff18800000062a1888ff18800000082a188aff188000000a2a188dff188000000d2a188aff1880000008ff188a2a188aff188000000a2a188000000a16188000000000000000000000000000000000000000000000000a60188000000000000
-- 029:9000d2000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
-- 030:9f01070f11000f21000f31000f41000f51000f61000f71000f81000f91000fa1000fb1000fc1000fd1000fe1000ff100bff1370ef1000df1000cf1000bf1000af10009f10008f10007f10006f10005f10004f10003f10002f10001f10000f100e0f10701f10002f10003f10004f10005f10006f10007f10008f10009f1000af1000bf1000cf1000df1000ef1000ff1009ff1090fe1000fd1000fc1000fb1000fa1000f91000f81000f71000f61000f51000f41000f31000f21000f11000f0100
-- 031:6000426000426000649181d80000009ff1fa6ff1649000f89000096000426000649881090000009ff1fa6000640000006000426000426000649181d80000009ff1fa6ff1649000f8b00039600042600064b881390000009ff1fa6000640000006000426000426000649181d80000009ff1fa6ff1649000f8e00009600042600064e881090000009ff1fa6000640000006000426000426000649181d80000009ff1fa6ff1649000f890000b60004260006498810b0000009ff1fa600064000000
-- 032:9ff18800000092a188bff188000000b2a188dff188000000d2a1884ff18a00000042a18a9ff18a0000004ff18a92a18a7ff18a00000072a18a6ff18a00000062a18a4ff18a00000042a18a00000041618a0000000000000000000000000000009ff18800000092a188bff188000000b2a188dff188000000d2a1884ff18a00000042a18adff188000000bff188d2a188dff188000000d2a188000000d16188000000000000000000000000000000000000000000000000000000000000000000
-- 033:d00042600012600022600002d00042600012600022600004d00042600012600022600002d00042600012600022600004d00042600004600022600002d00042600012600022600004d00042600012600022600002d00042600012600022600002d00042600012600022600002d00042600012600022600004d00042600012600022600002d00042600012600022600004d00042000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
-- 034:6ff1866ff19800010000f10068818a0a01006ff1fa00010000f1000001006f018600f10068815a0a01006ff1fa0001000000006ff18660019800f10068818a0a01006ff1fa00010000f1000001006f018600f10068815a0a01006ff1fa0001006ff1866ff19800010000f10068818a0a01006ff1fa00010000f1000001006f018600f10068815a0a01006ff1fa0001000000006ff18660019800f10068818a0a01006ff1fa00010000f100000100000000000000000000000000000000000000
-- 035:6000d20000000000000000000000000000000000000000000000000000000000000000000000000000000000000000004000d2000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000e000d20000000000000000000000000000000000000000000000000000000000000000000000000000000000000000006000d20000000000000000000000000000000000000000000000000000000000000000006ff1d2088100044100022100
-- 036:6f01070f11000f21000f31000f41000f51000f61000f71000f81000f91000fa1000fb1000fc1000fd1000fe1000ff100aff1370ef1000df1000cf1000bf1000af10009f10008f10007f10006f10005f10004f10003f10002f10001f10000f100e0f10701f10002f10003f10004f10005f10006f10007f10008f10009f1000af1000bf1000cf1000df1000ef1000ff100aff1390fe1000fd1000fc1000fb1000fa1000f91000f81000f71000f61000f51000f41000f31000c21000a1100080100
-- 037:6004426000426000646181d80000006ff1fa6ff1646000f86000096000426000646881090000006ff1fa7000640000006000426000426000648181d80000008ff1fa6ff1648000f8a00039600042600064a881390000008ff1fa6000640000006000426000426000646181d80000006ff1fa6ff1646000f8e00009600042600064e881090000006ff1fa6000640000006000426000426000646181d80000006ff1fa6ff1646000f86000096000426000646881096ff142000000000000000000
-- 038:00000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000009040090019b811197012100013100014100014100014100014100014100
-- 039:6a410904a1000a410004a1000a410004a1000a410004a1000a410004a1000a410004a1000a410004a1000a410004a1000a410004a1000a410004a1000a410004a1000a410004a1000a410004a1000a410004a1000a410004a1000a410004a1000841000471000641000451000441000431000241000411000041000401000041000401000041000401000041000411000241000431000441000451000641000471000841000491000a410004a1000a310004a1000a210004a1000a110003a100
-- 040:6ff1866ff19800010000f10068818a0a01006ff1fa00010000f1000001006f018600f10000000000000000000000000068848400f00001f00002f00003f00004f00005f00006f00007f00008f00009f0000af0000bf0000cf0000df0000ef00080f1f701f10002f10003f10004f10005f10006f10007f10008f10009f1000af1000bf1000cf1000df1000ef1000fe1000fd1000fc1000fb1000fa1000f91000f81000f71000f6100022100000000000000000000000000000000000000000000
-- 041:0000006ff1fa64011760f1fa68815a0a01006ff1fa00210000f10064011700000000000068815a0a01006ff1fa6801fa0021006ff1fa64011960f1fa68815a0a01006ff1fa00210000f10064011900000000000068815a0a01006ff1fa0021000000004ff1fa64012740f1fa68815a0a01004ff1fa00210000f10064012700000000000068815a0a01004ff1fa0021000000004ff1fa64012940f1fa68815a0a01004ff1fa00210000f10064012900000000000068815a0a01004ff1fa002100
-- 042:0000006ff1fa64013760f1fa68815a0a01006ff1fa00210000f10064013700000000000068815a0a01006ff1fa6801fa0021006ff1fa64013960f1fa68815a0a01006ff1fa00210000f10064013900000000000068815a0a01006ff1fa0021000000006ff1fa64010760f1fa68815a0a01006ff1fa00210000f10064010700000000000068815a0a01006ff1fa0021000000006ff1fa64010960f1fa68815a0a01006ff1fa00210000f10064010900000000000068815a0a01006ff1fa002100
-- 043:6a4109000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000800039000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000094100084100074100064100054100044100034100024100
-- 044:6f119601f1000ff1000f110001f1000ff1000f110001f1000181006181090000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000001008c049181019d020100030100040100040100040100040100040100000000000000041100041100042100042100043100043100044100034100024100014100004100003100003100001100000000000000000000000000000000000000000000000100000000000000000000
-- 045:6481996604930000000000000000000000000000000000000000000000000241000131000121000111000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000f0400400091000000000000000000000000000000000000000000011100000000000000000000000000000000000000000000
-- 046:6001a209f10060a4ac6001a20f91006074ac0000000000000000000000000000006001a209f10060a4ac6001a20f91006074ac0000006001a20ff1006084ac6001a208910060a4ac6001a20441006084ac6001a202210060a4ac6001a2000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000040400da4902000000000000000000000000000000000000000000000000000000
-- 047:6001a20ff10060a4ac6001a20ff1006074bab001b8bf01b800f1008f01b80601000401006001a204810060a4ac6001a20841006074ac0000006001a20841006084ac6001a208410060a4ac6001a2b081b6080100004100000100000000000100000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
-- 048:6ff1c20ee1000dd1000cc1000bb1000aa1000991000881000771000661000551000441000331006001930011000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000f04000000000000000000000f0400400091000000000000000000000000000000000000000000000000000100000000000000000000000000000000000000000000
-- 049:0000000000000000000230000000006011e20000000121000231000000000000000000000000000000000000000000000120000000009231e2000000000000000000000000000000000000000000012000000000e231e20000000000000000000000000000000000000000000120000000006231e2000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
-- 050:0000000000000000000010006101d50101000000000000006881e50661000441000221000111000001000000000000000000000660009881e5066100044100022100011100000100000000000000000000000000d881e50661000441000221000111000001000000000000000000000e04006881e10771000661000551000441000331000221000111000101000011000101000011000101000011000100000c04000c04006341c9078100087100067100065100045100043100023100021100
-- 051:0000000000006021d70011000000000001006001ff0804006081fd0701000061000501000041000301000021000101000011006001fd6081fb0701000061000501000041000301000021000101000011006001fb6081f90701000061000501000041000301000021000101000011006001fd6081fb070100006100050100004100030100002100010100001100010100001100010100001100010100001100010100001100010100000100621147000000000000000000632147000000000000
-- 052:0000000000000000000000000000000000000000000000006081c70061000041000021000011000001000000000000000000000000006081c70061000041000021000011000001000000000000000000000000006081c70061000041000021000011000001000000000000000000000000006081c70061000041000021000011000001006111b7022100023100024100025100026100027100028100028100026100024100022100623147000000000000000000645149000000000000000000
-- 053:efa19801f600043100000000dfa1980000009000980000000431000000006fa198000000000000000000efa19801f600043100000000dfa1980000000431000000009fa1980000006fa198000000043100000000efa1960000000431000204004fa19861f69800000000000000000000000000000000000000000000000000000000000000000000000000000000000002f60000000003f60000000004f6000000000e91000c81000a7100086100064100043100032100021100011100000000
-- </PATTERNS>

-- <PATTERNS1>
-- 000:dff902000000da4902000000dff9440a4100dff902000000000000000000da4902000000dff9440a4100d4a904000000dff902000000dff902000000dff9440a4100dff902000000000000000000da4902000000dff9440a410004a1000a4100dff902000000da4902000000dff9440a4100dff902000000000000000000da4902000000dff9440a4100d4a904000000dff902000000dff902000000dff9440a4100dff902000000000000000000da4902000000dff944da4902d4a904da4906
-- 001:dff902000000da4902000000d00804000000000000000000000000000000000000000000012100000000000000000000000000000000da4902000000da4904000000000000000000000000000000000000000000012100000000000000000000dff902000000da4902000000d00804000000000000000000000000000000000000000000012100000000000000000000000000000000da4902000000d00804000000000000000000000000000000000000000000012100000000000000000000
-- 002:9ff9020000009a4902000000dff9440a41009ff9040000000000000000009a4904000000dff9440a410094a9040000009ff9020000009ff902000000dff9440a41009ff9040000000000000000009a4904000000dff9440a410004a1000a41006ff9020000006a4902000000dff9440a41006ff9040000000000000000006a4904000000dff9440a410064a9040000008ff9020000008ff902000000dff9440a41008ff9040000000000000000008a4904000000dff9448a490284a9048a4906
-- 003:9ff9020000009a49020000009008040000000000000000000000000000000000000000000121000000000000000000000000000000009a49020000009a49040000000000000000000000000000000000000000000121000000000000000000006a49020000006a49020000006008040000000000000000000000000000000000000000000121000000000000000000000000000000008a4902000000800804000000000000000000000000000000000000000000012100000000000000000000
-- 004:dff902000000da4902000000da49040000000941000000000841000040000741000000000621000000000521000000000421000000000221000000000211000100000211000000006271b80681006f81b608f1000f8100dc41b80641009441b8022100000100000100000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
-- 008:0000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000006721b608410068f1b80f810008f100d4c1b60461009241b6000100000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
-- 009:54c98a60088a54a88ad4a88884a88864a88854a888d0088650088a60088a50088ad00888800888600888500888d0088650088a60088a50088ad00888800888600888500888d0088650088a60088a50088ad00888800888600888500888d00886f0088840088af00888b00888600888400888f00886b00886f0088840088af00888b00888600888400888f00886b00886f0088840088af00888b00888600888400888f00886b00886f0088840088af00888b00888600888400888f00886b00886
-- 010:00000058298a60088a54a88ad4a88884a88864a88854a888d0088650088a60088a50088ad00888800888600888500888d0088650088a60088a50088ad00888800888600888500888d0088650088a60088a50088ad00888800888600888500888d00886f0088840088af00888b00888600888400888f00886b00886f0088840088af00888b00888600888400888f00886b00886f0088840088af00888b00888600888400888f00886b00886f0088840088af00888b00888600888400888f00886
-- 011:00000000000000000052598a60088a54a88ad4a88884a88864a88854a888d0088650088a60088a50088ad00888800888600888500888d0088650088a60088a50088ad00888800888600888500888d0088650088a60088a50088ad00888800888600888500888d00886f0088840088af00888b00888600888400888f00886b00886f0088840088af00888b00888600888400888f00886b00886f0088840088af00888b00888600888400888f00886b00886f0088840088af00888b00888600888
-- 012:b8298650088a60088a54a88ad4a88884a88864a88854a888d0088650088a60088a50088ad00888800888600888500888d0088650088a60088a50088ad00888800888600888500888d0088650088a60088a50088ad00888800888600888500888d00886f0088840088af00888b00888600888400888f00886b00886f0088840088af00888b00888600888400888f00886b00886f0088840088af00888b00888600888400888f00886b00886f0088840088af00888b00888600888400888f00886
-- 013:425988f00886b0088652598a60088a54a88ad4a88884a88864a88854a888d0088650088a60088a50088ad00888800888600888500888d0088650088a60088a50088ad00888800888600888500888d0088650088a60088a50088ad00888800888600888500888d00886f0088840088af00888b00888600888400888f00886b00886f0088840088af00888b00888600888400888f00886b00886f0088840088af00888b00888600888400888f00886b00886f0088840088af00888b00888600888
-- 014:f4c98840088af00888b00888600888400888f00886b00886f0088840088af00888b00888600888400888f00886b00886d00888f00888d00888900888400888f00886d00886900886d00888f00888d00888900888400888f00886d00886900886d00888f00888d00888900888400888f00886d00886900886d00888f00888d00888900888400888f00886d00886900886f0088840088af00888c00888800888600888f00886c00886f0088840088af00888c00888800888600888f00886c00886
-- 015:000000f8298840088af00888b00888600888400888f00886b00886f0088840088af00888b00888600888400888f00886b00886d00888f00888d00888900888400888f00886d00886900886d00888f00888d00888900888400888f00886d00886900886d00888f00888d00888900888400888f00886d00886900886d00888f00888d00888900888400888f00886d00886900886f0088840088af00888c00888800888600888f00886c00886f0088840088af00888c00888800888600888f00886
-- 016:54c98a64c98a54c98ad4c98884c98864c98854b988d4b98654b98a64b98a54b98ad4b98883a98863a98853a988d3a98653a98a63a98a53998ad39988839988639988539988d3998652898a62898a52898ad28988828988628988527988d2798652798a62798a52798ad27988826988626988526988d2698652698a62698a52598ad25988825988625988525988d2598652498a62498a52498ad24988824988624988523988d2398652398a62398a52298ad22988822988622988522988d22986
-- 017:00000058298a68298a58298ad82988882988682988582988d8298657298a67298a57298ad72988872988672988572988d7298657298a66298a56298ad62988862988662988562988d6298656298a66298a55298ad52988852988652988552988d5298655298a65298a54298ad42988842988642988542988d4298654298a64298a54298ad32988832988632988532988d3298653298a63298a53298ad22988822988622988522988d2298652298a62298a52298ad22988822988611988511988
-- 018:000000f8298840088af00888b00888600888400888f00886b00886f0088840088af00888b00888600888400888f00886b00886d00888f00888d00888900888400888f00886d00886900886d00888f00888d00888900888400888f00886d00886900886d00888f00888d008886271b80681006f81b608f1000f8100dc41b80641009441b8982988400888f00886d00886900886f0088840088af00888c00888800888600888f00886c00886f0088840088af00888c00888800888600888f00886
-- 019:5ff178000000000000003400508178000000dff176000000000000000000d08176000000000000000000d401760000000000000000008ff1760000005000780000006000780000008000780000000000006ff1780000000000005ff1780000006000780000000000000000005ff178000000fff176000000000000000000f08176000000000000000000f40176000000000000000000f02176000000000000000000000000000000000000000000000000000000000000000000d88176fcc176
-- 020:5ff1780000000000000034005081780000008ff178000000000000000000808178000000000000000000840178000000000000000000dff178000000b000780000008000780000008000780000000000006ff1780000000000005ff1780000006000780000000000000000005ff178000000fff176000000000000000000f08176000000000000000000f40176000000000000000000f02176000000000000000000000000000000000000000000000000000000000000000000000000000000
-- 021:f00076000000000000003400f081760000006ff178000000000000000000608178000000fff176000000d00076000000000000000000d08176000000000000000000d40176000000000000000000d02176000000000000000000000000000000dff176000000000000000000d081760000004ff178000000000000000000408178000000dff176000000f00076000000000000000000f08176000000000000000000f40176000000000000000000f02176000000000000000000000000000000
-- 022:5ff1780000000000000034005081780000008ff178000000000000000000808178000000000000000000840178000000000000000000dff178000000b000780000008000780000008000780000000000006ff1780000000000005ff1780000006000780000000000000000008ff17800c400aff178000000000000000000a08178000000000000000000a40178000000000000000000a02178000000000000000000b441b66104b66cf1b80fc1006f81b60ff1000f8100dc41b80841009441b8
-- 025:6000d2000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
-- 026:6f01070f11000f21000f31000f41000f51000f61000f71000f81000f91000fa1000fb1000fc1000fd1000fe1000ff1008ff1370ef1000df1000cf1000bf1000af10009f10008f10007f10006f10005f10004f10003f10002f10001f10000f100b0f10701f10002f10003f10004f10005f10006f10007f10008f10009f1000af1000bf1000cf1000df1000ef1000ff1006ff1090fe1000fd1000fc1000fb1000fa1000f91000f81000f71000f61000f51000f41000f31000f21000f11000f0100
-- 027:6000426000426000646181d80000006ff1fa6ff1646000f860000b60004260006468810b0000006ff1fa6000640000006000426000426000646181d80000006ff1fa6ff1646000f880003b60004260006488813b0000006ff1fa6000640000006000426000426000646181d80000006ff1fa6ff1646000f8b0000b600042600064b8810b0000006ff1fa6000640000006000426000426000646181d80000006ff1fa6ff1646000f860000b60004260006468810b0000006ff1fa600064000000
-- 028:6ff18800000062a1888ff18800000082a188aff188000000a2a188dff188000000d2a1886ff18a000000dff18862a18a4ff18a00000042a18afff188000000f2a188dff188000000d2a188000000d161880000000000000000000000000000006ff18800000062a1888ff18800000082a188aff188000000a2a188dff188000000d2a188aff1880000008ff188a2a188aff188000000a2a188000000a16188000000000000000000000000000000000000000000000000a60188000000000000
-- 029:9000d2000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
-- 030:9f01070f11000f21000f31000f41000f51000f61000f71000f81000f91000fa1000fb1000fc1000fd1000fe1000ff100bff1370ef1000df1000cf1000bf1000af10009f10008f10007f10006f10005f10004f10003f10002f10001f10000f100e0f10701f10002f10003f10004f10005f10006f10007f10008f10009f1000af1000bf1000cf1000df1000ef1000ff1009ff1090fe1000fd1000fc1000fb1000fa1000f91000f81000f71000f61000f51000f41000f31000f21000f11000f0100
-- 031:6000426000426000649181d80000009ff1fa6ff1649000f89000096000426000649881090000009ff1fa6000640000006000426000426000649181d80000009ff1fa6ff1649000f8b00039600042600064b881390000009ff1fa6000640000006000426000426000649181d80000009ff1fa6ff1649000f8e00009600042600064e881090000009ff1fa6000640000006000426000426000649181d80000009ff1fa6ff1649000f890000b60004260006498810b0000009ff1fa600064000000
-- 032:9ff18800000092a188bff188000000b2a188dff188000000d2a1884ff18a00000042a18a9ff18a0000004ff18a92a18a7ff18a00000072a18a6ff18a00000062a18a4ff18a00000042a18a00000041618a0000000000000000000000000000009ff18800000092a188bff188000000b2a188dff188000000d2a1884ff18a00000042a18adff188000000bff188d2a188dff188000000d2a188000000d16188000000000000000000000000000000000000000000000000000000000000000000
-- 034:6ff18800000062a1888ff18800000082a188aff188000000a2a188dff188000000d2a1886ff18a000000dff18862a18a4ff18a00000042a18afff188000000f2a188dff188000000d2a188000000d161880000000000000000000000000000006ff18800000062a1888ff18800000082a1889ff18800000092a188eff188000000e2a1889ff1880000008ff18892a188aff188000000a2a188000000a16188000000000000000000000000000000000000000000000000000000000000000000
-- 035:6000d20000000000000000000000000000000000000000000000000000000000000000000000000000000000000000004000d2000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000e000d20000000000000000000000000000000000000000000000000000000000000000000000000000000000000000006000d2000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
-- 036:6f01070f11000f21000f31000f41000f51000f61000f71000f81000f91000fa1000fb1000fc1000fd1000fe1000ff100aff1370ef1000df1000cf1000bf1000af10009f10008f10007f10006f10005f10004f10003f10002f10001f10000f100e0f10701f10002f10003f10004f10005f10006f10007f10008f10009f1000af1000bf1000cf1000df1000ef1000ff100aff1390fe1000fd1000fc1000fb1000fa1000f91000f81000f71000f61000f51000f41000f31000f21000f11000f0100
-- 037:6004426000426000646181d80000006ff1fa6ff1646000f86000096000426000646881090000006ff1fa7000640000006000426000426000648181d80000008ff1fa6ff1648000f8a00039600042600064a881390000008ff1fa6000640000006000426000426000646181d80000006ff1fa6ff1646000f8e00009600042600064e881090000006ff1fa6000640000006000426000426000646181d80000006ff1fa6ff1646000f86000096000426000646881090000006ff1fa6ff1646000f8
-- </PATTERNS1>

-- <TRACKS>
-- 000:900000900ac21805c14c06046d3a46c02e431805c14c0604180a644c0ba4180ae44c0b2565571622b326000000000000000020
-- 001:2fc47d000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000021
-- 002:ad6c57ed70684696e90000000000000000000000000000000000000000000000000000000000000000000000000000000000a0
-- 003:65575b65532622a963180a644c0ba4180ae44c0badeeb07c000000000000000000000000000000000000000000000000000020
-- </TRACKS>

-- <TRACKS1>
-- 000:282b03282d83182d05182d453c3095282d05282dc54c3395544252000000000000000000000000000000000000000000000040
-- 001:ad6c10ad6c10ed7020ed7020ad6c57ad6c57ed7068ed70684696200000000000000000000000000000000000000000000000a0
-- </TRACKS1>

-- <PALETTE>
-- 000:1a1c2c5d275db13e53ef7d57ffcd75a7f07038b76425717929366f3b5dc941a6f673eff7f4f4f494b0c2566c86333c57
-- </PALETTE>
