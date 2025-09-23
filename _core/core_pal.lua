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
