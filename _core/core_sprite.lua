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
