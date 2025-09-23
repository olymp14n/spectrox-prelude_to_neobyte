-- title:   core_lut
-- author:  Olympian / Spectrox
-- desc:    n/a
-- site:    https://github.com/olymp14n
-- license: CC0 1.0 Universal
-- version: 1.0
-- script:  lua

-------------------------------------------------------------------------------
--- LUT
-------------------------------------------------------------------------------

local SIN_LUT_SIZE = 4096

local _sinScale = SIN_LUT_SIZE / (2 * math.pi)
local _sinMask = SIN_LUT_SIZE - 1
local _sinTab = {}
local _cosTab = {}

function core_lut_init()
	for i = 0, SIN_LUT_SIZE - 1 do
		_sinTab[i] = math.sin(i * 2 * math.pi / SIN_LUT_SIZE)
		_cosTab[i] = math.cos(i * 2 * math.pi / SIN_LUT_SIZE)
	end
end

function core_lut_sin(x)
	return _sinTab[((x * _sinScale) // 1) & _sinMask]
end
