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
