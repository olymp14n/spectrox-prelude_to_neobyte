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
