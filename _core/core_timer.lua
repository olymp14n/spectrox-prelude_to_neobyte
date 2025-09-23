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
