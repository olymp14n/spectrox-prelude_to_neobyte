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
