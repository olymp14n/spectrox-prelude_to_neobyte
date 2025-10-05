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
---                  	row: Number of the row to play. -1 for first row.
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

