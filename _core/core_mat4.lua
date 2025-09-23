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
