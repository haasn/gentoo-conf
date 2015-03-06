require 'mp.options'
local opt = {
    maxiters = 3,
    maxdelta = 0.01,
    mindelta = 0.0005,
    displayfps = 60
}
read_options(opt)

function get_scale(ratio, factor)
    scale = ratio * factor / math.floor(ratio * factor + 0.5)
    delta = math.abs(scale - 1)

    if (delta < opt.mindelta) then
        return 1   -- close enough, just use it
    end
    if (delta > opt.maxdelta) then
        return nil -- large deviation, skip
    end
    return scale   -- decent match found
end

function adjust_speed(event)
    fps = mp.get_property_number("fps")

    if not fps then
        return
    end

    display_fps = opt.displayfps
    ratio = display_fps / fps

    for i=1,opt.maxiters do
        scale = get_scale(ratio, i)
        if scale then
            break
        end
    end

    if scale then
        mp.set_property("speed", scale)
    end
end

mp.register_event("playback-restart", adjust_speed)
