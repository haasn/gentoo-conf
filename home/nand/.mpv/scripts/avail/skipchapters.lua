require 'mp.options'
local opt = {
    patterns = {"^OP$","^[Oo]pening$"
               ,"^ED$","^[Ee]nding$"
               ,"^[Pp]review$"}
}
read_options(opt)

function check_chapter(_, chapter)
    if not chapter then
        return
    end
    local title = mp.get_property("chapter-list/"..chapter.."/title")
    for _, p in pairs(opt.patterns) do
        if string.match(title, p) then
            print("Skipping chapter:", title)
            mp.command("no-osd add chapter 1")
            return
        end
    end
end

mp.observe_property("chapter", "string", check_chapter)
