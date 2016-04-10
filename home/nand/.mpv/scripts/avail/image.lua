function zoom_invariant_add(prop, amt)
    amt = amt / 2 ^ mp.get_property_number("video-zoom")
    mp.set_property_number(prop, mp.get_property_number(prop) + amt)
end

mp.add_key_binding(nil, "zoom-invariant-add", zoom_invariant_add)
