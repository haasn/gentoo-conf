vec4 sample(sampler2D tex, vec2 pos, vec2 tex_size)
{
    float luma = dot(texture(tex, pos).rgb, vec3(0.2126, 0.7152, 0.0722));
    return vec4(luma, luma, luma, 1.0);
}
