vec4 sample(sampler2D tex, vec2 pos, vec2 size)
{
    return texture(tex, pos).grba;
}
