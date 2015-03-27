// vim: set ft=glsl:

// roughly corresponds to fk3db parameters, which this algorithm is
// loosely inspired by
#define threshold 64
#define range     15
#define grain     16

float rand(vec2 co){
    return fract(sin(dot(co.xy ,vec2(12.9898,78.233))) * 43758.5453);
}

vec4 sample(sampler2D tex, vec2 pos, vec2 size, vec2 sub)
{
    vec2 pt = 1.0 / (size * sub);
    float d = rand(pos + pt) * range;

    vec4 ref[4];
    ref[0] = texture(tex, pos + pt * vec2( d, 0));
    ref[1] = texture(tex, pos + pt * vec2( 0, d));
    ref[2] = texture(tex, pos + pt * vec2(-d, 0));
    ref[3] = texture(tex, pos + pt * vec2( 0,-d));

    vec4 avg = (ref[0] + ref[1] + ref[2] + ref[3])/4.0;
    vec4 col = texture(tex, pos);
    vec4 diff = abs(col - avg);

    col = mix(avg, col, greaterThan(diff, vec4(threshold/16384.0)));
    col.rgb += (grain/8192.0) *
        (vec3(rand(pos), rand(pos+vec2(d)), rand(pos-vec2(d))) - vec3(0.5));
    return col;
}
