// vim: set ft=glsl:

// roughly corresponds to f3kdb parameters, which this algorithm is
// loosely inspired by
#define threshold 64
#define range     16
#define grain     24

float rand(vec2 co) {
    return fract(sin(dot(co.yx, vec2(12.9898,78.233))) * 43758.5453);
}

vec4 sample(sampler2D tex, vec2 pos, vec2 tex_size)
{
    // Compute random angle and distance pairs
    vec2 dist = vec2(rand(pos + vec2(random)), rand(pos - vec2(random))) * range;
    vec4 pt = vec4(dist.x / image_size, dist.y / image_size);
    vec2 dir = vec2(rand(pos.yx - vec2(random)), rand(pos.xy + vec2(random))) * 6.2831853;
    vec4 o = vec4(cos(dir.x), sin(dir.x), cos(dir.y), sin(dir.y));

    // Sample two quarter-turn patterns around the source pixel
    vec4 ref[8];
    ref[0] = texture(tex, pos + pt.x * vec2( o.x,  o.y));
    ref[1] = texture(tex, pos + pt.x * vec2(-o.y,  o.x));
    ref[2] = texture(tex, pos + pt.x * vec2(-o.x, -o.y));
    ref[3] = texture(tex, pos + pt.x * vec2( o.y, -o.x));
    ref[4] = texture(tex, pos + pt.y * vec2( o.z,  o.w));
    ref[5] = texture(tex, pos + pt.y * vec2(-o.w,  o.z));
    ref[6] = texture(tex, pos + pt.y * vec2(-o.z, -o.w));
    ref[7] = texture(tex, pos + pt.y * vec2( o.w, -o.z));

    // Average and compare with the actual sample
    vec4 avg = cmul*(ref[0]+ref[1]+ref[2]+ref[3]+ref[4]+ref[5]+ref[6]+ref[7])/8.0;
    vec4 col = cmul*texture(tex, pos);
    vec4 diff = abs(col - avg);

    // Use the average if below the threshold
    col = mix(avg, col, greaterThan(diff, vec4(threshold/16384.0)));

    // Add some random noise to the output
    vec3 noise = vec3(rand(2*pos + vec2(random)),
                      rand(3*pos + vec2(random)),
                      rand(4*pos + vec2(random)));
    col.rgb += (grain/8192.0) * (noise - vec3(0.5));
    return col;
}
