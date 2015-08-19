// vim: set ft=glsl:

// roughly corresponds to f3kdb parameters, which this algorithm is
// loosely inspired by
#define threshold 64
#define range     16
#define grain     24

// decent-quality PRNG, shamelessly stolen from a GLSL tricks forum post
float mod289(float x)   { return x - floor(x * (1.0 / 289.0)) * 289.0; }
float permute(float x)  { return mod289(((x*34.0)+1.0)*x); }
float rand(float x)     { return fract(x*1.0/41.0); }

vec4 sample(sampler2D tex, vec2 pos, vec2 tex_size)
{
    float h;
    // Initialize the PRNG by hashing the position + a random uniform
    vec3 m = vec3(pos, random) + vec3(1.0);
    h = permute(permute(permute(m.x)+m.y)+m.z);

    // Compute a random angle and distance
    float dist = rand(h) * range;     h = permute(h);
    float dir  = rand(h) * 6.2831853; h = permute(h);

    vec2 pt = dist / image_size;
    vec2 o = vec2(cos(dir), sin(dir));

    // Sample at quarter-turn intervals around the source pixel
    vec4 ref[4];
    ref[0] = texture(tex, pos + pt * vec2( o.x,  o.y));
    ref[1] = texture(tex, pos + pt * vec2(-o.y,  o.x));
    ref[2] = texture(tex, pos + pt * vec2(-o.x, -o.y));
    ref[3] = texture(tex, pos + pt * vec2( o.y, -o.x));

    // Average and compare with the actual sample
    vec4 avg = cmul*(ref[0] + ref[1] + ref[2] + ref[3])/4.0;
    vec4 col = cmul*texture(tex, pos);
    vec4 diff = abs(col - avg);

    // Use the average if below the threshold
    col = mix(avg, col, greaterThan(diff, vec4(threshold/16384.0)));

    // Add some random noise to the output
    vec3 noise;
    noise.x = rand(h); h = permute(h);
    noise.y = rand(h); h = permute(h);
    noise.z = rand(h); h = permute(h);

    col.rgb += (grain/8192.0) * (noise - vec3(0.5));
    return col;
}
