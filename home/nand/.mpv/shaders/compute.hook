//!HOOK LUMA
//!BIND HOOKED
//!COMPUTE 32 8
//!DESC avg convolution

// Kernel size, 9x9 as an example
const ivec2 ksize = ivec2(9, 9);
const ivec2 offset = ksize / 2;

// We need to load extra source texels to account for padding due to kernel
// overhang
const ivec2 isize = ivec2(gl_WorkGroupSize) + ksize - 1;

shared float inp[isize.y][isize.x];

vec4 hook()
{
    // load texels into shmem
    ivec2 base = ivec2(gl_WorkGroupID) * ivec2(gl_WorkGroupSize);
    for (uint y = gl_LocalInvocationID.y; y < isize.y; y += gl_WorkGroupSize.y) {
        for (uint x = gl_LocalInvocationID.x; x < isize.x; x += gl_WorkGroupSize.x)
            inp[y][x] = texelFetch(HOOKED_raw, base + ivec2(x,y) - offset, 0).x;
    }

    // ensure writes visible
    groupMemoryBarrier();
    barrier();

    // do convolution
    float sum;
    for (uint y = 0; y < ksize.y; y++) {
        for (uint x = 0; x < ksize.x; x++)
            sum += inp[gl_LocalInvocationID.y+y][gl_LocalInvocationID.x+x];
    }

    return vec4(sum / (ksize.x * ksize.y), 0, 0, 1);
}
