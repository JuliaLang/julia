# This file is a part of Julia. License is MIT: https://julialang.org/license

# Nonlinear functions, in order to encourage inlining, these sadly are not an array of lambdas
function Round0(b,c,d)
    return UInt32((b & c) | (~b & d))
end

function Round1And3(b,c,d)
    return UInt32(b ⊻ c ⊻ d)
end

function Round2(b,c,d)
    return UInt32((b & c) | (b & d) | (c & d))
end

function transform!(context::SHA1_CTX)
    # Buffer is 16 elements long, we expand to 80
    pbuf = buffer_pointer(context)
    for i in 1:16
        context.W[i] = bswap(unsafe_load(pbuf, i))
    end

    # First round of expansions
    for i in 17:32
        @inbounds begin
            context.W[i] = lrot(1, context.W[i-3] ⊻ context.W[i-8] ⊻ context.W[i-14] ⊻ context.W[i-16], 32)
        end
    end

    # Second round of expansions (possibly 4-way SIMD-able)
    for i in 33:80
        @inbounds begin
            context.W[i] = lrot(2, context.W[i-6] ⊻ context.W[i-16] ⊻ context.W[i-28] ⊻ context.W[i-32], 32)
        end
    end

    # Initialize registers with the previous intermediate values (our state)
    a = context.state[1]
    b = context.state[2]
    c = context.state[3]
    d = context.state[4]
    e = context.state[5]

    # Run our rounds, manually separated into the four rounds, unfortunately using an array of lambdas
    # really kills performance and causes a huge number of allocations, so we make it easy on the compiler
    for i = 1:20
        @inbounds begin
            temp = UInt32(lrot(5, a, 32) + Round0(b,c,d) + e + context.W[i] + K1[1])
            e = d
            d = c
            c = lrot(30, b, 32)
            b = a
            a = temp
        end
    end

    for i = 21:40
        @inbounds begin
            temp = UInt32(lrot(5, a, 32) + Round1And3(b,c,d) + e + context.W[i] + K1[2])
            e = d
            d = c
            c = lrot(30, b, 32)
            b = a
            a = temp
        end
    end

    for i = 41:60
        @inbounds begin
            temp = UInt32(lrot(5, a, 32) + Round2(b,c,d) + e + context.W[i] + K1[3])
            e = d
            d = c
            c = lrot(30, b, 32)
            b = a
            a = temp
        end
    end

    for i = 61:80
        @inbounds begin
            temp = UInt32(lrot(5, a, 32) + Round1And3(b,c,d) + e + context.W[i] + K1[4])
            e = d
            d = c
            c = lrot(30, b, 32)
            b = a
            a = temp
        end
    end

    context.state[1] += a
    context.state[2] += b
    context.state[3] += c
    context.state[4] += d
    context.state[5] += e
end
