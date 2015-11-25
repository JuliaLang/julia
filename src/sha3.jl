function transform!{T<:SHA3_CTX}(context::T)
    # First, update state with buffer
    buffer_as_uint64 = reinterpret(eltype(context.state), context.buffer)
    for idx in 1:div(blocklen(T),8)
        context.state[idx] $= buffer_as_uint64[idx]
    end
    bc = Array{UInt64,1}(5)

    # We always assume 24 rounds
    for round in 0:23
        # Theta function
        for i in 1:5
            bc[i] = context.state[i] $ context.state[i + 5] $ context.state[i + 10] $ context.state[i + 15] $ context.state[i + 20]
        end

        for i in 1:5
            temp = bc[mod1(i + 4, 5)] $ L64(1, bc[mod1(i + 1, 5)])
            for j in 0:5:20
                context.state[i + j] $= temp
            end
        end

        # Rho Pi
        temp = context.state[2]
        for i in 1:24
            j = SHA3_PILN[i]
            bc[1] = context.state[j]
            context.state[j] = L64(SHA3_ROTC[i], temp)
            temp = bc[1]
        end

        # Chi
        for j in 0:5:20
            for i in 1:5
                bc[i] = context.state[i + j]
            end
            for i in 1:5
                context.state[j + i] $= (~bc[mod1(i + 1, 5)] & bc[mod1(i + 2, 5)])
            end
        end

        # Iota
        context.state[1] $= SHA3_ROUND_CONSTS[round+1]
    end

    return context.state
end



# Finalize data in the buffer, append total bitlength, and return our precious hash!
function digest!{T<:SHA3_CTX}(context::T)
    usedspace = context.bytecount % blocklen(T)
    # If we have anything in the buffer still, pad and transform that data
    if usedspace < blocklen(T) - 1
        # Begin padding with a 0x06
        context.buffer[usedspace+1] = 0x06
        # Fill with zeros up until the last byte
        context.buffer[usedspace+2:end-1] = 0x00
        # Finish it off with a 0x80
        context.buffer[end] = 0x80
    else
        # Otherwise, we have to add on a whole new buffer just for the zeros and 0x80
        context.buffer[end] = 0x06
        transform!(context)

        context.buffer[1:end-1] = 0x0
        context.buffer[end] = 0x80
    end

    # Final transform:
    transform!(context)

    # Return the digest
    return reinterpret(UInt8, context.state)[1:digestlen(T)]
end
