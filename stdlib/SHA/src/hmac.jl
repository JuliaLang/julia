# This file is a part of Julia. License is MIT: https://julialang.org/license

struct HMAC_CTX{CTX<:SHA_CTX}
    context::CTX
    outer::Vector{UInt8}

    function HMAC_CTX(ctx::CTX, key::Vector{UInt8}, blocksize::Integer=blocklen(CTX)) where CTX
        if length(key) > blocksize
            _ctx = CTX()
            update!(_ctx, key)
            key = digest!(_ctx)
        end

        pad = blocksize - length(key)

        if pad > 0
            key = [key; fill(0x00, pad)]
        end

        update!(ctx, key .⊻ 0x36)
        new{CTX}(ctx, key .⊻ 0x5c)
    end
end

function update!(ctx::HMAC_CTX, data, datalen=length(data))
    update!(ctx.context, data, datalen)
end

function digest!(ctx::HMAC_CTX{CTX}) where CTX
    digest = digest!(ctx.context)
    _ctx = CTX()
    update!(_ctx, ctx.outer)
    update!(_ctx, digest)
    digest!(_ctx)
end
