# Constructing functions for some concrete types.
#
# Meant to be used to improve inference, when the input arguments are not necessarily
# precisely inferred. Partial workaround for issue #42372.
module _ConstructingFunctions
    export
        _Bool,
        _Int, _Int8, _Int16, _Int32, _Int64, _Int128,
        _UInt, _UInt8, _UInt16, _UInt32, _UInt64, _UInt128,
        _Char, _String
    struct ConstructorStrict{F} <: Function end
    # Keyword arguments are not available at this point in bootstrap, so they're not
    # supported.
    function (::ConstructorStrict{F})(args::Vararg{Any, N}) where {F, N}
        @inline
        r = F(args...)
        r::F
    end
    const _Bool = ConstructorStrict{Bool}()
    const _Int = ConstructorStrict{Int}()
    const _Int8 = ConstructorStrict{Int8}()
    const _Int16 = ConstructorStrict{Int16}()
    const _Int32 = ConstructorStrict{Int32}()
    const _Int64 = ConstructorStrict{Int64}()
    const _Int128 = ConstructorStrict{Int128}()
    const _UInt = ConstructorStrict{UInt}()
    const _UInt8 = ConstructorStrict{UInt8}()
    const _UInt16 = ConstructorStrict{UInt16}()
    const _UInt32 = ConstructorStrict{UInt32}()
    const _UInt64 = ConstructorStrict{UInt64}()
    const _UInt128 = ConstructorStrict{UInt128}()
    const _Char = ConstructorStrict{Char}()
    const _String = ConstructorStrict{String}()
end

using ._ConstructingFunctions
