# source: bitfields.jl
#
# author: Jeffrey A. Sanroff
# date  : 2012-Sep-19 in New York City

module BitFields

export sgnd_bitmaskof, unsd_bitmaskof,
       shift0s_up, shift0s_dn,
       shift1s_up, shift1s_dn,
       sign_extend_up, sign_extend_bitpos_up,
       mask_then_shift_up, shift_dn_then_mask


include("($JULIA_JTM_DIR)/bitwise/BitLevel.jl")
import BitLevel.bitsizeof
import BitLevel.bitspanof
import BitLevel.allbits0
import BitLevel.allbits1
import BitLevel.lsbs1
import BitLevel.msbs0
import Bitlevel.signs_differ
import Bitlevel.convert

# For bitfields that are unsigned and cannot hold negatives
#     bitspanof(   maxnng - minnng )
# For bitfields that are signed and may hold negative values
#     bitspanof( -(maxnng - maxneg) )


# shift0s_up: (n=2) 0b1110..01..011 ==> 0b..01..01100
# shift0s_dn: (n=2) 0b1110..01..011 ==> 0b001110..01..0

shift0s_up(x::Integer, n::Integer) = (x <<  n)
shift0s_dn(x::Integer, n::Integer) = (x >>> n)

shift1s_up(x::Integer, n::Integer) = (x <<  n) | lsbs1(n)
shift1s_dn(x::Integer, n::Integer) = (x >>> n) | msbs1(n)


sgnd_bitmaskof(x::Integer) = lsbs1( bitspanof(x), typeof(x) )

function sgnd_bitmaskof{T<:Integer}(lo::T, hi::T)
	if (hi < lo)
		lo, hi = hi, lo
	end
    range = hi - lo + signs_differ(hi,lo)
    lsb1( bitspanof(range), T)
end

unsd_bitmaskof(x::Integer) = convert(Unsigned, sgnd_bitmaskof(x))
unsd_bitmaskof{T<:Integer}(lo::T, hi::T) = convert(Unsigned, sgnd_bitmaskof(x))

sghd_bitfilterof{T<:Integer}( ~(sgnd_bitmaskof(x)) )
sgnd_bitfilterof{T<:Integer}(lo::T, hi::T)( ~(sgnd_bitmaskof(x)) )

unsd_bitfilterof{T<:Integer}( ~(unsd_bitmaskof(x)) )
unsd_bitfilterof{T<:Integer}(lo::T, hi::T)( ~(unsd_bitmaskof(x)) )


sign_extend_up{T<:Integer}(value::T, mask::T)
    if ((value $ (mask >>> 1)) != 0)              # if the bitwidth-restricted field
         value | (~mask)                          # is < 0 (bitindex-qua-signbit is set)
    else                                          # fill 1s above for true negative value
         value
    end
end

sign_extend_bitpos_up(value::Integer, signbit_index::Integer)
                                                  # if the field is stored as < 0
    if ((value & (1 << signbit_index )) != 0)     # fill 1s above, making it a
       value | (allbits1(T) << signbit_index)     # 2s-complement negative value
    else
       value
    end
end


# appropriate for use with all bitfields, required for shifted fields
mask_then_shift_up{T}(source_value::T, mask::T, shift::Integer)
    ((source_value & mask) << shift)

# for use with bitfields that must cover the lsbs of its carrier type
mask_then_shift_up{T}(source_value::T, mask::T) = (source_value & mask)
shift_dn_then_mask{T}(source_value::T, mask::T) = (source_value & mask)


# for use with bitfields designated 'unsigned' (the default)
shift_dn_then_mask{T}(target_value::T, mask::T, shift::Integer) =
    ((target_value >>> shift) & mask)

# sign exteding verion
# for use with bitfields designated 'signed'                    dispatch mostly
shift_dn_then_mask{T}(target_value::T, mask::T, shift::Integer, sign_extend::Bool )
    target_value = ((target_value >>> shift) & mask)
    sign_extend_up( target_value, mask )
end







end # module
