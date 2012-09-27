# source: BitFields.jl
#
# author: Jeffrey A. Sanroff
# date  : 2012-Sep-19 in New York City


include("$(JULIA_JTM_DIR)/bitwise/BitLevel.jl")

module BitFields

export bitmaskof, bitfilterof,
       bitmaskat, bitfilterat,
       set_bitfield, get_bitfield

#       mask_in, filter_out,
#       mask_then_shift_up, shift_dn_then_mask,
#       sign_extend_up, sign_extend_bitpos_up


import Base.*

import BitLevel.bitsizeof
import BitLevel.bitspanof
import BitLevel.allbits0
import BitLevel.allbits1
import BitLevel.lsbs1
import BitLevel.msbs0
import BitLevel.signs_differ


# For bitfields that are unsigned and cannot hold negatives
#     bitspanof(   maxnng - minnng )
# For bitfields that are signed and may hold negative values
#     bitspanof( -(maxnng - maxneg) )



# bitmaskof(x) covers x with 1b
#
#                                         Signed
#
# bitmaskof(value)                         false
# bitmaskof(min_value, max_value)      (min_value < 0)

function bitmaskof{T<:Unsigned}(lo::T, hi::T)
    lsbs1( bitspanof( abs(hi-lo) ) )
end

function bitmaskof{T<:Signed}(lo::T, hi::T)
	if (hi < lo)
		lo, hi = hi, lo
	end
	if (lo < 0)
        lsbs1( bitspanof( (hi - lo) + signs_differ(lo,hi) ), convert(Unsigned, T) )
	else
	    lsbs1( bitspanof( (hi - lo) + (x==0) ), T )
	end
end


# bitfilterof(x) covers x with 0b

bitfilterof{T<:Integer}(x::T)         = ~bitmaskof(x)
bitfilterof{T<:Integer}(lo::T, hi::T) = ~bitmaskof(lo,hi)

# bitmaskof, bitfilterof shifted up to bitpos (b(N-1)..b0)
# bitmaskat(x,bitpos)
# bitfilterat(x,bitpos)

bitmaskat{T<:Integer}(x::T, bitpos::Int)         = (bitmaskof(x)     << bitpos)
bitmaskat{T<:Integer}(lo::T, hi::T, bitpos::Int) = (bitmaskof(lo,hi) << bitpos)

bitfilterat{T<:Integer}(x::T, bitpos::Int)        = ~bitmaskat(x, bitpos)
bitfilterat{T<:Integer}(lo::T, hi::T, bitpos::Int)= ~bitmaskat(lo, hi, bitpos)


# 1) type matched
# 2,3) both [un]signed, different bitwidths
# 4) different signedness, same or different bitwidths
#
function mask_in(x::Integer, mask::Integer)
	dsz = sizeof(mask) - sizeof(x)
	if (dsz == 0)    # diff signedness, same sizeof: use typeof(value)
		  ( x & reinterpret(typeof(x),mask) )
	elseif (dsz > 0) # mask in wider type than value: use typeof(mask)
	    ( convert(typeof(mask), x) & mask )
	else             # value in wider type than mask: use typeof(value)
      ( x & convert(typeof(x),mask) )
  end
end

# 1) type matched
# 2,3) both [un]signed, different bitwidths
# 4) different signedness, same or different bitwidths
#
function filter_out(x::Integer, filter::Integer)
	dsz = sizeof(filter) - sizeof(x)
	if (dsz == 0)    # diff signedness, same sizeof: use typeof(value)
		  ( x & reinterpret(typeof(x),filter) )
	elseif (dsz > 0) # filter in wider type than value: use typeof(filter)
	    ( convert(typeof(filter), x) & filter )
	else             # value in wider type than filter: use typeof(value)
      ( x & convert(typeof(x),filter) )
  end
end



# appropriate for use with all bitfields, required for shifted fields
mask_then_shift_up{T<:Integer}(value::T, mask::T, shift::Integer) =
    ((value & mask) << shift)

mask_then_shift_up(value::Integer, mask::Integer, shift::Integer) =
    (mask_in(value, mask) << shift)



# for use with bitfields designated 'unsigned' (the default)
shift_dn_then_mask{T<:Unsigned}(value::T, mask::T, shift::Integer) =
    ((value >>> shift) & mask)

shift_dn_then_mask(value::Integer, mask::Unsigned, shift::Integer) =
    mask_in((value >>> shift), mask)



# sign exteding verion
# for use with bitfields designated 'signed'
function shift_dn_then_mask{T<:Signed}(value::T, mask::T, shift::Integer )
    value = ((value >>> shift) & mask)
    sign_extend_up( value, mask )
end

function shift_dn_then_mask(value::Integer, mask::Signed, shift::Integer )
    value = mask_in((value >>> shift), mask)
    sign_extend_up( value, mask )
end


function sign_extend_up{T<:Integer}(value::T, mask::T)
    if ((value $ (mask >>> 1)) != 0)              # if the bitwidth-restricted field
         value | (~mask)                          # is < 0 (bitindex-qua-signbit is set)
    else                                          # fill 1s above for true negative value
         value
    end
end


# the sign bit is at signbit_pos, ps in 0..(N-1)
#
function sign_extend_bitpos_up(value::Integer, signbit_pos::Integer)
                                                  # if the field is stored as < 0
    if ((value & (1 << signbit_pos )) != 0)       # fill 1s above, making it a
       value | (allbits1(T) << signbit_pos)       # 2s-complement negative value
    else
       value
    end
end


# anonymous functions to use as getter and setter with bitfields
#
# put value at destination bits
# mask_then_shift_up{T<:Integer}(value::T, mask::T, shift::Int) =
#    ((value & mask) << shift)
#
# clear destination bits
# prep_embed{($T)}(embed::($T), mask::($T), shift::Integer) =
#    (embed & ~(mask << shift))
#
# incorporate bitfield value in extant multi-bitfield quantity (embed)
# set_bitfield{($T)}(embed::($T), mask::($T), shift::Int, value::Integer) =
#    (prep_embed(embed, mask, shift) | mask_then_shift_up(value, mask, shift))


for T in ( :Uint32, :Int32, :Uint64, :Int64, :Uint128, :Int128 )
	@eval begin

        prep_embed(embed::($(T)), mask::($(T)), shift::Int) =
           (embed & ~(mask << shift))

        prep_embed(embed::($(T)), mask::Integer, shift::Int) =
           (embed & convert(($T), ~(mask << shift)))

        set_bitfield(embed::($T), value::($T), mask::($T), shift::Int) =
           (prep_embed(embed, mask, shift) | mask_then_shift_up(value, mask, shift))

        set_bitfield(embed::($T),value::Integer, mask::Integer, shift::Int) =
           (prep_embed(embed, mask, shift) |
           	 convert(($T) ,mask_then_shift_up(value, mask, shift)))

        get_bitfield(embed::($T), mask::$(T), shift::Integer) =
           shift_dn_then_mask(embed, mask, shift)

        get_bitfield(embed::($T), mask::Integer, shift::Integer) =
           shift_dn_then_mask( embed, convert(($T), mask), shift)

	end
end



end # module

import BitLevel
