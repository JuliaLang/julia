# This is a stable least significant bit first radix sort.
#
# That is, it first sorts the entire vector by the last chunk_size bits, then by the second
# to last chunk_size bits, and so on. Stability means that it will not reorder two elements
# that compare equal. This is essential so that the order introduced by earlier,
# less significant passes is preserved by later passes.
#
# Each pass divides the input into 2^chunk_size == mask+1 buckets. To do this, it
#  * counts the number of entries that fall into each bucket
#  * uses those counts to compute the indices to move elements of those buckets into
#  * moves elements into the computed indices in the swap array
#  * switches the swap and working array
#
# In the case of an odd number of passes, the returned vector will === the input vector t,
# not v. This is one of the many reasons radix_sort! is not exported.
function radix_sort!(v::AbstractVector{U}, lo::Integer, hi::Integer, bits::Unsigned,
                     t::AbstractVector{U}, chunk_size=radix_chunk_size_heuristic(lo, hi, bits)) where U <: Unsigned
    # bits is unsigned for performance reasons.
    mask = UInt(1) << chunk_size - 1
    counts = Vector{UInt}(undef, mask+2)

    @inbounds for shift in 0:chunk_size:bits-1

        # counts[2:mask+2] will store the number of elements that fall into each bucket.
        # if chunk_size = 8, counts[2] is bucket 0x00 and counts[257] is bucket 0xff.
        counts .= 0
        for k in lo:hi
            x = v[k]                  # lookup the element
            i = (x >> shift)&mask + 2 # compute its bucket's index for this pass
            counts[i] += 1            # increment that bucket's count
        end

        counts[1] = lo                # set target index for the first bucket
        cumsum!(counts, counts)       # set target indices for subsequent buckets
        # counts[1:mask+1] now stores indices where the first member of each bucket
        # belongs, not the number of elements in each bucket. We will put the first element
        # of bucket 0x00 in t[counts[1]], the next element of bucket 0x00 in t[counts[1]+1],
        # and the last element of bucket 0x00 in t[counts[2]-1].

        for k in lo:hi
            x = v[k]                  # lookup the element
            i = (x >> shift)&mask + 1 # compute its bucket's index for this pass
            j = counts[i]             # lookup the target index
            t[j] = x                  # put the element where it belongs
            counts[i] = j + 1         # increment the target index for the next
        end                           #  ↳ element in this bucket

        v, t = t, v # swap the now sorted destination vector t back into primary vector v

    end

    v
end
function radix_chunk_size_heuristic(lo::Integer, hi::Integer, bits::Unsigned)
    # chunk_size is the number of bits to radix over at once.
    # We need to allocate an array of size 2^chunk size, and on the other hand the higher
    # the chunk size the fewer passes we need. Theoretically, chunk size should be based on
    # the Lambert W function applied to length. Empirically, we use this heuristic:
    guess = min(10, log(maybe_unsigned(hi-lo))*3/4+3)
    # TODO the maximum chunk size should be based on architecture cache size.

    # We need iterations * chunk size ≥ bits, and these cld's
    # make an effort to get iterations * chunk size ≈ bits
    UInt8(cld(bits, cld(bits, guess)))
end
