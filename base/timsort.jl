# timsort for julia
#
# This is an implementation of timsort based on the algorithm description at
#
#    http://svn.python.org/projects/python/trunk/Objects/listsort.txt
#    http://en.wikipedia.org/wiki/Timsort
#
# @kmsquire


typealias Run Range1{Int}

const MIN_GALLOP = 7

type MergeState
    runs::Array{Run}
    min_gallop::Int
end
MergeState() = MergeState(Run[], MIN_GALLOP)

# Determine a good minimum run size for efficient merging
# For details, see "Computing minrun" in 
# http://svn.python.org/projects/python/trunk/Objects/listsort.txt
function merge_compute_minrun(N::Int, bits::Int)
    r = 0
    max_val = 2^bits
    while N >= max_val
        r |= (N & 1)
        N >>= 1
    end
    N + r
end

merge_compute_minrun(N::Int) = merge_compute_minrun(N, 6)

# Macro to create different versions of the sort function,
# cribbed from sort.jl
macro _jl_timsort_functions(suffix, lt, args...)
insertionsort = esc(symbol("insertionsort$(suffix)!"))
timsort = esc(symbol("timsort$(suffix)!"))
next_run = esc(symbol("_jl_next_run$suffix"))
merge_collapse = esc(symbol("_jl_merge_collapse$suffix"))
merge = esc(symbol("_jl_merge$suffix"))
merge_lo = esc(symbol("_jl_merge_lo$suffix"))
merge_hi = esc(symbol("_jl_merge_hi$suffix"))

gallop_last = esc(symbol("_jl_gallop_last$suffix"))
gallop_first = esc(symbol("_jl_gallop_first$suffix"))
rgallop_last = esc(symbol("_jl_rgallop_last$suffix"))
rgallop_first = esc(symbol("_jl_rgallop_first$suffix"))

lt = @eval (a,b)->$lt

quote


# Galloping binary search starting at left
function ($gallop_last)($(args...), a::AbstractVector, x, lo::Int, hi::Int)
    i = lo
    inc = 1
    lo = lo-1
    hi = hi+1
    while i < hi && !($(lt(:x, :(a[i]))))
        lo = i
        i += inc
        inc <<= 1
    end
    hi = min(i+1, hi)
    # Binary search
    while lo < hi-1
        i = (lo+hi)>>>1
        if !($(lt(:x, :(a[i]))))
            lo = i
        else
            hi = i
        end
    end
    hi
end


# Galloping binary search starting at right
function ($rgallop_last)($(args...), a::AbstractVector, x, lo::Int, hi::Int)
    i = hi
    dec = 1
    lo = lo-1
    hi = hi+1
    while i > lo && $(lt(:x, :(a[i])))
        hi = i
        i -= dec
        dec <<= 1
    end
    lo = max(lo, i-1)
    # Binary search
    while lo < hi-1
        i = (lo+hi)>>>1
        if !($(lt(:x, :(a[i]))))
            lo = i
        else
            hi = i
        end
    end
    hi
end


# Galloping binary search starting at left
function ($gallop_first)($(args...), a::AbstractVector, x, lo::Int, hi::Int)
    i = lo
    inc = 1
    lo = lo-1
    hi = hi+1
    while i < hi && $(lt(:(a[i]),:x))
        lo = i
        i += inc
        inc <<= 1
    end
    hi = min(i+1, hi)
    # Binary search
    while lo < hi-1
        i = (lo+hi)>>>1
        if $(lt(:(a[i]), :x))
            lo = i
        else
            hi = i
        end
    end
    hi
end


# Galloping binary search starting at right
function ($rgallop_first)($(args...), a::AbstractVector, x, lo::Int, hi::Int)
    i = hi
    dec = 1
    lo = lo-1
    hi = hi+1
    while i > lo && !($(lt(:(a[i]),:x)))
        hi = i
        i -= dec
        dec <<= 1
    end
    lo = max(lo, i-1)
    # Binary search
    while lo < hi-1
        i = (lo+hi)>>>1
        if $(lt(:(a[i]), :x))
            lo = i
        else
            hi = i
        end
    end
    hi
end


# Get the next run
# Returns the a range a:b, or b:-1:a for a reversed sequence
function ($next_run)($(args...), v::AbstractVector, lo::Int, hi::Int)
    if lo == hi
        return lo:lo
    end
   
    if !($(lt(:(v[lo+1]), :(v[lo]))))
        for i = lo+2:hi
            if $(lt(:(v[i]), :(v[i-1])))
                return lo:i-1
            end
        end
        return lo:hi
    else
        for i = lo+2:hi
            if !($(lt(:(v[i]), :(v[i-1]))))
                return i-1:-1:lo
            end
        end
        return hi:-1:lo
    end
end


# Merge consecutive runs
# For A,B,C = last three lengths, merge_collapse!()
# maintains 2 invariants:
#
#  A > B + C
#  B > C
#
# If any of these are violated, a merge occurs to 
# correct it
function ($merge_collapse)($(args...), v::AbstractVector, state::MergeState, force::Bool)

    while length(state.runs) > 2
        (a,b,c) = state.runs[end-2:end]
       
        if length(a) > length(b)+length(c) && length(b) > length(c) && !force
            break # invariants are satisfied, leave loop
        end

        if length(a) < length(c)
            ($merge)($(args...), v,a,b,state)
            pop(state.runs)
            pop(state.runs)
            pop(state.runs)
            push(state.runs, first(a):last(b))
            push(state.runs, c)
        else
            ($merge)($(args...), v,b,c,state)
            pop(state.runs)
            pop(state.runs)
            push(state.runs, first(b):last(c))
        end
    end

    if length(state.runs) == 2
        (a,b) = state.runs[end-1:end]

        if length(a) <= length(b) || force
            ($merge)($(args...), v,a,b,state)
            pop(state.runs)
            pop(state.runs)
            push(state.runs, first(a):last(b))
        end
    end

    return # v   ## Used to return v, but should be unnecessary, as the only user of this function
                  ## is timsort(), and the return value is ignored there...
end


# Version which permutes an auxilliary array mirroring the sort
function ($merge_collapse)($(args...), v::AbstractVector, p::AbstractVector{Int}, state::MergeState, force::Bool)

    while length(state.runs) > 2
        (a,b,c) = state.runs[end-2:end]
       
        if length(a) > length(b)+length(c) && length(b) > length(c) && !force
            break # invariants are satisfied, leave loop
        end

        if length(a) < length(c)
            ($merge)($(args...), v,p,a,b,state)
            pop(state.runs)
            pop(state.runs)
            pop(state.runs)
            push(state.runs, first(a):last(b))
            push(state.runs, c)
        else
            ($merge)($(args...), v,p,b,c,state)
            pop(state.runs)
            pop(state.runs)
            push(state.runs, first(b):last(c))
        end
    end

    if length(state.runs) == 2
        (a,b) = state.runs[end-1:end]

        if length(a) <= length(b) || force
            ($merge)($(args...), v,p,a,b,state)
            pop(state.runs)
            pop(state.runs)
            push(state.runs, first(a):last(b))
        end
    end

    return # v   ## Used to return v, but should be unnecessary, as the only user of this function
                  ## is timsort(), and the return value is ignored there...
end


# Merge runs a and b in vector v
function ($merge)($(args...), v::AbstractVector, a::Run, b::Run, state::MergeState)

    # First elements in a <= b[1] are already in place
    a = ($gallop_last)($(args...), v, v[first(b)], first(a), last(a)) : last(a)

    # Last elements in b >= a[end] are already in place
    b = first(b) : ($rgallop_first)($(args...), v, v[last(a)], first(b), last(b))-1

    if length(a) == 0 || length(b) == 0
        return
    end

    # Choose merge_lo or merge_hi based on the amount
    # of temporary memory needed (smaller of a and b)
    if length(a) < length(b)
        ($merge_lo)($(args...), v, a, b, state)
    else
        ($merge_hi)($(args...), v, a, b, state)
    end
end


# Merge runs a and b in vector v (verseion which permutes an auxilliary array mirroring the sort)
function ($merge)($(args...), v::AbstractVector, p::AbstractVector{Int}, a::Run, b::Run, state::MergeState)

    # First elements in a <= b[1] are already in place
    a = ($gallop_last)($(args...), v, v[first(b)], first(a), last(a)) : last(a)

    # Last elements in b >= a[end] are already in place
    b = first(b) : ($rgallop_first)($(args...), v, v[last(a)], first(b), last(b))-1

    if length(a) == 0 || length(b) == 0
        return
    end

    # Choose merge_lo or merge_hi based on the amount
    # of temporary memory needed (smaller of a and b)
    if length(a) < length(b)
        ($merge_lo)($(args...), v, p, a, b, state)
    else
        ($merge_hi)($(args...), v, p, a, b, state)
    end
end

# Merge runs a and b in vector v (a is smaller)
function ($merge_lo)($(args...), v::AbstractVector, a::Run, b::Run, state::MergeState)

    # Copy a
    v_a = v[a]

    # Pointer into (sub)arrays
    i = first(a)
    from_a = 1
    from_b = first(b)

    mode = :normal
    while true
        if mode == :normal
            # Compare and copy element by element
            count_a = count_b = 0
            while from_a <= length(a) && from_b <= last(b)
                if $(lt(:(v[from_b]), :(v_a[from_a])))
                    v[i] = v[from_b]
                    from_b += 1
                    count_a = 0
                    count_b += 1
                else
                    v[i] = v_a[from_a]
                    from_a += 1
                    count_a += 1
                    count_b = 0
                end
                i += 1

                # Switch to galloping mode if a string of elements
                # has come from the same set
                if count_b >= state.min_gallop || count_a >= state.min_gallop
                    mode = :galloping
                    break
                end
            end
            # Finalize if we've exited the loop normally
            if mode == :normal
                mode = :finalize
            end
        end

        if mode == :galloping
            # Use binary search to find range to copy
            while from_a <= length(a) && from_b <= last(b)
                # Copy the next run from b
                b_run = from_b : ($gallop_first)($(args...), v, v_a[from_a], from_b, last(b)) - 1
                i_end = i + length(b_run) - 1
                v[i:i_end] = v[b_run]
                i = i_end + 1
                from_b = last(b_run) + 1
                
                # ... then copy the first element from a
                v[i] = v_a[from_a]
                i += 1
                from_a += 1

                if from_a > length(a) || from_b > last(b) break end

                # Copy the next run from a
                a_run = from_a : ($gallop_last)($(args...), v_a, v[from_b], from_a, length(a)) - 1
                i_end = i + length(a_run) - 1
                v[i:i_end] = v_a[a_run]
                i = i_end + 1
                from_a = last(a_run) + 1

                # ... then copy the first element from b
                v[i] = v[from_b]
                i += 1
                from_b += 1

                # Return to normal mode if we haven't galloped...
                if length(a_run) < MIN_GALLOP && length(b_run) < MIN_GALLOP
                    mode = :normal
                    break
                end
                # Reduce min_gallop if this gallop was successful
                state.min_gallop -= 1
            end
            if mode == :galloping
                mode = :finalize
            end
            state.min_gallop = max(state.min_gallop,0) + 2  # penalty for leaving gallop mode
        end

        if mode == :finalize
            # copy end of a
            i_end = i + (length(a) - from_a)
            v[i:i_end] = v_a[from_a:end]
            break
        end
    end
end

# Merge runs a and b in vector v (a is smaller)
function ($merge_lo)($(args...), v::AbstractVector, p::AbstractVector{Int}, a::Run, b::Run, state::MergeState)

    # Copy a
    v_a = v[a]
    p_a = p[a]

    # Pointer into (sub)arrays
    i = first(a)
    from_a = 1
    from_b = first(b)

    mode = :normal
    while true
        if mode == :normal
            # Compare and copy element by element
            count_a = count_b = 0
            while from_a <= length(a) && from_b <= last(b)
                if $(lt(:(v[from_b]), :(v_a[from_a])))
                    v[i] = v[from_b]
                    p[i] = p[from_b]
                    from_b += 1
                    count_a = 0
                    count_b += 1
                else
                    v[i] = v_a[from_a]
                    p[i] = p_a[from_a]
                    from_a += 1
                    count_a += 1
                    count_b = 0
                end
                i += 1

                # Switch to galloping mode if a string of elements
                # has come from the same set
                if count_b >= state.min_gallop || count_a >= state.min_gallop
                    mode = :galloping
                    break
                end
            end
            # Finalize if we've exited the loop normally
            if mode == :normal
                mode = :finalize
            end
        end

        if mode == :galloping
            # Use binary search to find range to copy
            while from_a <= length(a) && from_b <= last(b)
                # Copy the next run from b
                b_run = from_b : ($gallop_first)($(args...), v, v_a[from_a], from_b, last(b)) - 1
                i_end = i + length(b_run) - 1
                v[i:i_end] = v[b_run]
                p[i:i_end] = p[b_run]
                i = i_end + 1
                from_b = last(b_run) + 1
                
                # ... then copy the first element from a
                v[i] = v_a[from_a]
                p[i] = p_a[from_a]
                i += 1
                from_a += 1

                if from_a > length(a) || from_b > last(b) break end

                # Copy the next run from a
                a_run = from_a : ($gallop_last)($(args...), v_a, v[from_b], from_a, length(a)) - 1
                i_end = i + length(a_run) - 1
                v[i:i_end] = v_a[a_run]
                p[i:i_end] = p_a[a_run]
                i = i_end + 1
                from_a = last(a_run) + 1

                # ... then copy the first element from b
                v[i] = v[from_b]
                p[i] = p[from_b]
                i += 1
                from_b += 1

                # Return to normal mode if we haven't galloped...
                if length(a_run) < MIN_GALLOP && length(b_run) < MIN_GALLOP
                    mode = :normal
                    break
                end
                # Reduce min_gallop if this gallop was successful
                state.min_gallop -= 1
            end
            if mode == :galloping
                mode = :finalize
            end
            state.min_gallop = max(state.min_gallop,0) + 2  # penalty for leaving gallop mode
        end

        if mode == :finalize
            # copy end of a
            i_end = i + (length(a) - from_a)
            v[i:i_end] = v_a[from_a:end]
            p[i:i_end] = p_a[from_a:end]
            break
        end
    end
end


# Merge runs a and b in vector v (b is smaller)
function ($merge_hi)($(args...), v::AbstractVector, a::Run, b::Run, state::MergeState)

    # Copy b
    v_b = v[b]

    # Pointer into (sub)arrays
    i = last(b)
    from_a = last(a)
    from_b = length(b)

    mode = :normal
    while true
        if mode == :normal
            # Compare and copy element by element
            count_a = count_b = 0
            while from_a >= first(a) && from_b >= 1
                if $(lt(:(v[from_a]), :(v_b[from_b])))
                    v[i] = v_b[from_b]
                    from_b -= 1
                    count_a = 0
                    count_b += 1
                else
                    v[i] = v[from_a]
                    from_a -= 1
                    count_a += 1
                    count_b = 0
                end
                i -= 1

                # Switch to galloping mode if a string of elements
                # has come from the same set
                if count_b >= state.min_gallop || count_a >= state.min_gallop
                   mode = :galloping
                   break
                end
            end
            # Finalize if we've exited the loop normally
            if mode == :normal
                mode = :finalize
            end
        end

        if mode == :galloping
            # Use binary search to find range to copy
            while from_a >= first(a) && from_b >= 1
                # Copy the next run from b
                b_run = ($rgallop_first)($(args...), v_b, v[from_a], 1, from_b) : from_b
                i_start = i - length(b_run) + 1
                v[i_start:i] = v_b[b_run]
                i = i_start - 1
                from_b = first(b_run) - 1

                # ... then copy the first element from a
                v[i] = v[from_a]
                i -= 1
                from_a -= 1

                if from_a < first(a) || from_b < 1 break end
                
                # Copy the next run from a
                a_run = ($rgallop_last)($(args...), v, v_b[from_b], first(a), from_a) : from_a
                i_start = i - length(a_run) + 1
                v[i_start:i] = v[a_run]
                i = i_start - 1
                from_a = first(a_run) - 1
                
                # ... then copy the first element from b
                v[i] = v_b[from_b]
                i -= 1
                from_b -= 1

                # Return to normal mode if we haven't galloped...
                if length(a_run) < MIN_GALLOP && length(b_run) < MIN_GALLOP
                    mode = :normal
                    break
                end
                # Reduce min_gallop if this gallop was successful
                state.min_gallop -= 1
            end
            if mode == :galloping
                mode = :finalize
            end
            state.min_gallop = max(state.min_gallop, 0) + 2  # penalty for leaving gallop mode
        end

        if mode == :finalize
            # copy start of b
            i_start = i - from_b + 1
            v[i_start:i] = v_b[1:from_b]
            break
        end
    end
end

# Merge runs a and b in vector v (b is smaller)
function ($merge_hi)($(args...), v::AbstractVector, p::AbstractVector{Int}, a::Run, b::Run, state::MergeState)

    # Copy b
    v_b = v[b]
    p_b = p[b]

    # Pointer into (sub)arrays
    i = last(b)
    from_a = last(a)
    from_b = length(b)

    mode = :normal
    while true
        if mode == :normal
            # Compare and copy element by element
            count_a = count_b = 0
            while from_a >= first(a) && from_b >= 1
                if $(lt(:(v[from_a]), :(v_b[from_b])))
                    v[i] = v_b[from_b]
                    p[i] = p_b[from_b]
                    from_b -= 1
                    count_a = 0
                    count_b += 1
                else
                    v[i] = v[from_a]
                    p[i] = p[from_a]
                    from_a -= 1
                    count_a += 1
                    count_b = 0
                end
                i -= 1

                # Switch to galloping mode if a string of elements
                # has come from the same set
                if count_b >= state.min_gallop || count_a >= state.min_gallop
                   mode = :galloping
                   break
                end
            end
            # Finalize if we've exited the loop normally
            if mode == :normal
                mode = :finalize
            end
        end

        if mode == :galloping
            # Use binary search to find range to copy
            while from_a >= first(a) && from_b >= 1
                # Copy the next run from b
                b_run = ($rgallop_first)($(args...), v_b, v[from_a], 1, from_b) : from_b
                i_start = i - length(b_run) + 1
                v[i_start:i] = v_b[b_run]
                p[i_start:i] = p_b[b_run]
                i = i_start - 1
                from_b = first(b_run) - 1

                # ... then copy the first element from a
                v[i] = v[from_a]
                p[i] = p[from_a]
                i -= 1
                from_a -= 1

                if from_a < first(a) || from_b < 1 break end
                
                # Copy the next run from a
                a_run = ($rgallop_last)($(args...), v, v_b[from_b], first(a), from_a) : from_a
                i_start = i - length(a_run) + 1
                v[i_start:i] = v[a_run]
                p[i_start:i] = p[a_run]
                i = i_start - 1
                from_a = first(a_run) - 1
                
                # ... then copy the first element from b
                v[i] = v_b[from_b]
                p[i] = p_b[from_b]
                i -= 1
                from_b -= 1

                # Return to normal mode if we haven't galloped...
                if length(a_run) < MIN_GALLOP && length(b_run) < MIN_GALLOP
                    mode = :normal
                    break
                end
                # Reduce min_gallop if this gallop was successful
                state.min_gallop -= 1
            end
            if mode == :galloping
                mode = :finalize
            end
            state.min_gallop = max(state.min_gallop, 0) + 2  # penalty for leaving gallop mode
        end

        if mode == :finalize
            # copy start of b
            i_start = i - from_b + 1
            v[i_start:i] = v_b[1:from_b]
            p[i_start:i] = p_b[1:from_b]
            break
        end
    end
end


# Timsort function
function ($timsort)($(args...), v::AbstractVector, lo::Int, hi::Int)
    # Initialization
    minrun = merge_compute_minrun(hi-lo+1)
    state = MergeState()

    i = lo
    while i <= hi
        run_range = $(next_run)($(args...), v, i, hi)
        count = length(run_range)
        if count < minrun
            # Make a run of length minrun
            count = min(minrun, hi-i+1)
            run_range = i:i+count-1
            ($insertionsort)($(args...), v, i, i+count-1)
        else
            if !issorted(run_range)
                run_range = last(run_range):first(run_range)
                reverse!(sub(v, run_range))
            end
        end

        # Push this run onto the queue and merge if needed
        push(state.runs, run_range)
        i = i+count
        ($merge_collapse)($(args...), v, state, false)
    end

    # Force merge at the end
    if length(state.runs) > 1
        ($merge_collapse)($(args...), v, state, true)
    end

    v
end

($timsort)($(args...), v::AbstractVector) = ($timsort)($(args...), v, 1, length(v))


# Timsort function which permutes an auxilliary array mirroring the sort
function ($timsort)($(args...), v::AbstractVector, p::AbstractVector{Int}, lo::Int, hi::Int)
    # Initialization
    minrun = merge_compute_minrun(hi-lo+1)
    state = MergeState()

    i = lo
    while i <= hi
        run_range = $(next_run)($(args...), v, i, hi)
        count = length(run_range)
        if count < minrun
            # Make a run of length minrun
            count = min(minrun, hi-i+1)
            run_range = i:i+count-1
            ($insertionsort)($(args...), v, p, i, i+count-1)
        else
            if !issorted(run_range)
                #reverse!(sub(v, run_range))
                # Why is this faster?
                for j = 0:div(count,2)-1
                    v[i+j], v[i+count-j-1] = v[i+count-j-1], v[i+j]
                    p[i+j], p[i+count-j-1] = p[i+count-j-1], p[i+j]
                end
                run_range = last(run_range):first(run_range)
            end
        end

        # Push this run onto the queue and merge if needed
        push(state.runs, run_range)
        i = i+count
        ($merge_collapse)($(args...), v, p, state, false)
    end

    # Force merge at the end
    if length(state.runs) > 1
        ($merge_collapse)($(args...), v, p, state, true)
    end

    v, p
end

($timsort)($(args...), v::AbstractVector, p::AbstractVector{Int}) = ($timsort)($(args...), v, p, 1, length(v))

end; end # quote; macro

@_jl_timsort_functions ""    :(isless($a,$b))
@_jl_timsort_functions "_r"  :(isless($b,$a))
@_jl_timsort_functions ""    :(lt($a,$b)) lt::Function
@_jl_timsort_functions "_by" :(isless(by($a),by($b))) by::Function

