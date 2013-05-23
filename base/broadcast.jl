module Broadcast

using ..Meta.quot
import Base.(.+), Base.(.-), Base.(.*), Base.(./), Base.(.\)
export broadcast, broadcast!, broadcast_function, broadcast!_function
export broadcast_getindex, broadcast_setindex!


## Broadcasting utilities ##

# Calculate the broadcast shape of the arguments, or error if incompatible
broadcast_shape() = ()
function broadcast_shape(As::AbstractArray...)
    nd = ndims(As[1])
    for i = 2:length(As)
        nd = max(nd, ndims(As[i]))
    end
    bshape = ones(Int, nd)
    for A in As
        for d = 1:ndims(A)
            n = size(A, d)
            if n != 1
                if bshape[d] == 1
                    bshape[d] = n
                elseif bshape[d] != n
                    error("arrays cannot be broadcast to a common size")
                end
            end
        end
    end
    return tuple(bshape...)
end

# Check that all arguments are broadcast compatible with shape
function check_broadcast_shape(shape::Dims, As::AbstractArray...)
    for A in As
        if ndims(A) > length(shape)
            error("cannot broadcast array to have fewer dimensions")
        end
        for k in 1:ndims(A)
            n, nA = shape[k], size(A, k)
            if n != nA != 1
                error("array cannot be broadcast to match destination")
            end
        end
    end
end

# Calculate strides as will be used by the generated inner loops
function calc_loop_strides(shape::Dims, As::AbstractArray...)
    # squeeze out singleton dimensions in shape
    dims = Array(Int, 0)
    loopshape = Array(Int, 0)
    nd = length(shape)
    sizehint(dims, nd)
    sizehint(loopshape, nd)
    for i = 1:nd
        s = shape[i]
        if s != 1
            push!(dims, i)
            push!(loopshape, s)
        end
    end
    nd = length(loopshape)

    strides = [(size(A, d) > 1 ? stride(A, d) : 0) for A in As, d in dims]
    # convert from regular strides to loop strides
    for k=(nd-1):-1:1, a=1:length(As)
        strides[a, k+1] -= strides[a, k]*loopshape[k]
    end

    tuple(loopshape...), strides
end

function broadcast_args(shape::Dims, As::(Array...))
    loopshape, strides = calc_loop_strides(shape, As...)
    (loopshape, As, ones(Int, length(As)), strides)
end
function broadcast_args(shape::Dims, As::(StridedArray...))
    loopshape, strides = calc_loop_strides(shape, As...)
    nA = length(As)
    offs = Array(Int, nA)
    baseAs = Array(Array, nA)
    for (k, A) in enumerate(As)
        offs[k],baseAs[k] = isa(A,SubArray) ? (A.first_index,A.parent) : (1,A)
    end
    (loopshape, tuple(baseAs...), offs, strides)
end


## Generation of inner loop instances ##

function code_inner_loop(fname::Symbol, extra_args::Vector, initial,
                         innermost::Function, narrays::Int, nd::Int)
    Asyms      = [gensym("A$a") for a=1:narrays]
    indsyms    = [gensym("k$a") for a=1:narrays]
    axissyms   = [gensym("i$d") for d=1:nd]
    sizesyms   = [gensym("n$d") for d=1:nd]
    stridesyms = [gensym("s$(a)_$d") for a=1:narrays, d=1:nd]
        
    loop = innermost([:($arr[$ind]) for (arr, ind) in zip(Asyms, indsyms)]...)
    for (d, (axis, n)) in enumerate(zip(axissyms, sizesyms))
        loop = :(
            for $axis=1:$n
                $loop
                $([:($ind += $(stridesyms[a, d]))
                   for (a, ind) in enumerate(indsyms)]...)
            end
        )
    end
    
    @gensym shape arrays offsets strides
    quote
        function $fname($shape::NTuple{$nd, Int},
                        $arrays::NTuple{$narrays, StridedArray},
                        $offsets::Vector{Int},
                        $strides::Matrix{Int}, $(extra_args...))
            @assert size($strides) == ($narrays, $nd)
            ($(sizesyms...),)   = $shape
            $([:(if $n==0; return; end) for n in sizesyms]...)
            ($(Asyms...), )     = $arrays
            ($(stridesyms...),) = $strides
            ($(indsyms...), )   = $offsets
            $initial
            $loop
        end
    end
end


## Generation of inner loop staged functions ##

function code_inner(fname::Symbol, extra_args::Vector, initial,
                    innermost::Function)
    quote
        function $fname(shape::(Int...), arrays::(StridedArray...), 
                        offsets::Vector{Int}, strides::Matrix{Int},
                        $(extra_args...))
            f = eval(code_inner_loop($(quot(fname)), $(quot(extra_args)),
                                     $(quot(initial)), $(quot(innermost)),
                                     length(arrays), length(shape)))
            f(shape, arrays, offsets, strides, $(extra_args...))
        end
    end
end

code_foreach_inner(fname::Symbol, extra_args::Vector, innermost::Function) =
    code_inner(fname, extra_args, quote end, innermost)

function code_map!_inner(fname::Symbol, dest, extra_args::Vector, 
                         innermost::Function)
    @gensym k
    code_inner(fname, {dest, extra_args...}, :($k=1),
        (els...)->quote
            $dest[$k] = $(innermost(:($dest[$k]), els...))
            $k += 1
        end)
end


## (Generation of) complete broadcast functions ##

function code_broadcast(fname::Symbol, op)
    inner! = gensym("$(fname)_inner!")
    innerdef = code_map!_inner(inner!, :(result::Array), [],
                               (dest, els...) -> :( $op($(els...)) ))
    quote
        $innerdef
        $fname() = $op()
        function $fname(As::StridedArray...)
            shape = broadcast_shape(As...)
            result = Array(promote_type([eltype(A) for A in As]...), shape)
            $inner!(broadcast_args(shape, As)..., result)
            result
        end        
    end
end

function code_broadcast!(fname::Symbol, op)
    inner! = gensym("$(fname)!_inner!")
    innerdef = code_foreach_inner(inner!, [],
                                  (dest, els...) -> :( $dest=$op($(els...)) ))
    quote
        $innerdef
        function $fname(dest::StridedArray, As::StridedArray...)
            shape = size(dest)
            check_broadcast_shape(shape, As...)
            $inner!(broadcast_args(shape, tuple(dest, As...))...)
            dest
        end        
    end
end

eval(code_map!_inner(:broadcast_getindex_inner!,
                     :(result::Array), [:(A::AbstractArray)],
                     (dest, inds...) -> :( A[$(inds...)] )))
function broadcast_getindex(A::AbstractArray,
                            ind1::StridedArray{Int},
                            inds::StridedArray{Int}...)
    inds = tuple(ind1, inds...)
    shape = broadcast_shape(inds...)
    result = Array(eltype(A), shape)
    broadcast_getindex_inner!(broadcast_args(shape, inds)..., result, A)
    result
end

eval(code_foreach_inner(:broadcast_setindex!_inner!, [:(A::AbstractArray)],
                        (x, inds...)->:( A[$(inds...)] = $x )))
function broadcast_setindex!(A::AbstractArray, X::StridedArray,
                             ind1::StridedArray{Int}, 
                             inds::StridedArray{Int}...)
    Xinds = tuple(X, ind1, inds...)
    shape = broadcast_shape(Xinds...)
    broadcast_setindex!_inner!(broadcast_args(shape, Xinds)..., A)
    Xinds[1]
end


## actual functions for broadcast and broadcast! ##

for (fname, op) in {(:.+, +), (:.-, -), (:.*, *), (:./, /), (:.\, \)}
    eval(code_broadcast(fname, quot(op)))
end

broadcastfuns = (Function=>Function)[]
function broadcast_function(op::Function)
    (haskey(broadcastfuns, op) ? broadcastfuns[op] :
        (broadcastfuns[op] = eval(code_broadcast(gensym("broadcast_$(op)"), 
                                                 quot(op)))))
end
broadcast(op::Function) = op()
broadcast(op::Function, As::StridedArray...) = broadcast_function(op)(As...)

broadcast!funs = (Function=>Function)[]
function broadcast!_function(op::Function)
    (haskey(broadcast!funs, op) ? broadcast!funs[op] :
        (broadcast!funs[op] = eval(code_broadcast!(gensym("broadcast!_$(op)"), 
                                                   quot(op)))))
end
function broadcast!(op::Function, dest::StridedArray, As::StridedArray...)
    broadcast!_function(op)(dest, As...)
end


end # module
