# automatically generated from files in doc/stdlib/ -- do not edit here

Any[

("Base","ndims","ndims(A) -> Integer

   Returns the number of dimensions of A

"),

("Base","size","size(A[, dim...])

   Returns a tuple containing the dimensions of A. Optionally you can
   specify the dimension(s) you want the length of, and get the length
   of that dimension, or a tuple of the lengths of dimensions you
   asked for.:

      julia> A = rand(2,3,4);

      julia> size(A, 2)
      3

      julia> size(A,3,2)
      (4,3)

"),

("Base","iseltype","iseltype(A, T)

   Tests whether A or its elements are of type T

"),

("Base","length","length(s)

   The number of characters in string \"s\".

"),

("Base","eachindex","eachindex(A...)

   Creates an iterable object for visiting each index of an
   AbstractArray \"A\" in an efficient manner. For array types that
   have opted into fast linear indexing (like \"Array\"), this is
   simply the range \"1:length(A)\". For other array types, this
   returns a specialized Cartesian range to efficiently index into the
   array with indices specified for every dimension. For other
   iterables, including strings and dictionaries, this returns an
   iterator object supporting arbitrary index types (e.g. unevenly
   spaced or non-integer indices).

   Example for a sparse 2-d array:

      julia> A = sprand(2, 3, 0.5)
      2x3 sparse matrix with 4 Float64 entries:
          [1, 1]  =  0.598888
          [1, 2]  =  0.0230247
          [1, 3]  =  0.486499
          [2, 3]  =  0.809041

      julia> for iter in eachindex(A)
                 @show iter.I_1, iter.I_2
                 @show A[iter]
             end
      (iter.I_1,iter.I_2) = (1,1)
      A[iter] = 0.5988881393454597
      (iter.I_1,iter.I_2) = (2,1)
      A[iter] = 0.0
      (iter.I_1,iter.I_2) = (1,2)
      A[iter] = 0.02302469881746183
      (iter.I_1,iter.I_2) = (2,2)
      A[iter] = 0.0
      (iter.I_1,iter.I_2) = (1,3)
      A[iter] = 0.4864987874354343
      (iter.I_1,iter.I_2) = (2,3)
      A[iter] = 0.8090413606455655

"),

("Base","Base","Base.linearindexing(A)

   \"linearindexing\" defines how an AbstractArray most efficiently
   accesses its elements.  If \"Base.linearindexing(A)\" returns
   \"Base.LinearFast()\", this means that linear indexing with only
   one index is an efficient operation.  If it instead returns
   \"Base.LinearSlow()\" (by default), this means that the array
   intrinsically accesses its elements with indices specified for
   every dimension.  Since converting a linear index to multiple
   indexing subscripts is typically very expensive, this provides a
   traits-based mechanism to enable efficient generic code for all
   array types.

   An abstract array subtype \"MyArray\" that wishes to opt into fast
   linear indexing behaviors should define \"linearindexing\" in the
   type-domain:

      Base.linearindexing{T<:MyArray}(::Type{T}) = Base.LinearFast()

"),

("Base","countnz","countnz(A)

   Counts the number of nonzero values in array A (dense or sparse).
   Note that this is not a constant-time operation. For sparse
   matrices, one should usually use \"nnz\", which returns the number
   of stored values.

"),

("Base","conj!","conj!(A)

   Convert an array to its complex conjugate in-place

"),

("Base","stride","stride(A, k)

   Returns the distance in memory (in number of elements) between
   adjacent elements in dimension k

"),

("Base","strides","strides(A)

   Returns a tuple of the memory strides in each dimension

"),

("Base","ind2sub","ind2sub(a, index) -> subscripts

   Returns a tuple of subscripts into array \"a\" corresponding to the
   linear index \"index\"

"),

("Base","ind2sub","ind2sub(a, index) -> subscripts

   Returns a tuple of subscripts into array \"a\" corresponding to the
   linear index \"index\"

"),

("Base","sub2ind","sub2ind(dims, i, j, k...) -> index

   The inverse of \"ind2sub\", returns the linear index corresponding
   to the provided subscripts

"),

("Base","Array","Array(dims)

   \"Array{T}(dims)\" constructs an uninitialized dense array with
   element type \"T\". \"dims\" may be a tuple or a series of integer
   arguments. The syntax \"Array(T, dims)\" is also available, but
   deprecated.

"),

("Base","getindex","getindex(collection, key...)

   Retrieve the value(s) stored at the given key or index within a
   collection. The syntax \"a[i,j,...]\" is converted by the compiler
   to \"getindex(a, i, j, ...)\".

"),

("Base","cell","cell(dims)

   Construct an uninitialized cell array (heterogeneous array).
   \"dims\" can be either a tuple or a series of integer arguments.

"),

("Base","zeros","zeros(A)

   Create an array of all zeros with the same element type and shape
   as A.

"),

("Base","zeros","zeros(A)

   Create an array of all zeros with the same element type and shape
   as A.

"),

("Base","ones","ones(A)

   Create an array of all ones with the same element type and shape as
   A.

"),

("Base","ones","ones(A)

   Create an array of all ones with the same element type and shape as
   A.

"),

("Base","trues","trues(dims)

   Create a \"BitArray\" with all values set to true

"),

("Base","falses","falses(dims)

   Create a \"BitArray\" with all values set to false

"),

("Base","fill","fill(x, dims)

   Create an array filled with the value \"x\". For example,
   \"fill(1.0, (10,10))\" returns a  10x10 array of floats, with each
   element initialized to 1.0.

   If \"x\" is an object reference, all elements will refer to the
   same object. \"fill(Foo(), dims)\" will return an array filled with
   the result of evaluating \"Foo()\" once.

"),

("Base","fill!","fill!(A, x)

   Fill array \"A\" with the value \"x\". If \"x\" is an object
   reference, all elements will refer to the same object. \"fill!(A,
   Foo())\" will return \"A\" filled with the result of evaluating
   \"Foo()\" once.

"),

("Base","reshape","reshape(A, dims)

   Create an array with the same data as the given array, but with
   different dimensions. An implementation for a particular type of
   array may choose whether the data is copied or shared.

"),

("Base","similar","similar(array, element_type, dims)

   Create an uninitialized array of the same type as the given array,
   but with the specified element type and dimensions. The second and
   third arguments are both optional. The \"dims\" argument may be a
   tuple or a series of integer arguments. For some special
   \"AbstractArray\" objects which are not real containers (like
   ranges), this function returns a standard \"Array\" to allow
   operating on elements.

"),

("Base","reinterpret","reinterpret(type, A)

   Change the type-interpretation of a block of memory. For example,
   \"reinterpret(Float32, UInt32(7))\" interprets the 4 bytes
   corresponding to \"UInt32(7)\" as a \"Float32\". For arrays, this
   constructs an array with the same binary data as the given array,
   but with the specified element type.

"),

("Base","eye","eye(A)

   Constructs an identity matrix of the same dimensions and type as
   \"A\".

"),

("Base","eye","eye(A)

   Constructs an identity matrix of the same dimensions and type as
   \"A\".

"),

("Base","eye","eye(A)

   Constructs an identity matrix of the same dimensions and type as
   \"A\".

"),

("Base","linspace","linspace(start, stop, n=100)

   Construct a range of \"n\" linearly spaced elements from \"start\"
   to \"stop\".

"),

("Base","logspace","logspace(start, stop, n=50)

   Construct a vector of \"n\" logarithmically spaced numbers from
   \"10^start\" to \"10^stop\".

"),

("Base","broadcast","broadcast(f, As...)

   Broadcasts the arrays \"As\" to a common size by expanding
   singleton dimensions, and returns an array of the results
   \"f(as...)\" for each position.

"),

("Base","broadcast!","broadcast!(f, dest, As...)

   Like \"broadcast\", but store the result of \"broadcast(f, As...)\"
   in the \"dest\" array. Note that \"dest\" is only used to store the
   result, and does not supply arguments to \"f\" unless it is also
   listed in the \"As\", as in \"broadcast!(f, A, A, B)\" to perform
   \"A[:] = broadcast(f, A, B)\".

"),

("Base","bitbroadcast","bitbroadcast(f, As...)

   Like \"broadcast\", but allocates a \"BitArray\" to store the
   result, rather then an \"Array\".

"),

("Base","broadcast_function","broadcast_function(f)

   Returns a function \"broadcast_f\" such that
   \"broadcast_function(f)(As...) === broadcast(f, As...)\". Most
   useful in the form \"const broadcast_f = broadcast_function(f)\".

"),

("Base","broadcast!_function","broadcast!_function(f)

   Like \"broadcast_function\", but for \"broadcast!\".

"),

("Base","getindex","getindex(collection, key...)

   Retrieve the value(s) stored at the given key or index within a
   collection. The syntax \"a[i,j,...]\" is converted by the compiler
   to \"getindex(a, i, j, ...)\".

"),

("Base","sub","sub(A, inds...)

   Like \"getindex()\", but returns a view into the parent array \"A\"
   with the given indices instead of making a copy.  Calling
   \"getindex()\" or \"setindex!()\" on the returned \"SubArray\"
   computes the indices to the parent array on the fly without
   checking bounds.

"),

("Base","parent","parent(A)

   Returns the \"parent array\" of an array view type (e.g.,
   SubArray), or the array itself if it is not a view

"),

("Base","parentindexes","parentindexes(A)

   From an array view \"A\", returns the corresponding indexes in the
   parent

"),

("Base","slicedim","slicedim(A, d, i)

   Return all the data of \"A\" where the index for dimension \"d\"
   equals \"i\". Equivalent to \"A[:,:,...,i,:,:,...]\" where \"i\" is
   in position \"d\".

"),

("Base","slice","slice(A, inds...)

   Returns a view of array \"A\" with the given indices like
   \"sub()\", but drops all dimensions indexed with scalars.

"),

("Base","setindex!","setindex!(collection, value, key...)

   Store the given value at the given key or index within a
   collection. The syntax \"a[i,j,...] = x\" is converted by the
   compiler to \"setindex!(a, x, i, j, ...)\".

"),

("Base","broadcast_getindex","broadcast_getindex(A, inds...)

   Broadcasts the \"inds\" arrays to a common size like \"broadcast\",
   and returns an array of the results \"A[ks...]\", where \"ks\" goes
   over the positions in the broadcast.

"),

("Base","broadcast_setindex!","broadcast_setindex!(A, X, inds...)

   Broadcasts the \"X\" and \"inds\" arrays to a common size and
   stores the value from each position in \"X\" at the indices given
   by the same positions in \"inds\".

"),

("Base","cat","cat(dims, A...)

   Concatenate the input arrays along the specified dimensions in the
   iterable \"dims\". For dimensions not in \"dims\", all input arrays
   should have the same size, which will also be the size of the
   output array along that dimension. For dimensions in \"dims\", the
   size of the output array is the sum of the sizes of the input
   arrays along that dimension. If \"dims\" is a single number, the
   different arrays are tightly stacked along that dimension. If
   \"dims\" is an iterable containing several dimensions, this allows
   to construct block diagonal matrices and their higher-dimensional
   analogues by simultaneously increasing several dimensions for every
   new input array and putting zero blocks elsewhere. For example,
   *cat([1,2], matrices...)* builds a block diagonal matrix, i.e. a
   block matrix with *matrices[1]*, *matrices[2]*, ... as diagonal
   blocks and matching zero blocks away from the diagonal.

"),

("Base","vcat","vcat(A...)

   Concatenate along dimension 1

"),

("Base","hcat","hcat(A...)

   Concatenate along dimension 2

"),

("Base","hvcat","hvcat(rows::Tuple{Vararg{Int}}, values...)

   Horizontal and vertical concatenation in one call. This function is
   called for block matrix syntax. The first argument specifies the
   number of arguments to concatenate in each block row. For example,
   \"[a b;c d e]\" calls \"hvcat((2,3),a,b,c,d,e)\".

   If the first argument is a single integer \"n\", then all block
   rows are assumed to have \"n\" block columns.

"),

("Base","flipdim","flipdim(A, d)

   Reverse \"A\" in dimension \"d\".

"),

("Base","circshift","circshift(A, shifts)

   Circularly shift the data in an array. The second argument is a
   vector giving the amount to shift in each dimension.

"),

("Base","find","find(f, A)

   Return a vector of the linear indexes of  \"A\" where \"f\" returns
   true.

"),

("Base","find","find(f, A)

   Return a vector of the linear indexes of  \"A\" where \"f\" returns
   true.

"),

("Base","findn","findn(A)

   Return a vector of indexes for each dimension giving the locations
   of the non-zeros in \"A\" (determined by \"A[i]!=0\").

"),

("Base","findnz","findnz(A)

   Return a tuple \"(I, J, V)\" where \"I\" and \"J\" are the row and
   column indexes of the non-zero values in matrix \"A\", and \"V\" is
   a vector of the non-zero values.

"),

("Base","findfirst","findfirst(predicate, A)

   Return the index of the first element of \"A\" for which
   \"predicate\" returns true.

"),

("Base","findfirst","findfirst(predicate, A)

   Return the index of the first element of \"A\" for which
   \"predicate\" returns true.

"),

("Base","findfirst","findfirst(predicate, A)

   Return the index of the first element of \"A\" for which
   \"predicate\" returns true.

"),

("Base","findlast","findlast(predicate, A)

   Return the index of the last element of \"A\" for which
   \"predicate\" returns true.

"),

("Base","findlast","findlast(predicate, A)

   Return the index of the last element of \"A\" for which
   \"predicate\" returns true.

"),

("Base","findlast","findlast(predicate, A)

   Return the index of the last element of \"A\" for which
   \"predicate\" returns true.

"),

("Base","findnext","findnext(A, v, i)

   Find the next index >= \"i\" of an element of \"A\" equal to \"v\"
   (using \"==\"), or \"0\" if not found.

"),

("Base","findnext","findnext(A, v, i)

   Find the next index >= \"i\" of an element of \"A\" equal to \"v\"
   (using \"==\"), or \"0\" if not found.

"),

("Base","findnext","findnext(A, v, i)

   Find the next index >= \"i\" of an element of \"A\" equal to \"v\"
   (using \"==\"), or \"0\" if not found.

"),

("Base","findprev","findprev(A, v, i)

   Find the previous index <= \"i\" of an element of \"A\" equal to
   \"v\" (using \"==\"), or \"0\" if not found.

"),

("Base","findprev","findprev(A, v, i)

   Find the previous index <= \"i\" of an element of \"A\" equal to
   \"v\" (using \"==\"), or \"0\" if not found.

"),

("Base","findprev","findprev(A, v, i)

   Find the previous index <= \"i\" of an element of \"A\" equal to
   \"v\" (using \"==\"), or \"0\" if not found.

"),

("Base","permutedims","permutedims(A, perm)

   Permute the dimensions of array \"A\". \"perm\" is a vector
   specifying a permutation of length \"ndims(A)\". This is a
   generalization of transpose for multi-dimensional arrays. Transpose
   is equivalent to \"permutedims(A, [2,1])\".

"),

("Base","ipermutedims","ipermutedims(A, perm)

   Like \"permutedims()\", except the inverse of the given permutation
   is applied.

"),

("Base","permutedims!","permutedims!(dest, src, perm)

   Permute the dimensions of array \"src\" and store the result in the
   array \"dest\". \"perm\" is a vector specifying a permutation of
   length \"ndims(src)\". The preallocated array \"dest\" should have
   \"size(dest) == size(src)[perm]\" and is completely overwritten. No
   in-place permutation is supported and unexpected results will
   happen if *src* and *dest* have overlapping memory regions.

"),

("Base","squeeze","squeeze(A, dims)

   Remove the dimensions specified by \"dims\" from array \"A\".
   Elements of \"dims\" must be unique and within the range
   \"1:ndims(A)\".

"),

("Base","vec","vec(Array) -> Vector

   Vectorize an array using column-major convention.

"),

("Base","promote_shape","promote_shape(s1, s2)

   Check two array shapes for compatibility, allowing trailing
   singleton dimensions, and return whichever shape has more
   dimensions.

"),

("Base","checkbounds","checkbounds(array, indexes...)

   Throw an error if the specified indexes are not in bounds for the
   given array.

"),

("Base","randsubseq","randsubseq(A, p) -> Vector

   Return a vector consisting of a random subsequence of the given
   array \"A\", where each element of \"A\" is included (in order)
   with independent probability \"p\".   (Complexity is linear in
   \"p*length(A)\", so this function is efficient even if \"p\" is
   small and \"A\" is large.)  Technically, this process is known as
   \"Bernoulli sampling\" of \"A\".

"),

("Base","randsubseq!","randsubseq!(S, A, p)

   Like \"randsubseq\", but the results are stored in \"S\" (which is
   resized as needed).

"),

("Base","cumprod","cumprod(A[, dim])

   Cumulative product along a dimension \"dim\" (defaults to 1). See
   also \"cumprod!()\" to use a preallocated output array, both for
   performance and to control the precision of the output (e.g. to
   avoid overflow).

"),

("Base","cumprod!","cumprod!(B, A[, dim])

   Cumulative product of \"A\" along a dimension, storing the result
   in \"B\". The dimension defaults to 1.

"),

("Base","cumsum","cumsum(A[, dim])

   Cumulative sum along a dimension \"dim\" (defaults to 1). See also
   \"cumsum!()\" to use a preallocated output array, both for
   performance and to control the precision of the output (e.g. to
   avoid overflow).

"),

("Base","cumsum!","cumsum!(B, A[, dim])

   Cumulative sum of \"A\" along a dimension, storing the result in
   \"B\". The dimension defaults to 1.

"),

("Base","cumsum_kbn","cumsum_kbn(A[, dim])

   Cumulative sum along a dimension, using the Kahan-Babuska-Neumaier
   compensated summation algorithm for additional accuracy. The
   dimension defaults to 1.

"),

("Base","cummin","cummin(A[, dim])

   Cumulative minimum along a dimension. The dimension defaults to 1.

"),

("Base","cummax","cummax(A[, dim])

   Cumulative maximum along a dimension. The dimension defaults to 1.

"),

("Base","diff","diff(A[, dim])

   Finite difference operator of matrix or vector.

"),

("Base","gradient","gradient(F[, h])

   Compute differences along vector \"F\", using \"h\" as the spacing
   between points. The default spacing is one.

"),

("Base","rot180","rot180(A, k)

   Rotate matrix \"A\" 180 degrees an integer \"k\" number of times.
   If \"k\" is even, this is equivalent to a \"copy\".

"),

("Base","rot180","rot180(A, k)

   Rotate matrix \"A\" 180 degrees an integer \"k\" number of times.
   If \"k\" is even, this is equivalent to a \"copy\".

"),

("Base","rotl90","rotl90(A, k)

   Rotate matrix \"A\" left 90 degrees an integer \"k\" number of
   times. If \"k\" is zero or a multiple of four, this is equivalent
   to a \"copy\".

"),

("Base","rotl90","rotl90(A, k)

   Rotate matrix \"A\" left 90 degrees an integer \"k\" number of
   times. If \"k\" is zero or a multiple of four, this is equivalent
   to a \"copy\".

"),

("Base","rotr90","rotr90(A, k)

   Rotate matrix \"A\" right 90 degrees an integer \"k\" number of
   times. If \"k\" is zero or a multiple of four, this is equivalent
   to a \"copy\".

"),

("Base","rotr90","rotr90(A, k)

   Rotate matrix \"A\" right 90 degrees an integer \"k\" number of
   times. If \"k\" is zero or a multiple of four, this is equivalent
   to a \"copy\".

"),

("Base","reducedim","reducedim(f, A, dims[, initial])

   Reduce 2-argument function \"f\" along dimensions of \"A\".
   \"dims\" is a vector specifying the dimensions to reduce, and
   \"initial\" is the initial value to use in the reductions. For *+*,
   >>**<<*, *max* and *min* the *initial* argument is optional.

   The associativity of the reduction is implementation-dependent; if
   you need a particular associativity, e.g. left-to-right, you should
   write your own loop. See documentation for \"reduce\".

"),

("Base","mapreducedim","mapreducedim(f, op, A, dims[, initial])

   Evaluates to the same as *reducedim(op, map(f, A), dims,
   f(initial))*, but is generally faster because the intermediate
   array is avoided.

"),

("Base","mapslices","mapslices(f, A, dims)

   Transform the given dimensions of array \"A\" using function \"f\".
   \"f\" is called on each slice of \"A\" of the form
   \"A[...,:,...,:,...]\". \"dims\" is an integer vector specifying
   where the colons go in this expression. The results are
   concatenated along the remaining dimensions. For example, if
   \"dims\" is \"[1,2]\" and A is 4-dimensional, \"f\" is called on
   \"A[:,:,i,j]\" for all \"i\" and \"j\".

"),

("Base","sum_kbn","sum_kbn(A)

   Returns the sum of all array elements, using the Kahan-Babuska-
   Neumaier compensated summation algorithm for additional accuracy.

"),

("Base","cartesianmap","cartesianmap(f, dims)

   Given a \"dims\" tuple of integers \"(m, n, ...)\", call \"f\" on
   all combinations of integers in the ranges \"1:m\", \"1:n\", etc.

      julia> cartesianmap(println, (2,2))
      11
      21
      12
      22

"),

("Base","nthperm","nthperm(p)

   Return the \"k\" that generated permutation \"p\". Note that
   \"nthperm(nthperm([1:n], k)) == k\" for \"1 <= k <= factorial(n)\".

"),

("Base","nthperm","nthperm(p)

   Return the \"k\" that generated permutation \"p\". Note that
   \"nthperm(nthperm([1:n], k)) == k\" for \"1 <= k <= factorial(n)\".

"),

("Base","nthperm!","nthperm!(v, k)

   In-place version of \"nthperm()\".

"),

("Base","randperm","randperm([rng], n)

   Construct a random permutation of length \"n\". The optional
   \"rng\" argument specifies a random number generator, see *Random
   Numbers*.

"),

("Base","invperm","invperm(v)

   Return the inverse permutation of v.

"),

("Base","isperm","isperm(v) -> Bool

   Returns true if v is a valid permutation.

"),

("Base","permute!","permute!(v, p)

   Permute vector \"v\" in-place, according to permutation \"p\".  No
   checking is done to verify that \"p\" is a permutation.

   To return a new permutation, use \"v[p]\".  Note that this is
   generally faster than \"permute!(v,p)\" for large vectors.

"),

("Base","ipermute!","ipermute!(v, p)

   Like permute!, but the inverse of the given permutation is applied.

"),

("Base","randcycle","randcycle([rng], n)

   Construct a random cyclic permutation of length \"n\". The optional
   \"rng\" argument specifies a random number generator, see *Random
   Numbers*.

"),

("Base","shuffle","shuffle([rng], v)

   Return a randomly permuted copy of \"v\". The optional \"rng\"
   argument specifies a random number generator, see *Random Numbers*.

"),

("Base","shuffle!","shuffle!([rng], v)

   In-place version of \"shuffle()\".

"),

("Base","reverse","reverse(v[, start=1[, stop=length(v)]])

   Return a copy of \"v\" reversed from start to stop.

"),

("Base","reverseind","reverseind(v, i)

   Given an index \"i\" in \"reverse(v)\", return the corresponding
   index in \"v\" so that \"v[reverseind(v,i)] == reverse(v)[i]\".
   (This can be nontrivial in the case where \"v\" is a Unicode
   string.)

"),

("Base","reverse!","reverse!(v[, start=1[, stop=length(v)]]) -> v

   In-place version of \"reverse()\".

"),

("Base","combinations","combinations(array, n)

   Generate all combinations of \"n\" elements from an indexable
   object.  Because the number of combinations can be very large, this
   function returns an iterator object. Use
   \"collect(combinations(array,n))\" to get an array of all
   combinations.

"),

("Base","permutations","permutations(array)

   Generate all permutations of an indexable object.  Because the
   number of permutations can be very large, this function returns an
   iterator object. Use \"collect(permutations(array))\" to get an
   array of all permutations.

"),

("Base","partitions","partitions(array, m)

   Generate all set partitions of the elements of an array into
   exactly m subsets, represented as arrays of arrays. Because the
   number of partitions can be very large, this function returns an
   iterator object. Use \"collect(partitions(array,m))\" to get an
   array of all partitions. The number of partitions into m subsets is
   equal to the Stirling number of the second kind and can be
   efficiently computed using \"length(partitions(array,m))\".

"),

("Base","partitions","partitions(array, m)

   Generate all set partitions of the elements of an array into
   exactly m subsets, represented as arrays of arrays. Because the
   number of partitions can be very large, this function returns an
   iterator object. Use \"collect(partitions(array,m))\" to get an
   array of all partitions. The number of partitions into m subsets is
   equal to the Stirling number of the second kind and can be
   efficiently computed using \"length(partitions(array,m))\".

"),

("Base","partitions","partitions(array, m)

   Generate all set partitions of the elements of an array into
   exactly m subsets, represented as arrays of arrays. Because the
   number of partitions can be very large, this function returns an
   iterator object. Use \"collect(partitions(array,m))\" to get an
   array of all partitions. The number of partitions into m subsets is
   equal to the Stirling number of the second kind and can be
   efficiently computed using \"length(partitions(array,m))\".

"),

("Base","partitions","partitions(array, m)

   Generate all set partitions of the elements of an array into
   exactly m subsets, represented as arrays of arrays. Because the
   number of partitions can be very large, this function returns an
   iterator object. Use \"collect(partitions(array,m))\" to get an
   array of all partitions. The number of partitions into m subsets is
   equal to the Stirling number of the second kind and can be
   efficiently computed using \"length(partitions(array,m))\".

"),

("Base","bitpack","bitpack(A::AbstractArray{T, N}) -> BitArray

   Converts a numeric array to a packed boolean array

"),

("Base","bitunpack","bitunpack(B::BitArray{N}) -> Array{Bool,N}

   Converts a packed boolean array to an array of booleans

"),

("Base","flipbits!","flipbits!(B::BitArray{N}) -> BitArray{N}

   Performs a bitwise not operation on B. See *~ operator*.

"),

("Base","rol!","rol!(B::BitArray{1}, i::Integer) -> BitArray{1}

   Performs a left rotation operation on B.

"),

("Base","rol!","rol!(B::BitArray{1}, i::Integer) -> BitArray{1}

   Performs a left rotation operation on B.

"),

("Base","rol","rol(B::BitArray{1}, i::Integer) -> BitArray{1}

   Performs a left rotation operation.

"),

("Base","ror!","ror!(B::BitArray{1}, i::Integer) -> BitArray{1}

   Performs a right rotation operation on B.

"),

("Base","ror!","ror!(B::BitArray{1}, i::Integer) -> BitArray{1}

   Performs a right rotation operation on B.

"),

("Base","ror","ror(B::BitArray{1}, i::Integer) -> BitArray{1}

   Performs a right rotation operation.

"),

("Base","sparse","sparse(A)

   Convert an AbstractMatrix \"A\" into a sparse matrix.

"),

("Base","sparsevec","sparsevec(A)

   Convert a dense vector \"A\" into a sparse matrix of size \"m x
   1\". In julia, sparse vectors are really just sparse matrices with
   one column.

"),

("Base","sparsevec","sparsevec(A)

   Convert a dense vector \"A\" into a sparse matrix of size \"m x
   1\". In julia, sparse vectors are really just sparse matrices with
   one column.

"),

("Base","issparse","issparse(S)

   Returns \"true\" if \"S\" is sparse, and \"false\" otherwise.

"),

("Base","sparse","sparse(A)

   Convert an AbstractMatrix \"A\" into a sparse matrix.

"),

("Base","sparsevec","sparsevec(A)

   Convert a dense vector \"A\" into a sparse matrix of size \"m x
   1\". In julia, sparse vectors are really just sparse matrices with
   one column.

"),

("Base","full","full(QRCompactWYQ[, thin=true]) -> Matrix

   Converts an orthogonal or unitary matrix stored as a
   \"QRCompactWYQ\" object, i.e. in the compact WY format
   [Bischof1987], to a dense matrix.

   Optionally takes a \"thin\" Boolean argument, which if \"true\"
   omits the columns that span the rows of \"R\" in the QR
   factorization that are zero. The resulting matrix is the \"Q\" in a
   thin QR factorization (sometimes called the reduced QR
   factorization).  If \"false\", returns a \"Q\" that spans all rows
   of \"R\" in its corresponding QR factorization.

"),

("Base","nnz","nnz(A)

   Returns the number of stored (filled) elements in a sparse matrix.

"),

("Base","spzeros","spzeros(m, n)

   Create a sparse matrix of size \"m x n\". This sparse matrix will
   not contain any nonzero values. No storage will be allocated for
   nonzero values during construction.

"),

("Base","spones","spones(S)

   Create a sparse matrix with the same structure as that of \"S\",
   but with every nonzero element having the value \"1.0\".

"),

("Base","speye","speye(type, m[, n])

   Create a sparse identity matrix of specified type of size \"m x
   m\". In case \"n\" is supplied, create a sparse identity matrix of
   size \"m x n\".

"),

("Base","spdiagm","spdiagm(B, d[, m, n])

   Construct a sparse diagonal matrix. \"B\" is a tuple of vectors
   containing the diagonals and \"d\" is a tuple containing the
   positions of the diagonals. In the case the input contains only one
   diagonaly, \"B\" can be a vector (instead of a tuple) and \"d\" can
   be the diagonal position (instead of a tuple), defaulting to 0
   (diagonal). Optionally, \"m\" and \"n\" specify the size of the
   resulting sparse matrix.

"),

("Base","sprand","sprand([rng], m, n, p[, rfn])

   Create a random \"m\" by \"n\" sparse matrix, in which the
   probability of any element being nonzero is independently given by
   \"p\" (and hence the mean density of nonzeros is also exactly
   \"p\"). Nonzero values are sampled from the distribution specified
   by \"rfn\". The uniform distribution is used in case \"rfn\" is not
   specified. The optional \"rng\" argument specifies a random number
   generator, see *Random Numbers*.

"),

("Base","sprandn","sprandn(m, n, p)

   Create a random \"m\" by \"n\" sparse matrix with the specified
   (independent) probability \"p\" of any entry being nonzero, where
   nonzero values are sampled from the normal distribution.

"),

("Base","sprandbool","sprandbool(m, n, p)

   Create a random \"m\" by \"n\" sparse boolean matrix with the
   specified (independent) probability \"p\" of any entry being
   \"true\".

"),

("Base","etree","etree(A[, post])

   Compute the elimination tree of a symmetric sparse matrix \"A\"
   from \"triu(A)\" and, optionally, its post-ordering permutation.

"),

("Base","symperm","symperm(A, p)

   Return the symmetric permutation of A, which is \"A[p,p]\". A
   should be symmetric and sparse, where only the upper triangular
   part of the matrix is stored. This algorithm ignores the lower
   triangular part of the matrix. Only the upper triangular part of
   the result is returned as well.

"),

("Base","nonzeros","nonzeros(A)

   Return a vector of the structural nonzero values in sparse matrix
   \"A\". This includes zeros that are explicitly stored in the sparse
   matrix. The returned vector points directly to the internal nonzero
   storage of \"A\", and any modifications to the returned vector will
   mutate \"A\" as well. See \"rowvals(A)\" and \"nzrange(A, col)\".

"),

("Base","rowvals","rowvals(A)

   Return a vector of the row indices of \"A\", and any modifications
   to the returned vector will mutate \"A\" as well. Given the
   internal storage format of sparse matrices, providing access to how
   the row indices are stored internally can be useful in conjuction
   with iterating over structural nonzero values. See \"nonzeros(A)\"
   and \"nzrange(A, col)\".

"),

("Base","nzrange","nzrange(A, col)

   Return the range of indices to the structural nonzero values of a
   sparse matrix column. In conjunction with \"nonzeros(A)\" and
   \"rowvals(A)\", this allows for convenient iterating over a sparse
   matrix

      A = sparse(I,J,V)
      rows = rowvals(A)
      vals = nonzeros(A)
      m, n = size(A)
      for i = 1:n
         for j in nzrange(A, i)
            row = rows[j]
            val = vals[j]
            # perform sparse wizardry...
         end
      end

"),

("Base","exit","exit([code])

   Quit (or control-D at the prompt). The default exit code is zero,
   indicating that the processes completed successfully.

"),

("Base","quit","quit()

   Quit the program indicating that the processes completed
   successfully. This function calls \"exit(0)\" (see \"exit()\").

"),

("Base","atexit","atexit(f)

   Register a zero-argument function to be called at exit.

"),

("Base","atreplinit","atreplinit(f)

   Register a one-argument function to be called before the REPL
   interface is initialized in interactive sessions; this is useful to
   customize the interface. The argument of \"f\" is the REPL object.
   This function should be called from within the \".juliarc.jl\"
   initialization file.

"),

("Base","isinteractive","isinteractive() -> Bool

   Determine whether Julia is running an interactive session.

"),

("Base","whos","whos([Module,] [pattern::Regex])

   Print information about exported global variables in a module,
   optionally restricted to those matching \"pattern\".

"),

("Base","edit","edit(function[, types])

   Edit the definition of a function, optionally specifying a tuple of
   types to indicate which method to edit.

"),

("Base","edit","edit(function[, types])

   Edit the definition of a function, optionally specifying a tuple of
   types to indicate which method to edit.

"),

("Base","@edit","@edit()

   Evaluates the arguments to the function call, determines their
   types, and calls the \"edit\" function on the resulting expression

"),

("Base","less","less(function[, types])

   Show the definition of a function using the default pager,
   optionally specifying a tuple of types to indicate which method to
   see.

"),

("Base","less","less(function[, types])

   Show the definition of a function using the default pager,
   optionally specifying a tuple of types to indicate which method to
   see.

"),

("Base","@less","@less()

   Evaluates the arguments to the function call, determines their
   types, and calls the \"less\" function on the resulting expression

"),

("Base","clipboard","clipboard() -> AbstractString

   Return a string with the contents of the operating system clipboard
   (\"paste\").

"),

("Base","clipboard","clipboard() -> AbstractString

   Return a string with the contents of the operating system clipboard
   (\"paste\").

"),

("Base","require","require(file::AbstractString...)

   Load source files once, in the context of the \"Main\" module, on
   every active node, searching standard locations for files.
   \"require\" is considered a top-level operation, so it sets the
   current \"include\" path but does not use it to search for files
   (see help for \"include\"). This function is typically used to load
   library code, and is implicitly called by \"using\" to load
   packages.

   When searching for files, \"require\" first looks in the current
   working directory, then looks for package code under \"Pkg.dir()\",
   then tries paths in the global array \"LOAD_PATH\".

"),

("Base","reload","reload(file::AbstractString)

   Like \"require\", except forces loading of files regardless of
   whether they have been loaded before. Typically used when
   interactively developing libraries.

"),

("Base","include","include(\"file.jl\")

   Evaluate the contents of a source file in the current context.
   During including, a task-local include path is set to the directory
   containing the file. Nested calls to \"include\" will search
   relative to that path. All paths refer to files on node 1 when
   running in parallel, and files will be fetched from node 1. This
   function is typically used to load source interactively, or to
   combine files in packages that are broken into multiple source
   files.

"),

("Base","include_string","include_string(code::AbstractString)

   Like \"include\", except reads code from the given string rather
   than from a file. Since there is no file path involved, no path
   processing or fetching from node 1 is done.

"),

("Base","help","help(name)

   Get help for a function. \"name\" can be an object or a string.

"),

("Base","apropos","apropos(string)

   Search documentation for functions related to \"string\".

"),

("Base","which","which(symbol)

   Return the module in which the binding for the variable referenced
   by \"symbol\" was created.

"),

("Base","which","which(symbol)

   Return the module in which the binding for the variable referenced
   by \"symbol\" was created.

"),

("Base","@which","@which()

   Applied to a function call, it evaluates the arguments to the
   specified function call, and returns the \"Method\" object for the
   method that would be called for those arguments.  Applied to a
   variable, it returns the module in which the variable was bound. It
   calls out to the \"which\" function.

"),

("Base","methods","methods(f[, types])

   Returns the method table for \"f\".

   If \"types\" is specified, returns an array of methods whose types
   match.

"),

("Base","methodswith","methodswith(typ[, module or function][, showparents])

   Return an array of methods with an argument of type \"typ\". If
   optional \"showparents\" is \"true\", also return arguments with a
   parent type of \"typ\", excluding type \"Any\".

   The optional second argument restricts the search to a particular
   module or function.

"),

("Base","@show","@show()

   Show an expression and result, returning the result

"),

("Base","versioninfo","versioninfo([verbose::Bool])

   Print information about the version of Julia in use. If the
   \"verbose\" argument is true, detailed system information is shown
   as well.

"),

("Base","workspace","workspace()

   Replace the top-level module (\"Main\") with a new one, providing a
   clean workspace. The previous \"Main\" module is made available as
   \"LastMain\". A previously-loaded package can be accessed using a
   statement such as \"using LastMain.Package\".

   This function should only be used interactively.

"),

("Base","ans","ans

   A variable referring to the last computed value, automatically set
   at the interactive prompt.

"),

("Base","===","===(x, y)

   ≡(x, y)

   See the \"is()\" operator

"),

("Base","isa","isa(x, type) -> Bool

   Determine whether \"x\" is of the given \"type\".

"),

("Base","isequal","isequal(x, y)

   Similar to \"==\", except treats all floating-point \"NaN\" values
   as equal to each other, and treats \"-0.0\" as unequal to \"0.0\".
   The default implementation of \"isequal\" calls \"==\", so if you
   have a type that doesn't have these floating-point subtleties then
   you probably only need to define \"==\".

   \"isequal\" is the comparison function used by hash tables
   (\"Dict\"). \"isequal(x,y)\" must imply that \"hash(x) ==
   hash(y)\".

   This typically means that if you define your own \"==\" function
   then you must define a corresponding \"hash\" (and vice versa).
   Collections typically implement \"isequal\" by calling \"isequal\"
   recursively on all contents.

   Scalar types generally do not need to implement \"isequal\"
   separate from \"==\", unless they represent floating-point numbers
   amenable to a more efficient implementation than that provided as a
   generic fallback (based on \"isnan\", \"signbit\", and \"==\").

"),

("Base","isless","isless(x, y)

   Test whether \"x\" is less than \"y\", according to a canonical
   total order. Values that are normally unordered, such as \"NaN\",
   are ordered in an arbitrary but consistent fashion. This is the
   default comparison used by \"sort\". Non-numeric types with a
   canonical total order should implement this function. Numeric types
   only need to implement it if they have special values such as
   \"NaN\".

"),

("Base","ifelse","ifelse(condition::Bool, x, y)

   Return \"x\" if \"condition\" is true, otherwise return \"y\". This
   differs from \"?\" or \"if\" in that it is an ordinary function, so
   all the arguments are evaluated first. In some cases, using
   \"ifelse\" instead of an \"if\" statement can eliminate the branch
   in generated code and provide higher performance in tight loops.

"),

("Base","lexcmp","lexcmp(x, y)

   Compare \"x\" and \"y\" lexicographically and return -1, 0, or 1
   depending on whether \"x\" is less than, equal to, or greater than
   \"y\", respectively. This function should be defined for
   lexicographically comparable types, and \"lexless\" will call
   \"lexcmp\" by default.

"),

("Base","lexless","lexless(x, y)

   Determine whether \"x\" is lexicographically less than \"y\".

"),

("Base","typeof","typeof(x)

   Get the concrete type of \"x\".

"),

("Base","tuple","tuple(xs...)

   Construct a tuple of the given objects.

"),

("Base","ntuple","ntuple(f::Function, n)

   Create a tuple of length \"n\", computing each element as \"f(i)\",
   where \"i\" is the index of the element.

"),

("Base","object_id","object_id(x)

   Get a unique integer id for \"x\". \"object_id(x)==object_id(y)\"
   if and only if \"is(x,y)\".

"),

("Base","hash","hash(x[, h])

   Compute an integer hash code such that \"isequal(x,y)\" implies
   \"hash(x)==hash(y)\". The optional second argument \"h\" is a hash
   code to be mixed with the result.

   New types should implement the 2-argument form, typically  by
   calling the 2-argument \"hash\" method recursively in order to mix
   hashes of the contents with each other (and with \"h\"). Typically,
   any type that implements \"hash\" should also implement its own
   \"==\" (hence \"isequal\") to guarantee the property mentioned
   above.

"),

("Base","finalizer","finalizer(x, function)

   Register a function \"f(x)\" to be called when there are no
   program-accessible references to \"x\". The behavior of this
   function is unpredictable if \"x\" is of a bits type.

"),

("Base","finalize","finalize(x)

   Immediately run finalizers registered for object \"x\".

"),

("Base","copy","copy(x)

   Create a shallow copy of \"x\": the outer structure is copied, but
   not all internal values. For example, copying an array produces a
   new array with identically-same elements as the original.

"),

("Base","deepcopy","deepcopy(x)

   Create a deep copy of \"x\": everything is copied recursively,
   resulting in a fully independent object. For example, deep-copying
   an array produces a new array whose elements are deep copies of the
   original elements. Calling *deepcopy* on an object should generally
   have the same effect as serializing and then deserializing it.

   As a special case, functions can only be actually deep-copied if
   they are anonymous, otherwise they are just copied. The difference
   is only relevant in the case of closures, i.e. functions which may
   contain hidden internal references.

   While it isn't normally necessary, user-defined types can override
   the default \"deepcopy\" behavior by defining a specialized version
   of the function \"deepcopy_internal(x::T, dict::ObjectIdDict)\"
   (which shouldn't otherwise be used), where \"T\" is the type to be
   specialized for, and \"dict\" keeps track of objects copied so far
   within the recursion. Within the definition, \"deepcopy_internal\"
   should be used in place of \"deepcopy\", and the \"dict\" variable
   should be updated as appropriate before returning.

"),

("Base","isdefined","isdefined([object], index | symbol)

   Tests whether an assignable location is defined. The arguments can
   be an array and index, a composite object and field name (as a
   symbol), or a module and a symbol. With a single symbol argument,
   tests whether a global variable with that name is defined in
   \"current_module()\".

"),

("Base","convert","convert(T, x)

   Convert \"x\" to a value of type \"T\".

   If \"T\" is an \"Integer\" type, an \"InexactError\" will be raised
   if \"x\" is not representable by \"T\", for example if \"x\" is not
   integer-valued, or is outside the range supported by \"T\".

      julia> convert(Int, 3.0)
      3

      julia> convert(Int, 3.5)
      ERROR: InexactError()
       in convert at int.jl:196

   If \"T\" is a \"FloatingPoint\" or \"Rational\" type, then it will
   return the closest value to \"x\" representable by \"T\".

      julia> x = 1/3
      0.3333333333333333

      julia> convert(Float32, x)
      0.33333334f0

      julia> convert(Rational{Int32}, x)
      1//3

      julia> convert(Rational{Int64}, x)
      6004799503160661//18014398509481984

"),

("Base","promote","promote(xs...)

   Convert all arguments to their common promotion type (if any), and
   return them all (as a tuple).

"),

("Base","oftype","oftype(x, y)

   Convert \"y\" to the type of \"x\" (\"convert(typeof(x), y)\").

"),

("Base","widen","widen(type | x)

   If the argument is a type, return a \"larger\" type (for numeric
   types, this will be a type with at least as much range and
   precision as the argument, and usually more). Otherwise the
   argument \"x\" is converted to \"widen(typeof(x))\".

      julia> widen(Int32)
      Int64

      julia> widen(1.5f0)
      1.5

"),

("Base","identity","identity(x)

   The identity function. Returns its argument.

"),

("Base","super","super(T::DataType)

   Return the supertype of DataType T

"),

("Base","<:","<:(T1, T2)

   Subtype operator, equivalent to \"issubtype(T1,T2)\".

"),

("Base","<:","<:(T1, T2)

   Subtype operator, equivalent to \"issubtype(T1,T2)\".

"),

("Base","subtypes","subtypes(T::DataType)

   Return a list of immediate subtypes of DataType T.  Note that all
   currently loaded subtypes are included, including those not visible
   in the current module.

"),

("Base","typemin","typemin(type)

   The lowest value representable by the given (real) numeric type.

"),

("Base","typemax","typemax(type)

   The highest value representable by the given (real) numeric type.

"),

("Base","realmin","realmin(type)

   The smallest in absolute value non-subnormal value representable by
   the given floating-point type

"),

("Base","realmax","realmax(type)

   The highest finite value representable by the given floating-point
   type

"),

("Base","maxintfloat","maxintfloat(type)

   The largest integer losslessly representable by the given floating-
   point type

"),

("Base","sizeof","sizeof(s::AbstractString)

   The number of bytes in string \"s\".

"),

("Base","eps","eps(::DateTime) -> Millisecond

   eps(::Date) -> Day

   Returns \"Millisecond(1)\" for \"DateTime\" values and \"Day(1)\"
   for \"Date\" values.

"),

("Base","eps","eps(::DateTime) -> Millisecond

   eps(::Date) -> Day

   Returns \"Millisecond(1)\" for \"DateTime\" values and \"Day(1)\"
   for \"Date\" values.

"),

("Base","promote_type","promote_type(type1, type2)

   Determine a type big enough to hold values of each argument type
   without loss, whenever possible. In some cases, where no type
   exists to which both types can be promoted losslessly, some loss is
   tolerated; for example, \"promote_type(Int64,Float64)\" returns
   \"Float64\" even though strictly, not all \"Int64\" values can be
   represented exactly as \"Float64\" values.

"),

("Base","promote_rule","promote_rule(type1, type2)

   Specifies what type should be used by \"promote\" when given values
   of types \"type1\" and \"type2\". This function should not be
   called directly, but should have definitions added to it for new
   types as appropriate.

"),

("Base","getfield","getfield(value, name::Symbol)

   Extract a named field from a value of composite type. The syntax
   \"a.b\" calls \"getfield(a, :b)\", and the syntax \"a.(b)\" calls
   \"getfield(a, b)\".

"),

("Base","setfield!","setfield!(value, name::Symbol, x)

   Assign \"x\" to a named field in \"value\" of composite type. The
   syntax \"a.b = c\" calls \"setfield!(a, :b, c)\", and the syntax
   \"a.(b) = c\" calls \"setfield!(a, b, c)\".

"),

("Base","fieldoffsets","fieldoffsets(type)

   The byte offset of each field of a type relative to the data start.
   For example, we could use it in the following manner to summarize
   information about a struct type:

      julia> structinfo(T) = [zip(fieldoffsets(T),fieldnames(T),T.types)...];

      julia> structinfo(StatStruct)
      12-element Array{Tuple{Int64,Symbol,DataType},1}:
       (0,:device,UInt64)
       (8,:inode,UInt64)
       (16,:mode,UInt64)
       (24,:nlink,Int64)
       (32,:uid,UInt64)
       (40,:gid,UInt64)
       (48,:rdev,UInt64)
       (56,:size,Int64)
       (64,:blksize,Int64)
       (72,:blocks,Int64)
       (80,:mtime,Float64)
       (88,:ctime,Float64)

"),

("Base","fieldtype","fieldtype(type, name::Symbol | index::Int)

   Determine the declared type of a field (specified by name or index)
   in a composite type.

"),

("Base","isimmutable","isimmutable(v)

   True if value \"v\" is immutable.  See *Immutable Composite Types*
   for a discussion of immutability. Note that this function works on
   values, so if you give it a type, it will tell you that a value of
   \"DataType\" is mutable.

"),

("Base","isbits","isbits(T)

   True if \"T\" is a \"plain data\" type, meaning it is immutable and
   contains no references to other values. Typical examples are
   numeric types such as \"UInt8\", \"Float64\", and
   \"Complex{Float64}\".

      julia> isbits(Complex{Float64})
      true

      julia> isbits(Complex)
      false

"),

("Base","isleaftype","isleaftype(T)

   Determine whether \"T\" is a concrete type that can have instances,
   meaning its only subtypes are itself and \"None\" (but \"T\" itself
   is not \"None\").

"),

("Base","typejoin","typejoin(T, S)

   Compute a type that contains both \"T\" and \"S\".

"),

("Base","typeintersect","typeintersect(T, S)

   Compute a type that contains the intersection of \"T\" and \"S\".
   Usually this will be the smallest such type or one close to it.

"),

("Base","Val{c}","Val{c}()

   Create a \"value type\" out of \"c\", which must be an \"isbits\"
   value. The intent of this construct is to be able to dispatch on
   constants, e.g., \"f(Val{false})\" allows you to dispatch directly
   (at compile-time) to an implementation \"f(::Type{Val{false}})\",
   without having to test the boolean value at runtime.

"),

("","@enum EnumName EnumValue1[=x] EnumValue2[=y]","@enum EnumName EnumValue1[=x] EnumValue2[=y]

   Create an \"Enum\" type with name \"EnumName\" and enum member
   values of \"EnumValue1\" and \"EnumValue2\" with optional assigned
   values of \"x\" and \"y\", respectively. \"EnumName\" can be used
   just like other types and enum member values as regular values,
   such as

      julia> @enum FRUIT apple=1 orange=2 kiwi=3

      julia> f(x::FRUIT) = \"I'm a FRUIT with value: \$(Int(x))\"
      f (generic function with 1 method)

      julia> f(apple)
      \"I'm a FRUIT with value: 1\"

"),

("Base","instances","instances(T::Type)

   Return a collection of all instances of the given type, if
   applicable. Mostly used for enumerated types (see \"@enum\").

"),

("Base","method_exists","method_exists(f, Tuple type) -> Bool

   Determine whether the given generic function has a method matching
   the given \"Tuple\" of argument types.

      julia> method_exists(length, Tuple{Array})
      true

"),

("Base","applicable","applicable(f, args...) -> Bool

   Determine whether the given generic function has a method
   applicable to the given arguments.

      julia> function f(x, y)
                 x + y
             end;

      julia> applicable(f, 1)
      false

      julia> applicable(f, 1, 2)
      true

"),

("Base","invoke","invoke(f, (types...), args...)

   Invoke a method for the given generic function matching the
   specified types (as a tuple), on the specified arguments. The
   arguments must be compatible with the specified types. This allows
   invoking a method other than the most specific matching method,
   which is useful when the behavior of a more general definition is
   explicitly needed (often as part of the implementation of a more
   specific method of the same function).

"),

("Base","|>","|>(x, f)

   Applies a function to the preceding argument. This allows for easy
   function chaining.

      julia> [1:5;] |> x->x.^2 |> sum |> inv
      0.01818181818181818

"),

("Base","call","call(x, args...)

   If \"x\" is not a \"Function\", then \"x(args...)\" is equivalent
   to \"call(x, args...)\".  This means that function-like behavior
   can be added to any type by defining new \"call\" methods.

"),

("Base","eval","eval([m::Module], expr::Expr)

   Evaluate an expression in the given module and return the result.
   Every module (except those defined with \"baremodule\") has its own
   1-argument definition of \"eval\", which evaluates expressions in
   that module.

"),

("Base","@eval","@eval()

   Evaluate an expression and return the value.

"),

("Base","evalfile","evalfile(path::AbstractString)

   Load the file using \"include\", evaluate all expressions, and
   return the value of the last one.

"),

("Base","esc","esc(e::ANY)

   Only valid in the context of an Expr returned from a macro.
   Prevents the macro hygiene pass from turning embedded variables
   into gensym variables. See the *Macros* section of the
   Metaprogramming chapter of the manual for more details and
   examples.

"),

("Base","gensym","gensym([tag])

   Generates a symbol which will not conflict with other variable
   names.

"),

("Base","@gensym","@gensym()

   Generates a gensym symbol for a variable. For example, \"@gensym x
   y\" is transformed into \"x = gensym(\"x\"); y = gensym(\"y\")\".

"),

("Base","parse","parse(type, str[, base])

   Parse a string as a number. If the type is an integer type, then a
   base can be specified (the default is 10). If the type is a
   floating point type, the string is parsed as a decimal floating
   point number. If the string does not contain a valid number, an
   error is raised.

"),

("Base","parse","parse(type, str[, base])

   Parse a string as a number. If the type is an integer type, then a
   base can be specified (the default is 10). If the type is a
   floating point type, the string is parsed as a decimal floating
   point number. If the string does not contain a valid number, an
   error is raised.

"),

("Base","Nullable","Nullable(x)

   Wrap value \"x\" in an object of type \"Nullable\", which indicates
   whether a value is present. \"Nullable(x)\" yields a non-empty
   wrapper, and \"Nullable{T}()\" yields an empty instance of a
   wrapper that might contain a value of type \"T\".

"),

("Base","get","get(f::Function, collection, key)

   Return the value stored for the given key, or if no mapping for the
   key is present, return \"f()\".  Use \"get!()\" to also store the
   default value in the dictionary.

   This is intended to be called using \"do\" block syntax:

      get(dict, key) do
          # default value calculated here
               time()
      end

"),

("Base","get","get(f::Function, collection, key)

   Return the value stored for the given key, or if no mapping for the
   key is present, return \"f()\".  Use \"get!()\" to also store the
   default value in the dictionary.

   This is intended to be called using \"do\" block syntax:

      get(dict, key) do
          # default value calculated here
               time()
      end

"),

("Base","isnull","isnull(x)

   Is the \"Nullable\" object \"x\" null, i.e. missing a value?

"),

("Base","run","run(command)

   Run a command object, constructed with backticks. Throws an error
   if anything goes wrong, including the process exiting with a non-
   zero status.

"),

("Base","spawn","spawn(command)

   Run a command object asynchronously, returning the resulting
   \"Process\" object.

"),

("Base","DevNull","DevNull

   Used in a stream redirect to discard all data written to it.
   Essentially equivalent to /dev/null on Unix or NUL on Windows.
   Usage: \"run(`cat test.txt` |> DevNull)\"

"),

("Base","success","success(command)

   Run a command object, constructed with backticks, and tell whether
   it was successful (exited with a code of 0). An exception is raised
   if the process cannot be started.

"),

("Base","process_running","process_running(p::Process)

   Determine whether a process is currently running.

"),

("Base","process_exited","process_exited(p::Process)

   Determine whether a process has exited.

"),

("Base","kill","kill(manager::FooManager, pid::Int, config::WorkerConfig)

   Implemented by cluster managers. It is called on the master
   process, by \"rmprocs\". It should cause the remote worker
   specified by \"pid\" to exit.
   \"Base.kill(manager::ClusterManager.....)\" executes a remote
   \"exit()\" on \"pid\"

"),

("Base","open","open(f::function, args...)

   Apply the function \"f\" to the result of \"open(args...)\" and
   close the resulting file descriptor upon completion.

   **Example**: \"open(readall, \"file.txt\")\"

"),

("Base","open","open(f::function, args...)

   Apply the function \"f\" to the result of \"open(args...)\" and
   close the resulting file descriptor upon completion.

   **Example**: \"open(readall, \"file.txt\")\"

"),

("Base","Sys","Sys.set_process_title(title::AbstractString)

   Set the process title. No-op on some operating systems. (not
   exported)

"),

("Base","Sys","Sys.get_process_title()

   Get the process title. On some systems, will always return empty
   string. (not exported)

"),

("Base","readandwrite","readandwrite(command)

   Starts running a command asynchronously, and returns a tuple
   (stdout,stdin,process) of the output stream and input stream of the
   process, and the process object itself.

"),

("Base","ignorestatus","ignorestatus(command)

   Mark a command object so that running it will not throw an error if
   the result code is non-zero.

"),

("Base","detach","detach(command)

   Mark a command object so that it will be run in a new process
   group, allowing it to outlive the julia process, and not have
   Ctrl-C interrupts passed to it.

"),

("Base","setenv","setenv(command, env; dir=working_dir)

   Set environment variables to use when running the given command.
   \"env\" is either a dictionary mapping strings to strings, an array
   of strings of the form \"\"var=val\"\", or zero or more
   \"\"var\"=>val\" pair arguments.  In order to modify (rather than
   replace) the existing environment, create \"env\" by \"copy(ENV)\"
   and then setting \"env[\"var\"]=val\" as desired, or use
   \"withenv\".

   The \"dir\" keyword argument can be used to specify a working
   directory for the command.

"),

("Base","withenv","withenv(f::Function, kv::Pair...)

   Execute \"f()\" in an environment that is temporarily modified (not
   replaced as in \"setenv\") by zero or more \"\"var\"=>val\"
   arguments \"kv\".  \"withenv\" is generally used via the
   \"withenv(kv...) do ... end\" syntax.  A value of \"nothing\" can
   be used to temporarily unset an environment variable (if it is
   set).  When \"withenv\" returns, the original environment has been
   restored.

"),

("Base","pipe","pipe(command; stdin, stdout, stderr, append=false)

   Redirect I/O to or from the given \"command\". Keyword arguments
   specify which of the command's streams should be redirected.
   \"append\" controls whether file output appends to the file. This
   is a more general version of the 2-argument \"pipe\" function.
   \"pipe(from, to)\" is equivalent to \"pipe(from, stdout=to)\" when
   \"from\" is a command, and to \"pipe(to, stdin=from)\" when
   \"from\" is another kind of data source.

   **Examples**:     * \"run(pipe(\"dothings\", stdout=\"out.txt\",
   stderr=\"errs.txt\"))\"

      * \"run(pipe(`update`, stdout=\"log.txt\", append=true))\"

"),

("Base","pipe","pipe(command; stdin, stdout, stderr, append=false)

   Redirect I/O to or from the given \"command\". Keyword arguments
   specify which of the command's streams should be redirected.
   \"append\" controls whether file output appends to the file. This
   is a more general version of the 2-argument \"pipe\" function.
   \"pipe(from, to)\" is equivalent to \"pipe(from, stdout=to)\" when
   \"from\" is a command, and to \"pipe(to, stdin=from)\" when
   \"from\" is another kind of data source.

   **Examples**:     * \"run(pipe(\"dothings\", stdout=\"out.txt\",
   stderr=\"errs.txt\"))\"

      * \"run(pipe(`update`, stdout=\"log.txt\", append=true))\"

"),

("Base","gethostname","gethostname() -> AbstractString

   Get the local machine's host name.

"),

("Base","getipaddr","getipaddr() -> AbstractString

   Get the IP address of the local machine, as a string of the form
   \"x.x.x.x\".

"),

("Base","getpid","getpid() -> Int32

   Get julia's process ID.

"),

("Base","time","time(t::TmStruct)

   Converts a \"TmStruct\" struct to a number of seconds since the
   epoch.

"),

("Base","time_ns","time_ns()

   Get the time in nanoseconds. The time corresponding to 0 is
   undefined, and wraps every 5.8 years.

"),

("Base","tic","tic()

   Set a timer to be read by the next call to \"toc()\" or \"toq()\".
   The macro call \"@time expr\" can also be used to time evaluation.

"),

("Base","toc","toc()

   Print and return the time elapsed since the last \"tic()\".

"),

("Base","toq","toq()

   Return, but do not print, the time elapsed since the last
   \"tic()\".

"),

("Base","@time","@time()

   A macro to execute an expression, printing the time it took to
   execute, the number of allocations, and the total number of bytes
   its execution caused to be allocated, before returning the value of
   the expression.

"),

("Base","@timev","@timev()

   This is a verbose version of the \"@time\" macro, it first prints
   the same information as \"@time\", then any non-zero memory
   allocation counters, and then returns the value of the expression.

"),

("Base","@timed","@timed()

   A macro to execute an expression, and return the value of the
   expression, elapsed time, total bytes allocated, garbage collection
   time, and an object with various memory allocation counters.

"),

("Base","@elapsed","@elapsed()

   A macro to evaluate an expression, discarding the resulting value,
   instead returning the number of seconds it took to execute as a
   floating-point number.

"),

("Base","@allocated","@allocated()

   A macro to evaluate an expression, discarding the resulting value,
   instead returning the total number of bytes allocated during
   evaluation of the expression. Note: the expression is evaluated
   inside a local function, instead of the current context, in order
   to eliminate the effects of compilation, however, there still may
   be some allocations due to JIT compilation.  This also makes the
   results inconsistent with the \"@time\" macros, which do not try to
   adjust for the effects of compilation.

"),

("Base","EnvHash","EnvHash() -> EnvHash

   A singleton of this type provides a hash table interface to
   environment variables.

"),

("Base","ENV","ENV

   Reference to the singleton \"EnvHash\", providing a dictionary
   interface to system environment variables.

"),

("Base","@unix","@unix()

   Given \"@unix? a : b\", do \"a\" on Unix systems (including Linux
   and OS X) and \"b\" elsewhere. See documentation for Handling
   Platform Variations in the Calling C and Fortran Code section of
   the manual.

"),

("Base","@osx","@osx()

   Given \"@osx? a : b\", do \"a\" on OS X and \"b\" elsewhere. See
   documentation for Handling Platform Variations in the Calling C and
   Fortran Code section of the manual.

"),

("Base","@linux","@linux()

   Given \"@linux? a : b\", do \"a\" on Linux and \"b\" elsewhere. See
   documentation for Handling Platform Variations in the Calling C and
   Fortran Code section of the manual.

"),

("Base","@windows","@windows()

   Given \"@windows? a : b\", do \"a\" on Windows and \"b\" elsewhere.
   See documentation for Handling Platform Variations in the Calling C
   and Fortran Code section of the manual.

"),

("Base","error","error(message::AbstractString)

   Raise an \"ErrorException\" with the given message

"),

("Base","throw","throw(e)

   Throw an object as an exception

"),

("Base","rethrow","rethrow([e])

   Throw an object without changing the current exception backtrace.
   The default argument is the current exception (if called within a
   \"catch\" block).

"),

("Base","backtrace","backtrace()

   Get a backtrace object for the current program point.

"),

("Base","catch_backtrace","catch_backtrace()

   Get the backtrace of the current exception, for use within
   \"catch\" blocks.

"),

("Base","assert","assert(cond)

   Throw an \"AssertionError\" if \"cond\" is false. Also available as
   the macro \"@assert expr\".

"),

("","@assert cond [text]","@assert cond [text]

   Throw an \"AssertionError\" if \"cond\" is false. Preferred syntax
   for writing assertions.

"),

("Base","ArgumentError","ArgumentError(msg)

   The parameters to a function call do not match a valid signature.

"),

("Base","AssertionError","AssertionError([msg])

   The asserted condition did not evalutate to \"true\".

"),

("Base","BoundsError","BoundsError([a][, i])

   An indexing operation into an array, \"a\", tried to access an out-
   of-bounds element, \"i\".

"),

("Base","DimensionMismatch","DimensionMismatch([msg])

   The objects called do not have matching dimensionality.

"),

("Base","DivideError","DivideError()

   Integer division was attempted with a denominator value of 0.

"),

("Base","DomainError","DomainError()

   The arguments to a function or constructor are outside the valid
   domain.

"),

("Base","EOFError","EOFError()

   No more data was available to read from a file or stream.

"),

("Base","ErrorException","ErrorException(msg)

   Generic error type. The error message, in the *.msg* field, may
   provide more specific details.

"),

("Base","InexactError","InexactError()

   Type conversion cannot be done exactly.

"),

("Base","InterruptException","InterruptException()

   The process was stopped by a terminal interrupt (CTRL+C).

"),

("Base","KeyError","KeyError(key)

   An indexing operation into an \"Associative\" (\"Dict\") or \"Set\"
   like object tried to access or delete a non-existent element.

"),

("Base","LoadError","LoadError(file::AbstractString, line::Int, error)

   An error occurred while *including*, *requiring*, or *using* a
   file. The error specifics should be available in the *.error*
   field.

"),

("Base","MethodError","MethodError(f, args)

   A method with the required type signature does not exist in the
   given generic function.

"),

("Base","NullException","NullException()

   An attempted access to a \"Nullable\" with no defined value.

"),

("Base","OutOfMemoryError","OutOfMemoryError()

   An operation allocated too much memory for either the system or the
   garbage collector to handle properly.

"),

("Base","ReadOnlyMemoryError","ReadOnlyMemoryError()

   An operation tried to write to memory that is read-only.

"),

("Base","OverflowError","OverflowError()

   The result of an expression is too large for the specified type and
   will cause a wraparound.

"),

("Base","ParseError","ParseError(msg)

   The expression passed to the *parse* function could not be
   interpreted as a valid Julia expression.

"),

("Base","ProcessExitedException","ProcessExitedException()

   After a client Julia process has exited, further attempts to
   reference the dead child will throw this exception.

"),

("Base","StackOverflowError","StackOverflowError()

   The function call grew beyond the size of the call stack. This
   usually happens when a call recurses infinitely.

"),

("Base","SystemError","SystemError(prefix::AbstractString[, errnum::Int32])

   A system call failed with an error code (in the \"errno\" global
   variable).

"),

("Base","TypeError","TypeError(func::Symbol, context::AbstractString, expected::Type, got)

   A type assertion failure, or calling an intrinsic function with an
   incorrect argument type.

"),

("Base","UndefRefError","UndefRefError()

   The item or field is not defined for the given object.

"),

("Base","UndefVarError","UndefVarError(var::Symbol)

   A symbol in the current scope is not defined.

"),

("Base","Timer","Timer(delay, repeat=0)

   Create a timer that wakes up tasks waiting for it (by calling
   \"wait\" on the timer object) at a specified interval.

"),

("Base","Timer","Timer(delay, repeat=0)

   Create a timer that wakes up tasks waiting for it (by calling
   \"wait\" on the timer object) at a specified interval.

"),

("Base","module_name","module_name(m::Module) -> Symbol

   Get the name of a module as a symbol.

"),

("Base","module_parent","module_parent(m::Module) -> Module

   Get a module's enclosing module. \"Main\" is its own parent.

"),

("Base","current_module","current_module() -> Module

   Get the *dynamically* current module, which is the module code is
   currently being read from. In general, this is not the same as the
   module containing the call to this function.

"),

("Base","fullname","fullname(m::Module)

   Get the fully-qualified name of a module as a tuple of symbols. For
   example, \"fullname(Base.Pkg)\" gives \"(:Base,:Pkg)\", and
   \"fullname(Main)\" gives \"()\".

"),

("Base","names","names(x::Module[, all=false[, imported=false]])

   Get an array of the names exported by a module, with optionally
   more module globals according to the additional parameters.

"),

("Base","nfields","nfields(x::DataType) -> Int

   Get the number of fields of a data type.

"),

("Base","fieldnames","fieldnames(x::DataType)

   Get an array of the fields of a data type.

"),

("Base","isconst","isconst([m::Module], s::Symbol) -> Bool

   Determine whether a global is declared \"const\" in a given module.
   The default module argument is \"current_module()\".

"),

("Base","isgeneric","isgeneric(f::Function) -> Bool

   Determine whether a function is generic.

"),

("Base","function_name","function_name(f::Function) -> Symbol

   Get the name of a generic function as a symbol, or \":anonymous\".

"),

("Base","function_module","function_module(f::Function, types) -> Module

   Determine the module containing a given definition of a generic
   function.

"),

("Base","functionloc","functionloc(m::Method)

   Returns a tuple \"(filename,line)\" giving the location of a method
   definition.

"),

("Base","functionloc","functionloc(m::Method)

   Returns a tuple \"(filename,line)\" giving the location of a method
   definition.

"),

("Base","gc","gc()

   Perform garbage collection. This should not generally be used.

"),

("Base","gc_enable","gc_enable(on::Bool)

   Control whether garbage collection is enabled using a boolean
   argument (true for enabled, false for disabled). Returns previous
   GC state. Disabling garbage collection should be used only with
   extreme caution, as it can cause memory use to grow without bound.

"),

("Base","macroexpand","macroexpand(x)

   Takes the expression x and returns an equivalent expression with
   all macros removed (expanded).

"),

("Base","expand","expand(x)

   Takes the expression x and returns an equivalent expression in
   lowered form

"),

("Base","code_lowered","code_lowered(f, types)

   Returns an array of lowered ASTs for the methods matching the given
   generic function and type signature.

"),

("Base","@code_lowered","@code_lowered()

   Evaluates the arguments to the function call, determines their
   types, and calls \"code_lowered()\" on the resulting expression

"),

("Base","code_typed","code_typed(f, types; optimize=true)

   Returns an array of lowered and type-inferred ASTs for the methods
   matching the given generic function and type signature. The keyword
   argument \"optimize\" controls whether additional optimizations,
   such as inlining, are also applied.

"),

("Base","@code_typed","@code_typed()

   Evaluates the arguments to the function call, determines their
   types, and calls \"code_typed()\" on the resulting expression

"),

("Base","code_warntype","code_warntype(f, types)

   Displays lowered and type-inferred ASTs for the methods matching
   the given generic function and type signature. The ASTs are
   annotated in such a way as to cause \"non-leaf\" types to be
   emphasized (if color is available, displayed in red). This serves
   as a warning of potential type instability. Not all non-leaf types
   are particularly problematic for performance, so the results need
   to be used judiciously. See *@code_warntype* for more information.

"),

("Base","@code_warntype","@code_warntype()

   Evaluates the arguments to the function call, determines their
   types, and calls \"code_warntype()\" on the resulting expression

"),

("Base","code_llvm","code_llvm(f, types)

   Prints the LLVM bitcodes generated for running the method matching
   the given generic function and type signature to \"STDOUT\".

   All metadata and dbg.* calls are removed from the printed bitcode.
   Use code_llvm_raw for the full IR.

"),

("Base","@code_llvm","@code_llvm()

   Evaluates the arguments to the function call, determines their
   types, and calls \"code_llvm()\" on the resulting expression

"),

("Base","code_native","code_native(f, types)

   Prints the native assembly instructions generated for running the
   method matching the given generic function and type signature to
   STDOUT.

"),

("Base","@code_native","@code_native()

   Evaluates the arguments to the function call, determines their
   types, and calls \"code_native()\" on the resulting expression

"),

("Base","precompile","precompile(f, args::Tuple{Vararg{Any}})

   Compile the given function \"f\" for the argument tuple (of types)
   \"args\", but do not execute it.

"),

("Base","ccall","ccall((symbol, library) or function_pointer, ReturnType, (ArgumentType1, ...), ArgumentValue1, ...)

   Call function in C-exported shared library, specified by
   \"(function name, library)\" tuple, where each component is an
   AbstractString or :Symbol.

   Note that the argument type tuple must be a literal tuple, and not
   a tuple-valued variable or expression. Alternatively, ccall may
   also be used to call a function pointer, such as one returned by
   dlsym.

   Each \"ArgumentValue\" to the \"ccall\" will be converted to the
   corresponding \"ArgumentType\", by automatic insertion of calls to
   \"unsafe_convert(ArgumentType, cconvert(ArgumentType,
   ArgumentValue))\". (see also the documentation for each of these
   functions for further details). In most cases, this simply results
   in a call to \"convert(ArgumentType, ArgumentValue)\"

"),

("Base","cglobal","cglobal((symbol, library)[, type=Void])

   Obtain a pointer to a global variable in a C-exported shared
   library, specified exactly as in \"ccall\".  Returns a
   \"Ptr{Type}\", defaulting to \"Ptr{Void}\" if no Type argument is
   supplied.  The values can be read or written by \"unsafe_load\" or
   \"unsafe_store!\", respectively.

"),

("Base","cfunction","cfunction(function::Function, ReturnType::Type, (ArgumentTypes...))

   Generate C-callable function pointer from Julia function. Type
   annotation of the return value in the callback function is a must
   for situations where Julia cannot infer the return type
   automatically.

   For example:

      function foo()
          # body

          retval::Float64
      end

      bar = cfunction(foo, Float64, ())

"),

("Base","unsafe_convert","unsafe_convert(T, x)

   Convert \"x\" to a value of type \"T\"

   In cases where \"convert\" would need to take a Julia object and
   turn it into a \"Ptr\", this function should be used to define and
   perform that conversion.

   Be careful to ensure that a julia reference to \"x\" exists as long
   as the result of this function will be used. Accordingly, the
   argument \"x\" to this function should never be an expression, only
   a variable name or field reference. For example, \"x=a.b.c\" is
   acceptable, but \"x=[a,b,c]\" is not.

   The \"unsafe\" prefix on this function indicates that using the
   result of this function after the \"x\" argument to this function
   is no longer accessible to the program may cause undefined
   behavior, including program corruption or segfaults, at any later
   time.

"),

("Base","cconvert","cconvert(T, x)

   Convert \"x\" to a value of type \"T\", typically by calling
   \"convert(T,x)\"

   In cases where \"x\" cannot be safely converted to \"T\", unlike
   \"convert\", \"cconvert\" may return an object of a type different
   from \"T\", which however is suitable for \"unsafe_convert\" to
   handle.

   Neither \"convert\" nor \"cconvert\" should take a Julia object and
   turn it into a \"Ptr\".

"),

("Base","unsafe_load","unsafe_load(p::Ptr{T}, i::Integer)

   Load a value of type \"T\" from the address of the ith element
   (1-indexed) starting at \"p\". This is equivalent to the C
   expression \"p[i-1]\".

   The \"unsafe\" prefix on this function indicates that no validation
   is performed on the pointer \"p\" to ensure that it is valid.
   Incorrect usage may segfault your program or return garbage
   answers, in the same manner as C.

"),

("Base","unsafe_store!","unsafe_store!(p::Ptr{T}, x, i::Integer)

   Store a value of type \"T\" to the address of the ith element
   (1-indexed) starting at \"p\". This is equivalent to the C
   expression \"p[i-1] = x\".

   The \"unsafe\" prefix on this function indicates that no validation
   is performed on the pointer \"p\" to ensure that it is valid.
   Incorrect usage may corrupt or segfault your program, in the same
   manner as C.

"),

("Base","unsafe_copy!","unsafe_copy!(dest::Array, do, src::Array, so, N)

   Copy \"N\" elements from a source array to a destination, starting
   at offset \"so\" in the source and \"do\" in the destination
   (1-indexed).

   The \"unsafe\" prefix on this function indicates that no validation
   is performed to ensure that N is inbounds on either array.
   Incorrect usage may corrupt or segfault your program, in the same
   manner as C.

"),

("Base","unsafe_copy!","unsafe_copy!(dest::Array, do, src::Array, so, N)

   Copy \"N\" elements from a source array to a destination, starting
   at offset \"so\" in the source and \"do\" in the destination
   (1-indexed).

   The \"unsafe\" prefix on this function indicates that no validation
   is performed to ensure that N is inbounds on either array.
   Incorrect usage may corrupt or segfault your program, in the same
   manner as C.

"),

("Base","copy!","copy!(dest, do, src, so, N)

   Copy \"N\" elements from collection \"src\" starting at offset
   \"so\", to array \"dest\" starting at offset \"do\". Returns
   \"dest\".

"),

("Base","copy!","copy!(dest, do, src, so, N)

   Copy \"N\" elements from collection \"src\" starting at offset
   \"so\", to array \"dest\" starting at offset \"do\". Returns
   \"dest\".

"),

("Base","pointer","pointer(array[, index])

   Get the native address of an array or string element. Be careful to
   ensure that a julia reference to \"a\" exists as long as this
   pointer will be used. This function is \"unsafe\" like
   \"unsafe_convert\".

   Calling \"Ref(array[, index])\" is generally preferable to this
   function.

"),

("Base","pointer_to_array","pointer_to_array(pointer, dims[, take_ownership::Bool])

   Wrap a native pointer as a Julia Array object. The pointer element
   type determines the array element type. \"own\" optionally
   specifies whether Julia should take ownership of the memory,
   calling \"free\" on the pointer when the array is no longer
   referenced.

"),

("Base","pointer_from_objref","pointer_from_objref(object_instance)

   Get the memory address of a Julia object as a \"Ptr\". The
   existence of the resulting \"Ptr\" will not protect the object from
   garbage collection, so you must ensure that the object remains
   referenced for the whole time that the \"Ptr\" will be used.

"),

("Base","unsafe_pointer_to_objref","unsafe_pointer_to_objref(p::Ptr)

   Convert a \"Ptr\" to an object reference. Assumes the pointer
   refers to a valid heap-allocated Julia object. If this is not the
   case, undefined behavior results, hence this function is considered
   \"unsafe\" and should be used with care.

"),

("Base","disable_sigint","disable_sigint(f::Function)

   Disable Ctrl-C handler during execution of a function, for calling
   external code that is not interrupt safe. Intended to be called
   using \"do\" block syntax as follows:

      disable_sigint() do
          # interrupt-unsafe code
          ...
      end

"),

("Base","reenable_sigint","reenable_sigint(f::Function)

   Re-enable Ctrl-C handler during execution of a function.
   Temporarily reverses the effect of \"disable_sigint\".

"),

("Base","systemerror","systemerror(sysfunc, iftrue)

   Raises a \"SystemError\" for \"errno\" with the descriptive string
   \"sysfunc\" if \"bool\" is true

"),

("Base","Ptr{T}","Ptr{T}

   A memory address referring to data of type \"T\". However, there is
   no guarantee that the memory is actually valid, or that it actually
   represents data of the specified type.

"),

("Base","Ref{T}","Ref{T}

   An object that safely references data of type \"T\". This type is
   guaranteed to point to valid, Julia-allocated memory of the correct
   type. The underlying data is protected from freeing by the garbage
   collector as long as the \"Ref\" itself is referenced.

   When passed as a \"ccall\" argument (either as a \"Ptr\" or \"Ref\"
   type), a \"Ref\" object will be converted to a native pointer to
   the data it references.

   There is no invalid (NULL) \"Ref\".

"),

("Base","Cchar","Cchar

   Equivalent to the native \"char\" c-type

"),

("Base","Cuchar","Cuchar

   Equivalent to the native \"unsigned char\" c-type (UInt8)

"),

("Base","Cshort","Cshort

   Equivalent to the native \"signed short\" c-type (Int16)

"),

("Base","Cushort","Cushort

   Equivalent to the native \"unsigned short\" c-type (UInt16)

"),

("Base","Cint","Cint

   Equivalent to the native \"signed int\" c-type (Int32)

"),

("Base","Cuint","Cuint

   Equivalent to the native \"unsigned int\" c-type (UInt32)

"),

("Base","Clong","Clong

   Equivalent to the native \"signed long\" c-type

"),

("Base","Culong","Culong

   Equivalent to the native \"unsigned long\" c-type

"),

("Base","Clonglong","Clonglong

   Equivalent to the native \"signed long long\" c-type (Int64)

"),

("Base","Culonglong","Culonglong

   Equivalent to the native \"unsigned long long\" c-type (UInt64)

"),

("Base","Cintmax_t","Cintmax_t

   Equivalent to the native \"intmax_t\" c-type (Int64)

"),

("Base","Cuintmax_t","Cuintmax_t

   Equivalent to the native \"uintmax_t\" c-type (UInt64)

"),

("Base","Csize_t","Csize_t

   Equivalent to the native \"size_t\" c-type (UInt)

"),

("Base","Cssize_t","Cssize_t

   Equivalent to the native \"ssize_t\" c-type

"),

("Base","Cptrdiff_t","Cptrdiff_t

   Equivalent to the native \"ptrdiff_t\" c-type (Int)

"),

("Base","Coff_t","Coff_t

   Equivalent to the native \"off_t\" c-type

"),

("Base","Cwchar_t","Cwchar_t

   Equivalent to the native \"wchar_t\" c-type (Int32)

"),

("Base","Cfloat","Cfloat

   Equivalent to the native \"float\" c-type (Float32)

"),

("Base","Cdouble","Cdouble

   Equivalent to the native \"double\" c-type (Float64)

"),

("Base","start","start(iter) -> state

   Get initial iteration state for an iterable object

"),

("Base","done","done(iter, state) -> Bool

   Test whether we are done iterating

"),

("Base","next","next(iter, state) -> item, state

   For a given iterable object and iteration state, return the current
   item and the next iteration state

"),

("Base","zip","zip(iters...)

   For a set of iterable objects, returns an iterable of tuples, where
   the \"i\"th tuple contains the \"i\"th component of each input
   iterable.

   Note that \"zip()\" is its own inverse:
   \"collect(zip(zip(a...)...)) == collect(a)\".

"),

("Base","enumerate","enumerate(iter)

   An iterator that yields \"(i, x)\" where \"i\" is an index starting
   at 1, and \"x\" is the \"i\"th value from the given iterator. It's
   useful when you need not only the values \"x\" over which you are
   iterating, but also the index \"i\" of the iterations.

      julia> a = [\"a\", \"b\", \"c\"];

      julia> for (index, value) in enumerate(a)
                 println(\"\\\$index \\\$value\")
             end
      1 a
      2 b
      3 c

"),

("Base","rest","rest(iter, state)

   An iterator that yields the same elements as \"iter\", but starting
   at the given \"state\".

"),

("Base","countfrom","countfrom(start=1, step=1)

   An iterator that counts forever, starting at \"start\" and
   incrementing by \"step\".

"),

("Base","take","take(iter, n)

   An iterator that generates at most the first \"n\" elements of
   \"iter\".

"),

("Base","drop","drop(iter, n)

   An iterator that generates all but the first \"n\" elements of
   \"iter\".

"),

("Base","cycle","cycle(iter)

   An iterator that cycles through \"iter\" forever.

"),

("Base","repeated","repeated(x[, n::Int])

   An iterator that generates the value \"x\" forever. If \"n\" is
   specified, generates \"x\" that many times (equivalent to
   \"take(repeated(x), n)\").

"),

("Base","isempty","isempty(collection) -> Bool

   Determine whether a collection is empty (has no elements).

      julia> isempty([])
      true

      julia> isempty([1 2 3])
      false

"),

("Base","empty!","empty!(collection) -> collection

   Remove all elements from a \"collection\".

"),

("Base","length","length(s)

   The number of characters in string \"s\".

"),

("Base","endof","endof(collection) -> Integer

   Returns the last index of the collection.

      julia> endof([1,2,4])
      3

"),

("Base","in","in(item, collection) -> Bool

   ∈(item, collection) -> Bool ∋(collection, item) -> Bool ∉(item,
   collection) -> Bool ∌(collection, item) -> Bool

   Determine whether an item is in the given collection, in the sense
   that it is \"==\" to one of the values generated by iterating over
   the collection. Some collections need a slightly different
   definition; for example \"Set\"s check whether the item
   \"isequal()\" to one of the elements. \"Dict\"s look for
   \"(key,value)\" pairs, and the key is compared using \"isequal()\".
   To test for the presence of a key in a dictionary, use \"haskey()\"
   or \"k in keys(dict)\".

"),

("Base","eltype","eltype(type)

   Determine the type of the elements generated by iterating a
   collection of the given \"type\". For associative collection types,
   this will be a \"(key,value)\" tuple type. The definition
   \"eltype(x) = eltype(typeof(x))\" is provided for convenience so
   that instances can be passed instead of types. However the form
   that accepts a type argument should be defined for new types.

"),

("Base","indexin","indexin(a, b)

   Returns a vector containing the highest index in \"b\" for each
   value in \"a\" that is a member of \"b\" . The output vector
   contains 0 wherever \"a\" is not a member of \"b\".

"),

("Base","findin","findin(a, b)

   Returns the indices of elements in collection \"a\" that appear in
   collection \"b\"

"),

("Base","unique","unique(itr[, dim])

   Returns an array containing only the unique elements of the
   iterable \"itr\", in the order that the first of each set of
   equivalent elements originally appears. If \"dim\" is specified,
   returns unique regions of the array \"itr\" along \"dim\".

"),

("Base","reduce","reduce(op, itr)

   Like \"reduce(op, v0, itr)\". This cannot be used with empty
   collections, except for some special cases (e.g. when \"op\" is one
   of \"+\", \"*\", \"max\", \"min\", \"&\", \"|\") when Julia can
   determine the neutral element of \"op\".

"),

("Base","reduce","reduce(op, itr)

   Like \"reduce(op, v0, itr)\". This cannot be used with empty
   collections, except for some special cases (e.g. when \"op\" is one
   of \"+\", \"*\", \"max\", \"min\", \"&\", \"|\") when Julia can
   determine the neutral element of \"op\".

"),

("Base","foldl","foldl(op, itr)

   Like \"foldl(op, v0, itr)\", but using the first element of \"itr\"
   as \"v0\". In general, this cannot be used with empty collections
   (see \"reduce(op, itr)\").

"),

("Base","foldl","foldl(op, itr)

   Like \"foldl(op, v0, itr)\", but using the first element of \"itr\"
   as \"v0\". In general, this cannot be used with empty collections
   (see \"reduce(op, itr)\").

"),

("Base","foldr","foldr(op, itr)

   Like \"foldr(op, v0, itr)\", but using the last element of \"itr\"
   as \"v0\". In general, this cannot be used with empty collections
   (see \"reduce(op, itr)\").

"),

("Base","foldr","foldr(op, itr)

   Like \"foldr(op, v0, itr)\", but using the last element of \"itr\"
   as \"v0\". In general, this cannot be used with empty collections
   (see \"reduce(op, itr)\").

"),

("Base","maximum","maximum(A, dims)

   Compute the maximum value of an array over the given dimensions.

"),

("Base","maximum","maximum(A, dims)

   Compute the maximum value of an array over the given dimensions.

"),

("Base","maximum!","maximum!(r, A)

   Compute the maximum value of \"A\" over the singleton dimensions of
   \"r\", and write results to \"r\".

"),

("Base","minimum","minimum(A, dims)

   Compute the minimum value of an array over the given dimensions.

"),

("Base","minimum","minimum(A, dims)

   Compute the minimum value of an array over the given dimensions.

"),

("Base","minimum!","minimum!(r, A)

   Compute the minimum value of \"A\" over the singleton dimensions of
   \"r\", and write results to \"r\".

"),

("Base","extrema","extrema(itr)

   Compute both the minimum and maximum element in a single pass, and
   return them as a 2-tuple.

"),

("Base","indmax","indmax(itr) -> Integer

   Returns the index of the maximum element in a collection.

"),

("Base","indmin","indmin(itr) -> Integer

   Returns the index of the minimum element in a collection.

"),

("Base","findmax","findmax(A, dims) -> (maxval, index)

   For an array input, returns the value and index of the maximum over
   the given dimensions.

"),

("Base","findmax","findmax(A, dims) -> (maxval, index)

   For an array input, returns the value and index of the maximum over
   the given dimensions.

"),

("Base","findmin","findmin(A, dims) -> (minval, index)

   For an array input, returns the value and index of the minimum over
   the given dimensions.

"),

("Base","findmin","findmin(A, dims) -> (minval, index)

   For an array input, returns the value and index of the minimum over
   the given dimensions.

"),

("Base","maxabs","maxabs(A, dims)

   Compute the maximum absolute values over given dimensions.

"),

("Base","maxabs","maxabs(A, dims)

   Compute the maximum absolute values over given dimensions.

"),

("Base","maxabs!","maxabs!(r, A)

   Compute the maximum absolute values over the singleton dimensions
   of \"r\", and write values to \"r\".

"),

("Base","minabs","minabs(A, dims)

   Compute the minimum absolute values over given dimensions.

"),

("Base","minabs","minabs(A, dims)

   Compute the minimum absolute values over given dimensions.

"),

("Base","minabs!","minabs!(r, A)

   Compute the minimum absolute values over the singleton dimensions
   of \"r\", and write values to \"r\".

"),

("Base","sum","sum(f, itr)

   Sum the results of calling function \"f\" on each element of
   \"itr\".

"),

("Base","sum","sum(f, itr)

   Sum the results of calling function \"f\" on each element of
   \"itr\".

"),

("Base","sum!","sum!(r, A)

   Sum elements of \"A\" over the singleton dimensions of \"r\", and
   write results to \"r\".

"),

("Base","sum","sum(f, itr)

   Sum the results of calling function \"f\" on each element of
   \"itr\".

"),

("Base","sumabs","sumabs(A, dims)

   Sum absolute values of elements of an array over the given
   dimensions.

"),

("Base","sumabs","sumabs(A, dims)

   Sum absolute values of elements of an array over the given
   dimensions.

"),

("Base","sumabs!","sumabs!(r, A)

   Sum absolute values of elements of \"A\" over the singleton
   dimensions of \"r\", and write results to \"r\".

"),

("Base","sumabs2","sumabs2(A, dims)

   Sum squared absolute values of elements of an array over the given
   dimensions.

"),

("Base","sumabs2","sumabs2(A, dims)

   Sum squared absolute values of elements of an array over the given
   dimensions.

"),

("Base","sumabs2!","sumabs2!(r, A)

   Sum squared absolute values of elements of \"A\" over the singleton
   dimensions of \"r\", and write results to \"r\".

"),

("Base","prod","prod(A, dims)

   Multiply elements of an array over the given dimensions.

"),

("Base","prod","prod(A, dims)

   Multiply elements of an array over the given dimensions.

"),

("Base","prod!","prod!(r, A)

   Multiply elements of \"A\" over the singleton dimensions of \"r\",
   and write results to \"r\".

"),

("Base","any","any(p, itr) -> Bool

   Determine whether predicate \"p\" returns true for any elements of
   \"itr\".

"),

("Base","any","any(p, itr) -> Bool

   Determine whether predicate \"p\" returns true for any elements of
   \"itr\".

"),

("Base","any!","any!(r, A)

   Test whether any values in \"A\" along the singleton dimensions of
   \"r\" are true, and write results to \"r\".

"),

("Base","all","all(p, itr) -> Bool

   Determine whether predicate \"p\" returns true for all elements of
   \"itr\".

      julia> all(i->(4<=i<=6), [4,5,6])
      true

"),

("Base","all","all(p, itr) -> Bool

   Determine whether predicate \"p\" returns true for all elements of
   \"itr\".

      julia> all(i->(4<=i<=6), [4,5,6])
      true

"),

("Base","all!","all!(r, A)

   Test whether all values in \"A\" along the singleton dimensions of
   \"r\" are true, and write results to \"r\".

"),

("Base","count","count(p, itr) -> Integer

   Count the number of elements in \"itr\" for which predicate \"p\"
   returns true.

"),

("Base","any","any(p, itr) -> Bool

   Determine whether predicate \"p\" returns true for any elements of
   \"itr\".

"),

("Base","all","all(p, itr) -> Bool

   Determine whether predicate \"p\" returns true for all elements of
   \"itr\".

      julia> all(i->(4<=i<=6), [4,5,6])
      true

"),

("Base","map","map(f, c...) -> collection

   Transform collection \"c\" by applying \"f\" to each element. For
   multiple collection arguments, apply \"f\" elementwise.

      julia> map((x) -> x * 2, [1, 2, 3])
      3-element Array{Int64,1}:
       2
       4
       6

      julia> map(+, [1, 2, 3], [10, 20, 30])
      3-element Array{Int64,1}:
       11
       22
       33

"),

("Base","map!","map!(function, destination, collection...)

   Like \"map()\", but stores the result in \"destination\" rather
   than a new collection. \"destination\" must be at least as large as
   the first collection.

"),

("Base","map!","map!(function, destination, collection...)

   Like \"map()\", but stores the result in \"destination\" rather
   than a new collection. \"destination\" must be at least as large as
   the first collection.

"),

("Base","mapreduce","mapreduce(f, op, itr)

   Like \"mapreduce(f, op, v0, itr)\". In general, this cannot be used
   with empty collections (see \"reduce(op, itr)\").

"),

("Base","mapreduce","mapreduce(f, op, itr)

   Like \"mapreduce(f, op, v0, itr)\". In general, this cannot be used
   with empty collections (see \"reduce(op, itr)\").

"),

("Base","mapfoldl","mapfoldl(f, op, itr)

   Like \"mapfoldl(f, op, v0, itr)\", but using the first element of
   \"itr\" as \"v0\". In general, this cannot be used with empty
   collections (see \"reduce(op, itr)\").

"),

("Base","mapfoldl","mapfoldl(f, op, itr)

   Like \"mapfoldl(f, op, v0, itr)\", but using the first element of
   \"itr\" as \"v0\". In general, this cannot be used with empty
   collections (see \"reduce(op, itr)\").

"),

("Base","mapfoldr","mapfoldr(f, op, itr)

   Like \"mapfoldr(f, op, v0, itr)\", but using the first element of
   \"itr\" as \"v0\". In general, this cannot be used with empty
   collections (see \"reduce(op, itr)\").

"),

("Base","mapfoldr","mapfoldr(f, op, itr)

   Like \"mapfoldr(f, op, v0, itr)\", but using the first element of
   \"itr\" as \"v0\". In general, this cannot be used with empty
   collections (see \"reduce(op, itr)\").

"),

("Base","first","first(coll)

   Get the first element of an iterable collection. Returns the start
   point of a \"Range\" even if it is empty.

"),

("Base","last","last(coll)

   Get the last element of an ordered collection, if it can be
   computed in O(1) time. This is accomplished by calling \"endof()\"
   to get the last index. Returns the end point of a \"Range\" even if
   it is empty.

"),

("Base","step","step(r)

   Get the step size of a \"Range\" object.

"),

("Base","collect","collect(element_type, collection)

   Return an array of type \"Array{element_type,1}\" of all items in a
   collection.

"),

("Base","collect","collect(element_type, collection)

   Return an array of type \"Array{element_type,1}\" of all items in a
   collection.

"),

("Base","issubset","issubset(A, S) -> Bool

   ⊆(A, S) -> Bool

   True if A is a subset of or equal to S.

"),

("Base","filter","filter(function, collection)

   Return a copy of \"collection\", removing elements for which
   \"function\" is false. For associative collections, the function is
   passed two arguments (key and value).

"),

("Base","filter!","filter!(function, collection)

   Update \"collection\", removing elements for which \"function\" is
   false. For associative collections, the function is passed two
   arguments (key and value).

"),

("Base","getindex","getindex(collection, key...)

   Retrieve the value(s) stored at the given key or index within a
   collection. The syntax \"a[i,j,...]\" is converted by the compiler
   to \"getindex(a, i, j, ...)\".

"),

("Base","setindex!","setindex!(collection, value, key...)

   Store the given value at the given key or index within a
   collection. The syntax \"a[i,j,...] = x\" is converted by the
   compiler to \"setindex!(a, x, i, j, ...)\".

"),

("Base","Dict","Dict([itr])

   \"Dict{K,V}()\" constructs a hash table with keys of type \"K\" and
   values of type \"V\".

   Given a single iterable argument, constructs a \"Dict\" whose key-
   value pairs are taken from 2-tuples \"(key,value)\" generated by
   the argument.

      julia> Dict([(\"A\", 1), (\"B\", 2)])
      Dict{ASCIIString,Int64} with 2 entries:
        \"B\" => 2
        \"A\" => 1

   Alternatively, a sequence of pair arguments may be passed.

      julia> Dict(\"A\"=>1, \"B\"=>2)
      Dict{ASCIIString,Int64} with 2 entries:
        \"B\" => 2
        \"A\" => 1

"),

("Base","haskey","haskey(collection, key) -> Bool

   Determine whether a collection has a mapping for a given key.

"),

("Base","get","get(f::Function, collection, key)

   Return the value stored for the given key, or if no mapping for the
   key is present, return \"f()\".  Use \"get!()\" to also store the
   default value in the dictionary.

   This is intended to be called using \"do\" block syntax:

      get(dict, key) do
          # default value calculated here
               time()
      end

"),

("Base","get","get(f::Function, collection, key)

   Return the value stored for the given key, or if no mapping for the
   key is present, return \"f()\".  Use \"get!()\" to also store the
   default value in the dictionary.

   This is intended to be called using \"do\" block syntax:

        get(dict, key) do
            # default value calculated here
                 time()
        end


               time()
      end

"),

("Base","get!","get!(f::Function, collection, key)

   Return the value stored for the given key, or if no mapping for the
   key is present, store \"key => f()\", and return \"f()\".

   This is intended to be called using \"do\" block syntax:

      get!(dict, key) do
          # default value calculated here
               time()
      end

"),

("Base","get!","get!(f::Function, collection, key)

   Return the value stored for the given key, or if no mapping for the
   key is present, store \"key => f()\", and return \"f()\".

   This is intended to be called using \"do\" block syntax:

        get!(dict, key) do
            # default value calculated here
                 time()
        end


               time()
      end

"),

("Base","getkey","getkey(collection, key, default)

   Return the key matching argument \"key\" if one exists in
   \"collection\", otherwise return \"default\".

"),

("Base","delete!","delete!(collection, key)

   Delete the mapping for the given key in a collection, and return
   the collection.

"),

("Base","pop!","pop!(collection) -> item

   Remove the last item in \"collection\" and return it.

      julia> A=[1, 2, 3, 4, 5, 6]
      6-element Array{Int64,1}:
       1
       2
       3
       4
       5
       6

      julia> pop!(A)
      6

      julia> A
      5-element Array{Int64,1}:
       1
       2
       3
       4
       5

"),

("Base","keys","keys(collection)

   Return an iterator over all keys in a collection.
   \"collect(keys(d))\" returns an array of keys.

"),

("Base","values","values(collection)

   Return an iterator over all values in a collection.
   \"collect(values(d))\" returns an array of values.

"),

("Base","merge","merge(collection, others...)

   Construct a merged collection from the given collections. If
   necessary, the types of the resulting collection will be promoted
   to accommodate the types of the merged collections. If the same key
   is present in another collection, the value for that key will be
   the value it has in the last collection listed.

      julia> a = Dict(\"foo\" => 0.0, \"bar\" => 42.0)
      Dict{ASCIIString,Float64} with 2 entries:
        \"bar\" => 42.0
        \"foo\" => 0.0

      julia> b = Dict(utf8(\"baz\") => 17, utf8(\"bar\") => 4711)
      Dict{UTF8String,Int64} with 2 entries:
        \"bar\" => 4711
        \"baz\" => 17

      julia> merge(a, b)
      Dict{UTF8String,Float64} with 3 entries:
        \"bar\" => 4711.0
        \"baz\" => 17.0
        \"foo\" => 0.0

      julia> merge(b, a)
      Dict{UTF8String,Float64} with 3 entries:
        \"bar\" => 42.0
        \"baz\" => 17.0
        \"foo\" => 0.0

"),

("Base","merge!","merge!(collection, others...)

   Update collection with pairs from the other collections

"),

("Base","sizehint!","sizehint!(s, n)

   Suggest that collection \"s\" reserve capacity for at least \"n\"
   elements. This can improve performance.

"),

("Base","Set","Set([itr])

   Construct a \"Set\" of the values generated by the given iterable
   object, or an empty set. Should be used instead of \"IntSet\" for
   sparse integer sets, or for sets of arbitrary objects.

"),

("Base","IntSet","IntSet([itr])

   Construct a sorted set of the integers generated by the given
   iterable object, or an empty set. Implemented as a bit string, and
   therefore designed for dense integer sets. Only non-negative
   integers can be stored. If the set will be sparse (for example
   holding a single very large integer), use \"Set\" instead.

"),

("Base","union","union(s1, s2...)

   ∪(s1, s2)

   Construct the union of two or more sets. Maintains order with
   arrays.

"),

("Base","union!","union!(s, iterable)

   Union each element of \"iterable\" into set \"s\" in-place.

"),

("Base","intersect","intersect(s1, s2...)

   ∩(s1, s2)

   Construct the intersection of two or more sets. Maintains order and
   multiplicity of the first argument for arrays and ranges.

"),

("Base","setdiff","setdiff(s1, s2)

   Construct the set of elements in \"s1\" but not \"s2\". Maintains
   order with arrays. Note that both arguments must be collections,
   and both will be iterated over. In particular,
   \"setdiff(set,element)\" where \"element\" is a potential member of
   \"set\", will not work in general.

"),

("Base","setdiff!","setdiff!(s, iterable)

   Remove each element of \"iterable\" from set \"s\" in-place.

"),

("Base","symdiff","symdiff(s1, s2...)

   Construct the symmetric difference of elements in the passed in
   sets or arrays. Maintains order with arrays.

"),

("Base","symdiff!","symdiff!(s1, s2)

   Construct the symmetric difference of sets \"s1\" and \"s2\",
   storing the result in \"s1\".

"),

("Base","symdiff!","symdiff!(s1, s2)

   Construct the symmetric difference of sets \"s1\" and \"s2\",
   storing the result in \"s1\".

"),

("Base","symdiff!","symdiff!(s1, s2)

   Construct the symmetric difference of sets \"s1\" and \"s2\",
   storing the result in \"s1\".

"),

("Base","complement","complement(s)

   Returns the set-complement of \"IntSet\" \"s\".

"),

("Base","complement!","complement!(s)

   Mutates \"IntSet\" \"s\" into its set-complement.

"),

("Base","intersect!","intersect!(s1, s2)

   Intersects sets \"s1\" and \"s2\" and overwrites the set \"s1\"
   with the result. If needed, \"s1\" will be expanded to the size of
   \"s2\".

"),

("Base","issubset","issubset(A, S) -> Bool

   ⊆(A, S) -> Bool

   True if A is a subset of or equal to S.

"),

("Base","push!","push!(collection, items...) -> collection

   Insert one or more \"items\" at the end of \"collection\".

      julia> push!([1, 2, 3], 4, 5, 6)
      6-element Array{Int64,1}:
       1
       2
       3
       4
       5
       6

   Use \"append!()\" to add all the elements of another collection to
   \"collection\". The result of the preceding example is equivalent
   to \"append!([1, 2, 3], [4, 5, 6])\".

"),

("Base","pop!","pop!(collection) -> item

   Remove the last item in \"collection\" and return it.

      julia> A=[1, 2, 3, 4, 5, 6]
      6-element Array{Int64,1}:
       1
       2
       3
       4
       5
       6

      julia> pop!(A)
      6

      julia> A
      5-element Array{Int64,1}:
       1
       2
       3
       4
       5

"),

("Base","unshift!","unshift!(collection, items...) -> collection

   Insert one or more \"items\" at the beginning of \"collection\".

      julia> unshift!([1, 2, 3, 4], 5, 6)
      6-element Array{Int64,1}:
       5
       6
       1
       2
       3
       4

"),

("Base","shift!","shift!(collection) -> item

   Remove the first \"item\" from \"collection\".

      julia> A = [1, 2, 3, 4, 5, 6]
      6-element Array{Int64,1}:
       1
       2
       3
       4
       5
       6

      julia> shift!(A)
      1

      julia> A
      5-element Array{Int64,1}:
       2
       3
       4
       5
       6

"),

("Base","insert!","insert!(collection, index, item)

   Insert an \"item\" into \"collection\" at the given \"index\".
   \"index\" is the index of \"item\" in the resulting \"collection\".

      julia> insert!([6, 5, 4, 2, 1], 4, 3)
      6-element Array{Int64,1}:
       6
       5
       4
       3
       2
       1

"),

("Base","deleteat!","deleteat!(collection, itr)

   Remove the items at the indices given by \"itr\", and return the
   modified \"collection\". Subsequent items are shifted to fill the
   resulting gap. \"itr\" must be sorted and unique.

      julia> deleteat!([6, 5, 4, 3, 2, 1], 1:2:5)
      3-element Array{Int64,1}:
       5
       3
       1

      julia> deleteat!([6, 5, 4, 3, 2, 1], (2, 2))
      ERROR: ArgumentError: indices must be unique and sorted
       in deleteat! at array.jl:631

"),

("Base","deleteat!","deleteat!(collection, itr)

   Remove the items at the indices given by \"itr\", and return the
   modified \"collection\". Subsequent items are shifted to fill the
   resulting gap. \"itr\" must be sorted and unique.

      julia> deleteat!([6, 5, 4, 3, 2, 1], 1:2:5)
      3-element Array{Int64,1}:
       5
       3
       1

      julia> deleteat!([6, 5, 4, 3, 2, 1], (2, 2))
      ERROR: ArgumentError: indices must be unique and sorted
       in deleteat! at array.jl:631

"),

("Base","splice!","splice!(collection, range[, replacement]) -> items

   Remove items in the specified index range, and return a collection
   containing the removed items. Subsequent items are shifted down to
   fill the resulting gap. If specified, replacement values from an
   ordered collection will be spliced in place of the removed items.

   To insert \"replacement\" before an index \"n\" without removing
   any items, use \"splice!(collection, n:n-1, replacement)\".

      julia> splice!(A, 4:3, 2)
      0-element Array{Int64,1}

      julia> A
      8-element Array{Int64,1}:
       -1
       -2
       -3
        2
        5
        4
        3
       -1

"),

("Base","splice!","splice!(collection, range[, replacement]) -> items

   Remove items in the specified index range, and return a collection
   containing the removed items. Subsequent items are shifted down to
   fill the resulting gap. If specified, replacement values from an
   ordered collection will be spliced in place of the removed items.

   To insert \"replacement\" before an index \"n\" without removing
   any items, use \"splice!(collection, n:n-1, replacement)\".

      julia> splice!(A, 4:3, 2)
      0-element Array{Int64,1}

      julia> A
      8-element Array{Int64,1}:
       -1
       -2
       -3
        2
        5
        4
        3
       -1

"),

("Base","resize!","resize!(collection, n) -> collection

   Resize \"collection\" to contain \"n\" elements. If \"n\" is
   smaller than the current collection length, the first \"n\"
   elements will be retained. If \"n\" is larger, the new elements are
   not guaranteed to be initialized.

      julia> resize!([6, 5, 4, 3, 2, 1], 3)
      3-element Array{Int64,1}:
       6
       5
       4

      julia> resize!([6, 5, 4, 3, 2, 1], 8)
      8-element Array{Int64,1}:
       6
       5
       4
       3
       2
       1
       0
       0

"),

("Base","append!","append!(collection, collection2) -> collection.

   Add the elements of \"collection2\" to the end of \"collection\".

      julia> append!([1],[2,3])
      3-element Array{Int64,1}:
       1
       2
       3

      julia> append!([1, 2, 3], [4, 5, 6])
      6-element Array{Int64,1}:
       1
       2
       3
       4
       5
       6

   Use \"push!()\" to add individual items to \"collection\" which are
   not already themselves in another collection. The result is of the
   preceding example is equivalent to \"push!([1, 2, 3], 4, 5, 6)\".

"),

("Base","prepend!","prepend!(collection, items) -> collection

   Insert the elements of \"items\" to the beginning of
   \"collection\".

      julia> prepend!([3],[1,2])
      3-element Array{Int64,1}:
       1
       2
       3

"),

("Base.Collections","PriorityQueue","PriorityQueue(K, V[, ord])

   Construct a new \"PriorityQueue\", with keys of type \"K\" and
   values/priorites of type \"V\". If an order is not given, the
   priority queue is min-ordered using the default comparison for
   \"V\".

"),

("Base.Collections","enqueue!","enqueue!(pq, k, v)

   Insert the a key \"k\" into a priority queue \"pq\" with priority
   \"v\".

"),

("Base.Collections","dequeue!","dequeue!(pq)

   Remove and return the lowest priority key from a priority queue.

"),

("Base.Collections","peek","peek(pq)

   Return the lowest priority key from a priority queue without
   removing that key from the queue.

"),

("Base.Collections","heapify","heapify(v[, ord])

   Return a new vector in binary heap order, optionally using the
   given ordering.

"),

("Base.Collections","heapify!","heapify!(v[, ord])

   In-place \"heapify()\".

"),

("Base.Collections","isheap","isheap(v[, ord])

   Return true iff an array is heap-ordered according to the given
   order.

"),

("Base.Collections","heappush!","heappush!(v, x[, ord])

   Given a binary heap-ordered array, push a new element \"x\",
   preserving the heap property. For efficiency, this function does
   not check that the array is indeed heap-ordered.

"),

("Base.Collections","heappop!","heappop!(v[, ord])

   Given a binary heap-ordered array, remove and return the lowest
   ordered element. For efficiency, this function does not check that
   the array is indeed heap-ordered.

"),

("Base","nothing","nothing

   The singleton instance of type \"Void\", used by convention when
   there is no value to return (as in a C \"void\" function). Can be
   converted to an empty \"Nullable\" value.

"),

("Base","OS_NAME","OS_NAME

   A symbol representing the name of the operating system. Possible
   values are \":Linux\", \":Darwin\" (OS X), or \":Windows\".

"),

("Base","ARGS","ARGS

   An array of the command line arguments passed to Julia, as strings.

"),

("Base","C_NULL","C_NULL

   The C null pointer constant, sometimes used when calling external
   code.

"),

("Base","CPU_CORES","CPU_CORES

   The number of CPU cores in the system.

"),

("Base","WORD_SIZE","WORD_SIZE

   Standard word size on the current machine, in bits.

"),

("Base","VERSION","VERSION

   An object describing which version of Julia is in use.

"),

("Base","LOAD_PATH","LOAD_PATH

   An array of paths (as strings) where the \"require\" function looks
   for code.

"),

("Base","JULIA_HOME","JULIA_HOME

   A string containing the full path to the directory containing the
   \"julia\" executable.

"),

("Base","ANY","ANY

   Equivalent to \"Any\" for dispatch purposes, but signals the
   compiler to skip code generation specialization for that field

"),

("Dates","Period","Period

"),

("Dates","Year","Year

"),

("Dates","Month","Month

"),

("Dates","Week","Week

"),

("Dates","Day","Day

"),

("Dates","Hour","Hour

"),

("Dates","Minute","Minute

"),

("Dates","Second","Second

"),

("Dates","Millisecond","Millisecond

   \"Period\" types represent discrete, human representations of time.

"),

("Dates","Instant","Instant

   \"Instant\" types represent integer-based, machine representations
   of time as continuous timelines starting from an epoch.

"),

("Dates","UTInstant{T}","UTInstant{T}

   The \"UTInstant\" represents a machine timeline based on *UT* time
   (1 day = one revolution of the earth). The \"{T}\" is a \"Period\"
   parameter that indicates the resolution or precision of the
   instant.

"),

("Dates","TimeType","TimeType

   \"TimeType\" types wrap \"Instant\" machine instances to provide
   human representations of the machine instant.

"),

("Dates","DateTime","DateTime

   \"DateTime\" wraps a \"UTInstant{Millisecond}\" and interprets it
   according to the proleptic Gregorian calendar.

"),

("Dates","Date","Date

   \"Date\" wraps a \"UTInstant{Day}\" and interprets it according to
   the proleptic Gregorian calendar.

"),

("Dates","DateTime","DateTime(dt::AbstractString, df::DateFormat) -> DateTime

   Similar form as above for parsing a \"DateTime\", but passes a
   \"DateFormat\" object instead of a raw formatting string. It is
   more efficient if similarly formatted date strings will be parsed
   repeatedly to first create a \"DateFormat\" object then use this
   method for parsing.

"),

("Dates","DateTime","DateTime(dt::AbstractString, df::DateFormat) -> DateTime

   Similar form as above for parsing a \"DateTime\", but passes a
   \"DateFormat\" object instead of a raw formatting string. It is
   more efficient if similarly formatted date strings will be parsed
   repeatedly to first create a \"DateFormat\" object then use this
   method for parsing.

"),

("Dates","DateTime","DateTime(dt::AbstractString, df::DateFormat) -> DateTime

   Similar form as above for parsing a \"DateTime\", but passes a
   \"DateFormat\" object instead of a raw formatting string. It is
   more efficient if similarly formatted date strings will be parsed
   repeatedly to first create a \"DateFormat\" object then use this
   method for parsing.

"),

("Dates","DateTime","DateTime(dt::AbstractString, df::DateFormat) -> DateTime

   Similar form as above for parsing a \"DateTime\", but passes a
   \"DateFormat\" object instead of a raw formatting string. It is
   more efficient if similarly formatted date strings will be parsed
   repeatedly to first create a \"DateFormat\" object then use this
   method for parsing.

"),

("Dates","DateTime","DateTime(dt::AbstractString, df::DateFormat) -> DateTime

   Similar form as above for parsing a \"DateTime\", but passes a
   \"DateFormat\" object instead of a raw formatting string. It is
   more efficient if similarly formatted date strings will be parsed
   repeatedly to first create a \"DateFormat\" object then use this
   method for parsing.

"),

("Dates","Dates","Dates.DateFormat(format::AbstractString) -> DateFormat

   Construct a date formatting object that can be passed repeatedly
   for parsing similarly formatted date strings. \"format\" is a
   format string in the form described above (e.g. \"\"yyyy-mm-
   dd\"\").

"),

("Dates","DateTime","DateTime(dt::AbstractString, df::DateFormat) -> DateTime

   Similar form as above for parsing a \"DateTime\", but passes a
   \"DateFormat\" object instead of a raw formatting string. It is
   more efficient if similarly formatted date strings will be parsed
   repeatedly to first create a \"DateFormat\" object then use this
   method for parsing.

"),

("Dates","Date","Date(dt::AbstractString, df::DateFormat) -> Date

   Parse a date from a date string \"dt\" using a \"DateFormat\"
   object \"df\".

"),

("Dates","Date","Date(dt::AbstractString, df::DateFormat) -> Date

   Parse a date from a date string \"dt\" using a \"DateFormat\"
   object \"df\".

"),

("Dates","Date","Date(dt::AbstractString, df::DateFormat) -> Date

   Parse a date from a date string \"dt\" using a \"DateFormat\"
   object \"df\".

"),

("Dates","Date","Date(dt::AbstractString, df::DateFormat) -> Date

   Parse a date from a date string \"dt\" using a \"DateFormat\"
   object \"df\".

"),

("Dates","Date","Date(dt::AbstractString, df::DateFormat) -> Date

   Parse a date from a date string \"dt\" using a \"DateFormat\"
   object \"df\".

"),

("Dates","Date","Date(dt::AbstractString, df::DateFormat) -> Date

   Parse a date from a date string \"dt\" using a \"DateFormat\"
   object \"df\".

"),

("Dates","now","now(::Type{UTC}) -> DateTime

   Returns a DateTime corresponding to the user's system time as
   UTC/GMT.

"),

("Dates","now","now(::Type{UTC}) -> DateTime

   Returns a DateTime corresponding to the user's system time as
   UTC/GMT.

"),

("Dates","eps","eps(::DateTime) -> Millisecond

   eps(::Date) -> Day

   Returns \"Millisecond(1)\" for \"DateTime\" values and \"Day(1)\"
   for \"Date\" values.

"),

("Dates","year","year(dt::TimeType) -> Int64

   month(dt::TimeType) -> Int64 week(dt::TimeType) -> Int64
   day(dt::TimeType) -> Int64 hour(dt::TimeType) -> Int64
   minute(dt::TimeType) -> Int64 second(dt::TimeType) -> Int64
   millisecond(dt::TimeType) -> Int64

   Return the field part of a Date or DateTime as an \"Int64\".

"),

("Dates","Year","Year(v)

   Month(v) Week(v) Day(v) Hour(v) Minute(v) Second(v) Millisecond(v)

   Construct a \"Period\" type with the given \"v\" value. Input must
   be losslessly convertible to an \"Int64\".

"),

("Dates","yearmonth","yearmonth(dt::TimeType) -> (Int64, Int64)

   Simultaneously return the year and month parts of a Date or
   DateTime.

"),

("Dates","monthday","monthday(dt::TimeType) -> (Int64, Int64)

   Simultaneously return the month and day parts of a Date or
   DateTime.

"),

("Dates","yearmonthday","yearmonthday(dt::TimeType) -> (Int64, Int64, Int64)

   Simultaneously return the year, month, and day parts of a Date or
   DateTime.

"),

("Dates","dayname","dayname(dt::TimeType; locale=\"english\") -> AbstractString

   Return the full day name corresponding to the day of the week of
   the Date or DateTime in the given \"locale\".

"),

("Dates","dayabbr","dayabbr(dt::TimeType; locale=\"english\") -> AbstractString

   Return the abbreviated name corresponding to the day of the week of
   the Date or DateTime in the given \"locale\".

"),

("Dates","dayofweek","dayofweek(dt::TimeType) -> Int64

   Returns the day of the week as an \"Int64\" with \"1 = Monday, 2 =
   Tuesday, etc.\".

"),

("Dates","dayofweekofmonth","dayofweekofmonth(dt::TimeType) -> Int

   For the day of week of \"dt\", returns which number it is in
   \"dt\"'s month. So if the day of the week of \"dt\" is Monday, then
   \"1 = First Monday of the month, 2 = Second Monday of the month,
   etc.\" In the range 1:5.

"),

("Dates","daysofweekinmonth","daysofweekinmonth(dt::TimeType) -> Int

   For the day of week of \"dt\", returns the total number of that day
   of the week in \"dt\"'s month. Returns 4 or 5. Useful in temporal
   expressions for specifying the last day of a week in a month by
   including \"dayofweekofmonth(dt) == daysofweekinmonth(dt)\" in the
   adjuster function.

"),

("Dates","monthname","monthname(dt::TimeType; locale=\"english\") -> AbstractString

   Return the full name of the month of the Date or DateTime in the
   given \"locale\".

"),

("Dates","monthabbr","monthabbr(dt::TimeType; locale=\"english\") -> AbstractString

   Return the abbreviated month name of the Date or DateTime in the
   given \"locale\".

"),

("Dates","daysinmonth","daysinmonth(dt::TimeType) -> Int

   Returns the number of days in the month of \"dt\". Value will be
   28, 29, 30, or 31.

"),

("Dates","isleapyear","isleapyear(dt::TimeType) -> Bool

   Returns true if the year of \"dt\" is a leap year.

"),

("Dates","dayofyear","dayofyear(dt::TimeType) -> Int

   Returns the day of the year for \"dt\" with January 1st being day
   1.

"),

("Dates","daysinyear","daysinyear(dt::TimeType) -> Int

   Returns 366 if the year of \"dt\" is a leap year, otherwise returns
   365.

"),

("Dates","quarterofyear","quarterofyear(dt::TimeType) -> Int

   Returns the quarter that \"dt\" resides in. Range of value is 1:4.

"),

("Dates","dayofquarter","dayofquarter(dt::TimeType) -> Int

   Returns the day of the current quarter of \"dt\". Range of value is
   1:92.

"),

("Dates","trunc","trunc([T], x[, digits[, base]])

   \"trunc(x)\" returns the nearest integral value of the same type as
   \"x\" whose absolute value is less than or equal to \"x\".

   \"trunc(T, x)\" converts the result to type \"T\", throwing an
   \"InexactError\" if the value is not representable.

   \"digits\" and \"base\" work as for \"round()\".

"),

("Dates","firstdayofweek","firstdayofweek(dt::TimeType) -> TimeType

   Adjusts \"dt\" to the Monday of its week.

"),

("Dates","lastdayofweek","lastdayofweek(dt::TimeType) -> TimeType

   Adjusts \"dt\" to the Sunday of its week.

"),

("Dates","firstdayofmonth","firstdayofmonth(dt::TimeType) -> TimeType

   Adjusts \"dt\" to the first day of its month.

"),

("Dates","lastdayofmonth","lastdayofmonth(dt::TimeType) -> TimeType

   Adjusts \"dt\" to the last day of its month.

"),

("Dates","firstdayofyear","firstdayofyear(dt::TimeType) -> TimeType

   Adjusts \"dt\" to the first day of its year.

"),

("Dates","lastdayofyear","lastdayofyear(dt::TimeType) -> TimeType

   Adjusts \"dt\" to the last day of its year.

"),

("Dates","firstdayofquarter","firstdayofquarter(dt::TimeType) -> TimeType

   Adjusts \"dt\" to the first day of its quarter.

"),

("Dates","lastdayofquarter","lastdayofquarter(dt::TimeType) -> TimeType

   Adjusts \"dt\" to the last day of its quarter.

"),

("Dates","tonext","tonext(func::Function, dt::TimeType;step=Day(1), negate=false, limit=10000, same=false) -> TimeType

   Adjusts \"dt\" by iterating at most \"limit\" iterations by
   \"step\" increments until \"func\" returns true. \"func\" must take
   a single \"TimeType\" argument and return a \"Bool\". \"same\"
   allows \"dt\" to be considered in satisfying \"func\". \"negate\"
   will make the adjustment process terminate when \"func\" returns
   false instead of true.

"),

("Dates","toprev","toprev(func::Function, dt::TimeType;step=Day(-1), negate=false, limit=10000, same=false) -> TimeType

   Adjusts \"dt\" by iterating at most \"limit\" iterations by
   \"step\" increments until \"func\" returns true. \"func\" must take
   a single \"TimeType\" argument and return a \"Bool\". \"same\"
   allows \"dt\" to be considered in satisfying \"func\". \"negate\"
   will make the adjustment process terminate when \"func\" returns
   false instead of true.

"),

("Dates","tofirst","tofirst(dt::TimeType, dow::Int;of=Month) -> TimeType

   Adjusts \"dt\" to the first \"dow\" of its month. Alternatively,
   \"of=Year\" will adjust to the first \"dow\" of the year.

"),

("Dates","tolast","tolast(dt::TimeType, dow::Int;of=Month) -> TimeType

   Adjusts \"dt\" to the last \"dow\" of its month. Alternatively,
   \"of=Year\" will adjust to the last \"dow\" of the year.

"),

("Dates","tonext","tonext(func::Function, dt::TimeType;step=Day(1), negate=false, limit=10000, same=false) -> TimeType

   Adjusts \"dt\" by iterating at most \"limit\" iterations by
   \"step\" increments until \"func\" returns true. \"func\" must take
   a single \"TimeType\" argument and return a \"Bool\". \"same\"
   allows \"dt\" to be considered in satisfying \"func\". \"negate\"
   will make the adjustment process terminate when \"func\" returns
   false instead of true.

"),

("Dates","toprev","toprev(func::Function, dt::TimeType;step=Day(-1), negate=false, limit=10000, same=false) -> TimeType

   Adjusts \"dt\" by iterating at most \"limit\" iterations by
   \"step\" increments until \"func\" returns true. \"func\" must take
   a single \"TimeType\" argument and return a \"Bool\". \"same\"
   allows \"dt\" to be considered in satisfying \"func\". \"negate\"
   will make the adjustment process terminate when \"func\" returns
   false instead of true.

"),

("Dates","recur{T<:TimeType}","recur{T<:TimeType}(func::Function, dr::StepRange{T};negate=false, limit=10000) -> Vector{T}

   \"func\" takes a single TimeType argument and returns a \"Bool\"
   indicating whether the input should be \"included\" in the final
   set. \"recur\" applies \"func\" over each element in the range of
   \"dr\", including those elements for which \"func\" returns
   \"true\" in the resulting Array, unless \"negate=true\", then only
   elements where \"func\" returns \"false\" are included.

"),

("Dates","Year","Year(v)

   Month(v) Week(v) Day(v) Hour(v) Minute(v) Second(v) Millisecond(v)

   Construct a \"Period\" type with the given \"v\" value. Input must
   be losslessly convertible to an \"Int64\".

"),

("Dates","default","default(p::Period) -> Period

   Returns a sensible \"default\" value for the input Period by
   returning \"one(p)\" for Year, Month, and Day, and \"zero(p)\" for
   Hour, Minute, Second, and Millisecond.

"),

("Dates","today","today() -> Date

   Returns the date portion of \"now()\".

"),

("Dates","unix2datetime","unix2datetime(x) -> DateTime

   Takes the number of seconds since unix epoch
   \"1970-01-01T00:00:00\" and converts to the corresponding DateTime.

"),

("Dates","datetime2unix","datetime2unix(dt::DateTime) -> Float64

   Takes the given DateTime and returns the number of seconds since
   the unix epoch as a \"Float64\".

"),

("Dates","julian2datetime","julian2datetime(julian_days) -> DateTime

   Takes the number of Julian calendar days since epoch
   \"-4713-11-24T12:00:00\" and returns the corresponding DateTime.

"),

("Dates","datetime2julian","datetime2julian(dt::DateTime) -> Float64

   Takes the given DateTime and returns the number of Julian calendar
   days since the julian epoch as a \"Float64\".

"),

("Dates","rata2datetime","rata2datetime(days) -> DateTime

   Takes the number of Rata Die days since epoch
   \"0000-12-31T00:00:00\" and returns the corresponding DateTime.

"),

("Dates","datetime2rata","datetime2rata(dt::TimeType) -> Int64

   Returns the number of Rata Die days since epoch from the given Date
   or DateTime.

"),

("Base","pwd","pwd() -> AbstractString

   Get the current working directory.

"),

("Base","cd","cd(f[, dir])

   Temporarily changes the current working directory (HOME if not
   specified) and applies function f before returning.

"),

("Base","cd","cd(f[, dir])

   Temporarily changes the current working directory (HOME if not
   specified) and applies function f before returning.

"),

("Base","readdir","readdir([dir]) -> Vector{ByteString}

   Returns the files and directories in the directory *dir* (or the
   current working directory if not given).

"),

("Base","mkdir","mkdir(path[, mode])

   Make a new directory with name \"path\" and permissions \"mode\".
   \"mode\" defaults to 0o777, modified by the current file creation
   mask.

"),

("Base","mkpath","mkpath(path[, mode])

   Create all directories in the given \"path\", with permissions
   \"mode\". \"mode\" defaults to 0o777, modified by the current file
   creation mask.

"),

("Base","symlink","symlink(target, link)

   Creates a symbolic link to \"target\" with the name \"link\".

   Note: This function raises an error under operating systems that
   do not support soft symbolic links, such as Windows XP.

"),

("Base","readlink","readlink(path) -> AbstractString

   Returns the value of a symbolic link \"path\".

"),

("Base","chmod","chmod(path, mode)

   Change the permissions mode of \"path\" to \"mode\". Only integer
   \"mode\"s (e.g. 0o777) are currently supported.

"),

("Base","stat","stat(file)

   Returns a structure whose fields contain information about the
   file. The fields of the structure are:

   +–––––-+––––––––––––––––––––––––––––––––––––+ | size      | The
   size (in bytes) of the file
   | +–––––-+––––––––––––––––––––––––––––––––––––+ | device    | ID of
   the device that contains the file                                |
   +–––––-+––––––––––––––––––––––––––––––––––––+ | inode     | The
   inode number of the file
   | +–––––-+––––––––––––––––––––––––––––––––––––+ | mode      | The
   protection mode of the file
   | +–––––-+––––––––––––––––––––––––––––––––––––+ | nlink     | The
   number of hard links to the file
   | +–––––-+––––––––––––––––––––––––––––––––––––+ | uid       | The
   user id of the owner of the file
   | +–––––-+––––––––––––––––––––––––––––––––––––+ | gid       | The
   group id of the file owner
   | +–––––-+––––––––––––––––––––––––––––––––––––+ | rdev      | If
   this file refers to a device, the ID of the device it refers to
   | +–––––-+––––––––––––––––––––––––––––––––––––+ | blksize   | The
   file-system preferred block size for the file
   | +–––––-+––––––––––––––––––––––––––––––––––––+ | blocks    | The
   number of such blocks allocated
   | +–––––-+––––––––––––––––––––––––––––––––––––+ | mtime     | Unix
   timestamp of when the file was last modified                      |
   +–––––-+––––––––––––––––––––––––––––––––––––+ | ctime     | Unix
   timestamp of when the file was created                            |
   +–––––-+––––––––––––––––––––––––––––––––––––+

"),

("Base","lstat","lstat(file)

   Like stat, but for symbolic links gets the info for the link itself
   rather than the file it refers to. This function must be called on
   a file path rather than a file object or a file descriptor.

"),

("Base","ctime","ctime(file)

   Equivalent to stat(file).ctime

"),

("Base","mtime","mtime(file)

   Equivalent to stat(file).mtime

"),

("Base","filemode","filemode(file)

   Equivalent to stat(file).mode

"),

("Base","filesize","filesize(path...)

   Equivalent to stat(file).size

"),

("Base","uperm","uperm(file)

   Gets the permissions of the owner of the file as a bitfield of

   +–––+–––––––––––-+ | 01   | Execute Permission    |
   +–––+–––––––––––-+ | 02   | Write Permission      |
   +–––+–––––––––––-+ | 04   | Read Permission       |
   +–––+–––––––––––-+

   For allowed arguments, see \"stat\".

"),

("Base","gperm","gperm(file)

   Like uperm but gets the permissions of the group owning the file

"),

("Base","operm","operm(file)

   Like uperm but gets the permissions for people who neither own the
   file nor are a member of the group owning the file

"),

("Base","cp","cp(src::AbstractString, dst::AbstractString; remove_destination::Bool=false, follow_symlinks::Bool=false)

   Copy the file, link, or directory from *src* to *dest*.
   \"remove_destination=true\" will first remove an existing *dst*.

   If *follow_symlinks=false*, and src is a symbolic link, dst will be
   created as a symbolic link. If *follow_symlinks=true* and src is a
   symbolic link, dst will be a copy of the file or directory *src*
   refers to.

"),

("Base","download","download(url[, localfile])

   Download a file from the given url, optionally renaming it to the
   given local file name. Note that this function relies on the
   availability of external tools such as \"curl\", \"wget\" or
   \"fetch\" to download the file and is provided for convenience. For
   production use or situations in which more options are need, please
   use a package that provides the desired functionality instead.

"),

("Base","mv","mv(src::AbstractString, dst::AbstractString; remove_destination::Bool=false)

   Move the file, link, or directory from *src* to *dest*.
   \"remove_destination=true\" will first remove an existing *dst*.

"),

("Base","rm","rm(path::AbstractString; recursive=false)

   Delete the file, link, or empty directory at the given path. If
   \"recursive=true\" is passed and the path is a directory, then all
   contents are removed recursively.

"),

("Base","touch","touch(path::AbstractString)

   Update the last-modified timestamp on a file to the current time.

"),

("Base","tempname","tempname()

   Generate a unique temporary file path.

"),

("Base","tempdir","tempdir()

   Obtain the path of a temporary directory (possibly shared with
   other processes).

"),

("Base","mktemp","mktemp([parent=tempdir()])

   Returns \"(path, io)\", where \"path\" is the path of a new
   temporary file in \"parent\" and \"io\" is an open file object for
   this path.

"),

("Base","mktempdir","mktempdir([parent=tempdir()])

   Create a temporary directory in the \"parent\" directory and return
   its path.

"),

("Base","isblockdev","isblockdev(path) -> Bool

   Returns \"true\" if \"path\" is a block device, \"false\"
   otherwise.

"),

("Base","ischardev","ischardev(path) -> Bool

   Returns \"true\" if \"path\" is a character device, \"false\"
   otherwise.

"),

("Base","isdir","isdir(path) -> Bool

   Returns \"true\" if \"path\" is a directory, \"false\" otherwise.

"),

("Base","isexecutable","isexecutable(path) -> Bool

   Returns \"true\" if the current user has permission to execute
   \"path\", \"false\" otherwise.

"),

("Base","isfifo","isfifo(path) -> Bool

   Returns \"true\" if \"path\" is a FIFO, \"false\" otherwise.

"),

("Base","isfile","isfile(path) -> Bool

   Returns \"true\" if \"path\" is a regular file, \"false\"
   otherwise.

"),

("Base","islink","islink(path) -> Bool

   Returns \"true\" if \"path\" is a symbolic link, \"false\"
   otherwise.

"),

("Base","ismount","ismount(path) -> Bool

   Returns \"true\" if \"path\" is a mount point, \"false\" otherwise.

"),

("Base","ispath","ispath(path) -> Bool

   Returns \"true\" if \"path\" is a valid filesystem path, \"false\"
   otherwise.

"),

("Base","isreadable","isreadable(path) -> Bool

   Returns \"true\" if the current user has permission to read
   \"path\", \"false\" otherwise.

"),

("Base","issetgid","issetgid(path) -> Bool

   Returns \"true\" if \"path\" has the setgid flag set, \"false\"
   otherwise.

"),

("Base","issetuid","issetuid(path) -> Bool

   Returns \"true\" if \"path\" has the setuid flag set, \"false\"
   otherwise.

"),

("Base","issocket","issocket(path) -> Bool

   Returns \"true\" if \"path\" is a socket, \"false\" otherwise.

"),

("Base","issticky","issticky(path) -> Bool

   Returns \"true\" if \"path\" has the sticky bit set, \"false\"
   otherwise.

"),

("Base","iswritable","iswritable(path) -> Bool

   Returns \"true\" if the current user has permission to write to
   \"path\", \"false\" otherwise.

"),

("Base","homedir","homedir() -> AbstractString

   Return the current user's home directory.

"),

("Base","dirname","dirname(path::AbstractString) -> AbstractString

   Get the directory part of a path.

"),

("Base","basename","basename(path::AbstractString) -> AbstractString

   Get the file name part of a path.

"),

("Base","@__FILE__","@__FILE__() -> AbstractString

   \"@__FILE__\" expands to a string with the absolute path and file
   name of the script being run. Returns \"nothing\" if run from a
   REPL or an empty string if evaluated by \"julia -e <expr>\".

"),

("Base","isabspath","isabspath(path::AbstractString) -> Bool

   Determines whether a path is absolute (begins at the root
   directory).

"),

("Base","isdirpath","isdirpath(path::AbstractString) -> Bool

   Determines whether a path refers to a directory (for example, ends
   with a path separator).

"),

("Base","joinpath","joinpath(parts...) -> AbstractString

   Join path components into a full path. If some argument is an
   absolute path, then prior components are dropped.

"),

("Base","abspath","abspath(path::AbstractString) -> AbstractString

   Convert a path to an absolute path by adding the current directory
   if necessary.

"),

("Base","normpath","normpath(path::AbstractString) -> AbstractString

   Normalize a path, removing \".\" and \"..\" entries.

"),

("Base","realpath","realpath(path::AbstractString) -> AbstractString

   Canonicalize a path by expanding symbolic links and removing \".\"
   and \"..\" entries.

"),

("Base","relpath","relpath(path::AbstractString, startpath::AbstractString = \".\") -> AbstractString

   Return a relative filepath to path either from the current
   directory or from an optional start directory. This is a path
   computation: the filesystem is not accessed to confirm the
   existence or nature of path or startpath.

"),

("Base","expanduser","expanduser(path::AbstractString) -> AbstractString

   On Unix systems, replace a tilde character at the start of a path
   with the current user's home directory.

"),

("Base","splitdir","splitdir(path::AbstractString) -> (AbstractString, AbstractString)

   Split a path into a tuple of the directory name and file name.

"),

("Base","splitdrive","splitdrive(path::AbstractString) -> (AbstractString, AbstractString)

   On Windows, split a path into the drive letter part and the path
   part. On Unix systems, the first component is always the empty
   string.

"),

("Base","splitext","splitext(path::AbstractString) -> (AbstractString, AbstractString)

   If the last component of a path contains a dot, split the path into
   everything before the dot and everything including and after the
   dot. Otherwise, return a tuple of the argument unmodified and the
   empty string.

"),


("Base","STDOUT","STDOUT

   Global variable referring to the standard out stream.

"),

("Base","STDERR","STDERR

   Global variable referring to the standard error stream.

"),

("Base","STDIN","STDIN

   Global variable referring to the standard input stream.

"),

("Base","open","open(f::function, args...)

   Apply the function \"f\" to the result of \"open(args...)\" and
   close the resulting file descriptor upon completion.

   **Example**: \"open(readall, \"file.txt\")\"

"),

("Base","open","open(f::function, args...)

   Apply the function \"f\" to the result of \"open(args...)\" and
   close the resulting file descriptor upon completion.

   **Example**: \"open(readall, \"file.txt\")\"

"),

("Base","open","open(f::function, args...)

   Apply the function \"f\" to the result of \"open(args...)\" and
   close the resulting file descriptor upon completion.

   **Example**: \"open(readall, \"file.txt\")\"

"),

("Base","IOBuffer","IOBuffer([data][, readable, writable[, maxsize]])

   Create an IOBuffer, which may optionally operate on a pre-existing
   array. If the readable/writable arguments are given, they restrict
   whether or not the buffer may be read from or written to
   respectively. By default the buffer is readable but not writable.
   The last argument optionally specifies a size beyond which the
   buffer may not be grown.

"),

("Base","IOBuffer","IOBuffer([data][, readable, writable[, maxsize]])

   Create an IOBuffer, which may optionally operate on a pre-existing
   array. If the readable/writable arguments are given, they restrict
   whether or not the buffer may be read from or written to
   respectively. By default the buffer is readable but not writable.
   The last argument optionally specifies a size beyond which the
   buffer may not be grown.

"),

("Base","IOBuffer","IOBuffer([data][, readable, writable[, maxsize]])

   Create an IOBuffer, which may optionally operate on a pre-existing
   array. If the readable/writable arguments are given, they restrict
   whether or not the buffer may be read from or written to
   respectively. By default the buffer is readable but not writable.
   The last argument optionally specifies a size beyond which the
   buffer may not be grown.

"),

("Base","IOBuffer","IOBuffer([data][, readable, writable[, maxsize]])

   Create an IOBuffer, which may optionally operate on a pre-existing
   array. If the readable/writable arguments are given, they restrict
   whether or not the buffer may be read from or written to
   respectively. By default the buffer is readable but not writable.
   The last argument optionally specifies a size beyond which the
   buffer may not be grown.

"),

("Base","takebuf_array","takebuf_array(b::IOBuffer)

   Obtain the contents of an \"IOBuffer\" as an array, without
   copying. Afterwards, the IOBuffer is reset to its initial state.

"),

("Base","takebuf_string","takebuf_string(b::IOBuffer)

   Obtain the contents of an \"IOBuffer\" as a string, without
   copying. Afterwards, the IOBuffer is reset to its initial state.

"),

("Base","fdio","fdio([name::AbstractString], fd::Integer[, own::Bool]) -> IOStream

   Create an \"IOStream\" object from an integer file descriptor. If
   \"own\" is true, closing this object will close the underlying
   descriptor. By default, an \"IOStream\" is closed when it is
   garbage collected. \"name\" allows you to associate the descriptor
   with a named file.

"),

("Base","flush","flush(stream)

   Commit all currently buffered writes to the given stream.

"),

("Base","close","close(stream)

   Close an I/O stream. Performs a \"flush\" first.

"),

("Base","write","write(stream, x)

   Write the canonical binary representation of a value to the given
   stream.

"),

("Base","read","read(stream, type, dims)

   Read a series of values of the given type from a stream, in
   canonical binary representation. \"dims\" is either a tuple or a
   series of integer arguments specifying the size of \"Array\" to
   return.

"),

("Base","read","read(stream, type, dims)

   Read a series of values of the given type from a stream, in
   canonical binary representation. \"dims\" is either a tuple or a
   series of integer arguments specifying the size of \"Array\" to
   return.

"),

("Base","read!","read!(stream, array::Array)

   Read binary data from a stream, filling in the argument \"array\".

"),

("Base","readbytes!","readbytes!(stream, b::Vector{UInt8}, nb=length(b))

   Read at most \"nb\" bytes from the stream into \"b\", returning the
   number of bytes read (increasing the size of \"b\" as needed).

"),

("Base","readbytes","readbytes(stream, nb=typemax(Int))

   Read at most \"nb\" bytes from the stream, returning a
   \"Vector{UInt8}\" of the bytes read.

"),

("Base","position","position(s)

   Get the current position of a stream.

"),

("Base","seek","seek(s, pos)

   Seek a stream to the given position.

"),

("Base","seekstart","seekstart(s)

   Seek a stream to its beginning.

"),

("Base","seekend","seekend(s)

   Seek a stream to its end.

"),

("Base","skip","skip(s, offset)

   Seek a stream relative to the current position.

"),

("Base","mark","mark(s)

   Add a mark at the current position of stream \"s\".  Returns the
   marked position.

   See also \"unmark()\", \"reset()\", \"ismarked()\"

"),

("Base","unmark","unmark(s)

   Remove a mark from stream \"s\". Returns \"true\" if the stream was
   marked, \"false\" otherwise.

   See also \"mark()\", \"reset()\", \"ismarked()\"

"),

("Base","reset","reset(s)

   Reset a stream \"s\" to a previously marked position, and remove
   the mark. Returns the previously marked position. Throws an error
   if the stream is not marked.

   See also \"mark()\", \"unmark()\", \"ismarked()\"

"),

("Base","ismarked","ismarked(s)

   Returns true if stream \"s\" is marked.

   See also \"mark()\", \"unmark()\", \"reset()\"

"),

("Base","eof","eof(stream) -> Bool

   Tests whether an I/O stream is at end-of-file. If the stream is not
   yet exhausted, this function will block to wait for more data if
   necessary, and then return \"false\". Therefore it is always safe
   to read one byte after seeing \"eof\" return \"false\". \"eof\"
   will return \"false\" as long as buffered data is still available,
   even if the remote end of a connection is closed.

"),

("Base","isreadonly","isreadonly(stream) -> Bool

   Determine whether a stream is read-only.

"),

("Base","isopen","isopen(stream) -> Bool

   Determine whether a stream is open (i.e. has not been closed yet).
   If the connection has been closed remotely (in case of e.g. a
   socket), \"isopen\" will return \"false\" even though buffered data
   may still be available. Use \"eof\" to check if necessary.

"),

("Base","serialize","serialize(stream, value)

   Write an arbitrary value to a stream in an opaque format, such that
   it can be read back by \"deserialize\". The read-back value will be
   as identical as possible to the original. In general, this process
   will not work if the reading and writing are done by different
   versions of Julia, or an instance of Julia with a different system
   image.

"),

("Base","deserialize","deserialize(stream)

   Read a value written by \"serialize\".

"),

("Base","print_escaped","print_escaped(io, str::AbstractString, esc::AbstractString)

   General escaping of traditional C and Unicode escape sequences,
   plus any characters in esc are also escaped (with a backslash).

"),

("Base","print_unescaped","print_unescaped(io, s::AbstractString)

   General unescaping of traditional C and Unicode escape sequences.
   Reverse of \"print_escaped()\".

"),

("Base","print_joined","print_joined(io, items, delim[, last])

   Print elements of \"items\" to \"io\" with \"delim\" between them.
   If \"last\" is specified, it is used as the final delimiter instead
   of \"delim\".

"),

("Base","print_shortest","print_shortest(io, x)

   Print the shortest possible representation, with the minimum number
   of consecutive non-zero digits, of number \"x\", ensuring that it
   would parse to the exact same number.

"),

("Base","fd","fd(stream)

   Returns the file descriptor backing the stream or file. Note that
   this function only applies to synchronous *File*'s and *IOStream*'s
   not to any of the asynchronous streams.

"),

("Base","redirect_stdout","redirect_stdout(stream)

   Replace STDOUT by stream for all C and julia level output to
   STDOUT. Note that *stream* must be a TTY, a Pipe or a TcpSocket.

"),

("Base","redirect_stdout","redirect_stdout(stream)

   Replace STDOUT by stream for all C and julia level output to
   STDOUT. Note that *stream* must be a TTY, a Pipe or a TcpSocket.

"),

("Base","redirect_stderr","redirect_stderr([stream])

   Like redirect_stdout, but for STDERR

"),

("Base","redirect_stdin","redirect_stdin([stream])

   Like redirect_stdout, but for STDIN. Note that the order of the
   return tuple is still (rd,wr), i.e. data to be read from STDIN, may
   be written to wr.

"),

("Base","readchomp","readchomp(x)

   Read the entirety of x as a string but remove trailing newlines.
   Equivalent to chomp(readall(x)).

"),

("Base","truncate","truncate(file, n)

   Resize the file or buffer given by the first argument to exactly
   *n* bytes, filling previously unallocated space with '0' if the
   file or buffer is grown

"),

("Base","skipchars","skipchars(stream, predicate; linecomment::Char)

   Advance the stream until before the first character for which
   \"predicate\" returns false. For example \"skipchars(stream,
   isspace)\" will skip all whitespace. If keyword argument
   \"linecomment\" is specified, characters from that character
   through the end of a line will also be skipped.

"),

("Base","countlines","countlines(io[, eol::Char])

   Read io until the end of the stream/file and count the number of
   non-empty lines. To specify a file pass the filename as the first
   argument. EOL markers other than 'n' are supported by passing them
   as the second argument.

"),

("Base","PipeBuffer","PipeBuffer(data::Vector{UInt8}[, maxsize])

   Create a PipeBuffer to operate on a data vector, optionally
   specifying a size beyond which the underlying Array may not be
   grown.

"),

("Base","PipeBuffer","PipeBuffer(data::Vector{UInt8}[, maxsize])

   Create a PipeBuffer to operate on a data vector, optionally
   specifying a size beyond which the underlying Array may not be
   grown.

"),

("Base","readavailable","readavailable(stream)

   Read all available data on the stream, blocking the task only if no
   data is available. The result is a \"Vector{UInt8,1}\".

"),

("Base","show","show(x)

   Write an informative text representation of a value to the current
   output stream. New types should overload \"show(io, x)\" where the
   first argument is a stream. The representation used by \"show\"
   generally includes Julia-specific formatting and type information.

"),

("Base","showcompact","showcompact(x)

   Show a more compact representation of a value. This is used for
   printing array elements. If a new type has a different compact
   representation, it should overload \"showcompact(io, x)\" where the
   first argument is a stream.

"),

("Base","showall","showall(x)

   Similar to \"show\", except shows all elements of arrays.

"),

("Base","summary","summary(x)

   Return a string giving a brief description of a value. By default
   returns \"string(typeof(x))\". For arrays, returns strings like
   \"2x2 Float64 Array\".

"),

("Base","print","print(x)

   Write (to the default output stream) a canonical (un-decorated)
   text representation of a value if there is one, otherwise call
   \"show\". The representation used by \"print\" includes minimal
   formatting and tries to avoid Julia-specific details.

"),

("Base","println","println(x)

   Print (using \"print()\") \"x\" followed by a newline.

"),

("Base","print_with_color","print_with_color(color::Symbol[, io], strings...)

   Print strings in a color specified as a symbol, for example
   \":red\" or \":blue\".

"),

("Base","info","info(msg)

   Display an informational message.

"),

("Base","warn","warn(msg)

   Display a warning.

"),

("Base","@printf","@printf([io::IOStream], \"%Fmt\", args...)

   Print arg(s) using C \"printf()\" style format specification
   string. Optionally, an IOStream may be passed as the first argument
   to redirect output.

"),

("Base","@sprintf","@sprintf(\"%Fmt\", args...)

   Return \"@printf\" formatted output as string.

"),

("Base","sprint","sprint(f::Function, args...)

   Call the given function with an I/O stream and the supplied extra
   arguments. Everything written to this I/O stream is returned as a
   string.

"),

("Base","showerror","showerror(io, e)

   Show a descriptive representation of an exception object.

"),

("Base","dump","dump(x)

   Show all user-visible structure of a value.

"),

("Base","xdump","xdump(x)

   Show all structure of a value, including all fields of objects.

"),

("Base","readall","readall(filename::AbstractString)

   Open \"filename\", read the entire contents as a string, then close
   the file. Equivalent to \"open(readall, filename)\".

"),

("Base","readall","readall(filename::AbstractString)

   Open \"filename\", read the entire contents as a string, then close
   the file. Equivalent to \"open(readall, filename)\".

"),

("Base","readline","readline(stream=STDIN)

   Read a single line of text, including a trailing newline character
   (if one is reached before the end of the input), from the given
   \"stream\" (defaults to \"STDIN\"),

"),

("Base","readuntil","readuntil(stream, delim)

   Read a string, up to and including the given delimiter byte.

"),

("Base","readlines","readlines(stream)

   Read all lines as an array.

"),

("Base","eachline","eachline(stream)

   Create an iterable object that will yield each line from a stream.

"),

("Base","readdlm","readdlm(source; options...)

   The columns are assumed to be separated by one or more whitespaces.
   The end of line delimiter is taken as \"n\". If all data is
   numeric, the result will be a numeric array. If some elements
   cannot be parsed as numbers, a cell array of numbers and strings is
   returned.

"),

("Base","readdlm","readdlm(source; options...)

   The columns are assumed to be separated by one or more whitespaces.
   The end of line delimiter is taken as \"n\". If all data is
   numeric, the result will be a numeric array. If some elements
   cannot be parsed as numbers, a cell array of numbers and strings is
   returned.

"),

("Base","readdlm","readdlm(source; options...)

   The columns are assumed to be separated by one or more whitespaces.
   The end of line delimiter is taken as \"n\". If all data is
   numeric, the result will be a numeric array. If some elements
   cannot be parsed as numbers, a cell array of numbers and strings is
   returned.

"),

("Base","readdlm","readdlm(source; options...)

   The columns are assumed to be separated by one or more whitespaces.
   The end of line delimiter is taken as \"n\". If all data is
   numeric, the result will be a numeric array. If some elements
   cannot be parsed as numbers, a cell array of numbers and strings is
   returned.

"),

("Base","readdlm","readdlm(source; options...)

   The columns are assumed to be separated by one or more whitespaces.
   The end of line delimiter is taken as \"n\". If all data is
   numeric, the result will be a numeric array. If some elements
   cannot be parsed as numbers, a cell array of numbers and strings is
   returned.

"),

("Base","readdlm","readdlm(source; options...)

   The columns are assumed to be separated by one or more whitespaces.
   The end of line delimiter is taken as \"n\". If all data is
   numeric, the result will be a numeric array. If some elements
   cannot be parsed as numbers, a cell array of numbers and strings is
   returned.

"),

("Base","writedlm","writedlm(f, A, delim='t')

   Write \"A\" (a vector, matrix or an iterable collection of iterable
   rows) as text to \"f\" (either a filename string or an \"IO\"
   stream) using the given delimeter \"delim\" (which defaults to tab,
   but can be any printable Julia object, typically a \"Char\" or
   \"AbstractString\").

   For example, two vectors \"x\" and \"y\" of the same length can be
   written as two columns of tab-delimited text to \"f\" by either
   \"writedlm(f, [x y])\" or by \"writedlm(f, zip(x, y))\".

"),

("Base","readcsv","readcsv(source, [T::Type]; options...)

   Equivalent to \"readdlm\" with \"delim\" set to comma.

"),

("Base","writecsv","writecsv(filename, A)

   Equivalent to \"writedlm\" with \"delim\" set to comma.

"),

("Base","Base64EncodePipe","Base64EncodePipe(ostream)

   Returns a new write-only I/O stream, which converts any bytes
   written to it into base64-encoded ASCII bytes written to
   \"ostream\".  Calling \"close\" on the \"Base64Pipe\" stream is
   necessary to complete the encoding (but does not close
   \"ostream\").

"),

("Base","Base64DecodePipe","Base64DecodePipe(istream)

   Returns a new read-only I/O stream, which decodes base64-encoded
   data read from \"istream\".

"),

("Base","base64encode","base64encode(writefunc, args...)

   base64encode(args...)

   Given a \"write\"-like function \"writefunc\", which takes an I/O
   stream as its first argument, \"base64(writefunc, args...)\" calls
   \"writefunc\" to write \"args...\" to a base64-encoded string, and
   returns the string.  \"base64(args...)\" is equivalent to
   \"base64(write, args...)\": it converts its arguments into bytes
   using the standard \"write\" functions and returns the
   base64-encoded string.

"),

("Base","base64decode","base64decode(string)

   Decodes the base64-encoded \"string\" and returns a
   \"Vector{UInt8}\" of the decoded bytes.

"),

("Base","display","display(x)

   display(d::Display, x) display(mime, x) display(d::Display, mime,
   x)

   Display \"x\" using the topmost applicable display in the display
   stack, typically using the richest supported multimedia output for
   \"x\", with plain-text \"STDOUT\" output as a fallback.  The
   \"display(d, x)\" variant attempts to display \"x\" on the given
   display \"d\" only, throwing a \"MethodError\" if \"d\" cannot
   display objects of this type.

   There are also two variants with a \"mime\" argument (a MIME type
   string, such as \"\"image/png\"\"), which attempt to display \"x\"
   using the requested MIME type *only*, throwing a \"MethodError\" if
   this type is not supported by either the display(s) or by \"x\".
   With these variants, one can also supply the \"raw\" data in the
   requested MIME type by passing \"x::AbstractString\" (for MIME
   types with text-based storage, such as text/html or
   application/postscript) or \"x::Vector{UInt8}\" (for binary MIME
   types).

"),

("Base","redisplay","redisplay(x)

   redisplay(d::Display, x) redisplay(mime, x) redisplay(d::Display,
   mime, x)

   By default, the \"redisplay\" functions simply call \"display\".
   However, some display backends may override \"redisplay\" to modify
   an existing display of \"x\" (if any).   Using \"redisplay\" is
   also a hint to the backend that \"x\" may be redisplayed several
   times, and the backend may choose to defer the display until (for
   example) the next interactive prompt.

"),

("Base","displayable","displayable(mime) -> Bool

   displayable(d::Display, mime) -> Bool

   Returns a boolean value indicating whether the given \"mime\" type
   (string) is displayable by any of the displays in the current
   display stack, or specifically by the display \"d\" in the second
   variant.

"),

("Base","writemime","writemime(stream, mime, x)

   The \"display\" functions ultimately call \"writemime\" in order to
   write an object \"x\" as a given \"mime\" type to a given I/O
   \"stream\" (usually a memory buffer), if possible.  In order to
   provide a rich multimedia representation of a user-defined type
   \"T\", it is only necessary to define a new \"writemime\" method
   for \"T\", via: \"writemime(stream, ::MIME\"mime\", x::T) = ...\",
   where \"mime\" is a MIME-type string and the function body calls
   \"write\" (or similar) to write that representation of \"x\" to
   \"stream\". (Note that the \"MIME\"\"\" notation only supports
   literal strings; to construct \"MIME\" types in a more flexible
   manner use \"MIME{symbol(\"\")}\".)

   For example, if you define a \"MyImage\" type and know how to write
   it to a PNG file, you could define a function \"writemime(stream,
   ::MIME\"image/png\", x::MyImage) = ...`\" to allow your images to
   be displayed on any PNG-capable \"Display\" (such as IJulia). As
   usual, be sure to \"import Base.writemime\" in order to add new
   methods to the built-in Julia function \"writemime\".

   Technically, the \"MIME\"mime\"\" macro defines a singleton type
   for the given \"mime\" string, which allows us to exploit Julia's
   dispatch mechanisms in determining how to display objects of any
   given type.

"),

("Base","mimewritable","mimewritable(mime, x)

   Returns a boolean value indicating whether or not the object \"x\"
   can be written as the given \"mime\" type.  (By default, this is
   determined automatically by the existence of the corresponding
   \"writemime\" function for \"typeof(x)\".)

"),

("Base","reprmime","reprmime(mime, x)

   Returns an \"AbstractString\" or \"Vector{UInt8}\" containing the
   representation of \"x\" in the requested \"mime\" type, as written
   by \"writemime\" (throwing a \"MethodError\" if no appropriate
   \"writemime\" is available).  An \"AbstractString\" is returned for
   MIME types with textual representations (such as \"\"text/html\"\"
   or \"\"application/postscript\"\"), whereas binary data is returned
   as \"Vector{UInt8}\".  (The function \"istext(mime)\" returns
   whether or not Julia treats a given \"mime\" type as text.)

   As a special case, if \"x\" is an \"AbstractString\" (for textual
   MIME types) or a \"Vector{UInt8}\" (for binary MIME types), the
   \"reprmime\" function assumes that \"x\" is already in the
   requested \"mime\" format and simply returns \"x\".

"),

("Base","stringmime","stringmime(mime, x)

   Returns an \"AbstractString\" containing the representation of
   \"x\" in the requested \"mime\" type.  This is similar to
   \"reprmime\" except that binary data is base64-encoded as an ASCII
   string.

"),

("Base","pushdisplay","pushdisplay(d::Display)

   Pushes a new display \"d\" on top of the global display-backend
   stack.  Calling \"display(x)\" or \"display(mime, x)\" will display
   \"x\" on the topmost compatible backend in the stack (i.e., the
   topmost backend that does not throw a \"MethodError\").

"),

("Base","popdisplay","popdisplay()

   popdisplay(d::Display)

   Pop the topmost backend off of the display-backend stack, or the
   topmost copy of \"d\" in the second variant.

"),

("Base","TextDisplay","TextDisplay(stream)

   Returns a \"TextDisplay <: Display\", which can display any object
   as the text/plain MIME type (only), writing the text representation
   to the given I/O stream.  (The text representation is the same as
   the way an object is printed in the Julia REPL.)

"),

("Base","istext","istext(m::MIME)

   Determine whether a MIME type is text data.

"),

("Base","mmap_array","mmap_array(type, dims, stream[, offset])

   Create an \"Array\" whose values are linked to a file, using
   memory-mapping. This provides a convenient way of working with data
   too large to fit in the computer's memory.

   The type determines how the bytes of the array are interpreted.
   Note that the file must be stored in binary format, and no format
   conversions are possible (this is a limitation of operating
   systems, not Julia).

   \"dims\" is a tuple specifying the size of the array.

   The file is passed via the stream argument.  When you initialize
   the stream, use \"\"r\"\" for a \"read-only\" array, and \"\"w+\"\"
   to create a new array used to write values to disk.

   Optionally, you can specify an offset (in bytes) if, for example,
   you want to skip over a header in the file. The default value for
   the offset is the current stream position.

   For example, the following code:

      # Create a file for mmapping
      # (you could alternatively use mmap_array to do this step, too)
      A = rand(1:20, 5, 30)
      s = open(\"/tmp/mmap.bin\", \"w+\")
      # We'll write the dimensions of the array as the first two Ints in the file
      write(s, size(A,1))
      write(s, size(A,2))
      # Now write the data
      write(s, A)
      close(s)

      # Test by reading it back in
      s = open(\"/tmp/mmap.bin\")   # default is read-only
      m = read(s, Int)
      n = read(s, Int)
      A2 = mmap_array(Int, (m,n), s)

   creates a \"m\"-by-\"n\" \"Matrix{Int}\", linked to the file
   associated with stream \"s\".

   A more portable file would need to encode the word size–-32 bit or
   64 bit–-and endianness information in the header. In practice,
   consider encoding binary data using standard formats like HDF5
   (which can be used with memory-mapping).

"),

("Base","mmap_bitarray","mmap_bitarray([type], dims, stream[, offset])

   Create a \"BitArray\" whose values are linked to a file, using
   memory-mapping; it has the same purpose, works in the same way, and
   has the same arguments, as \"mmap_array()\", but the byte
   representation is different. The \"type\" parameter is optional,
   and must be \"Bool\" if given.

   **Example**:  \"B = mmap_bitarray((25,30000), s)\"

   This would create a 25-by-30000 \"BitArray\", linked to the file
   associated with stream \"s\".

"),

("Base","msync","msync(ptr, len[, flags])

   Forces synchronization of the \"mmap()\"ped memory region from
   \"ptr\" to \"ptr+len\". Flags defaults to \"MS_SYNC\", but can be a
   combination of \"MS_ASYNC\", \"MS_SYNC\", or \"MS_INVALIDATE\". See
   your platform man page for specifics. The flags argument is not
   valid on Windows.

   You may not need to call \"msync\", because synchronization is
   performed at intervals automatically by the operating system.
   However, you can call this directly if, for example, you are
   concerned about losing the result of a long-running calculation.

"),

("Base","connect","connect(manager::FooManager, pid::Int, config::WorkerConfig) -> (instrm::AsyncStream, outstrm::AsyncStream)

   Implemented by cluster managers using custom transports. It should
   establish a logical connection to worker with id \"pid\", specified
   by \"config\" and return a pair of \"AsyncStream\" objects.
   Messages from \"pid\" to current process will be read off
   \"instrm\", while messages to be sent to \"pid\" will be written to
   \"outstrm\". The custom transport implementation must ensure that
   messages are delivered and received completely and in order.
   \"Base.connect(manager::ClusterManager.....)\" sets up TCP/IP
   socket connections in-between workers.

"),

("Base","connect","connect(manager::FooManager, pid::Int, config::WorkerConfig) -> (instrm::AsyncStream, outstrm::AsyncStream)

   Implemented by cluster managers using custom transports. It should
   establish a logical connection to worker with id \"pid\", specified
   by \"config\" and return a pair of \"AsyncStream\" objects.
   Messages from \"pid\" to current process will be read off
   \"instrm\", while messages to be sent to \"pid\" will be written to
   \"outstrm\". The custom transport implementation must ensure that
   messages are delivered and received completely and in order.
   \"Base.connect(manager::ClusterManager.....)\" sets up TCP/IP
   socket connections in-between workers.

"),

("Base","listen","listen(path) -> PipeServer

   Listens on/Creates a Named Pipe/Domain Socket

"),

("Base","listen","listen(path) -> PipeServer

   Listens on/Creates a Named Pipe/Domain Socket

"),

("Base","getaddrinfo","getaddrinfo(host)

   Gets the IP address of the \"host\" (may have to do a DNS lookup)

"),

("Base","parseip","parseip(addr)

   Parse a string specifying an IPv4 or IPv6 ip address.

"),

("Base","IPv4","IPv4(host::Integer) -> IPv4

   Returns IPv4 object from ip address formatted as Integer

"),

("Base","IPv6","IPv6(host::Integer) -> IPv6

   Returns IPv6 object from ip address formatted as Integer

"),

("Base","nb_available","nb_available(stream)

   Returns the number of bytes available for reading before a read
   from this stream or buffer will block.

"),

("Base","accept","accept(server[, client])

   Accepts a connection on the given server and returns a connection
   to the client. An uninitialized client stream may be provided, in
   which case it will be used instead of creating a new stream.

"),

("Base","listenany","listenany(port_hint) -> (UInt16, TcpServer)

   Create a TcpServer on any port, using hint as a starting point.
   Returns a tuple of the actual port that the server was created on
   and the server itself.

"),

("Base","watch_file","watch_file(cb=false, s; poll=false)

   Watch file or directory \"s\" and run callback \"cb\" when \"s\" is
   modified. The \"poll\" parameter specifies whether to use file
   system event monitoring or polling. The callback function \"cb\"
   should accept 3 arguments: \"(filename, events, status)\" where
   \"filename\" is the name of file that was modified, \"events\" is
   an object with boolean fields \"changed\" and \"renamed\" when
   using file system event monitoring, or \"readable\" and
   \"writable\" when using polling, and \"status\" is always 0. Pass
   \"false\" for \"cb\" to not use a callback function.

"),

("Base","poll_fd","poll_fd(fd, seconds::Real; readable=false, writable=false)

   Poll a file descriptor fd for changes in the read or write
   availability and with a timeout given by the second argument. If
   the timeout is not needed, use \"wait(fd)\" instead. The keyword
   arguments determine which of read and/or write status should be
   monitored and at least one of them needs to be set to true. The
   returned value is an object with boolean fields \"readable\",
   \"writable\", and \"timedout\", giving the result of the polling.

"),

("Base","poll_file","poll_file(s, interval_seconds::Real, seconds::Real)

   Monitor a file for changes by polling every *interval_seconds*
   seconds for *seconds* seconds. A return value of true indicates the
   file changed, a return value of false indicates a timeout.

"),

("Base","bind","bind(socket::Union{UDPSocket, TCPSocket}, host::IPv4, port::Integer)

   Bind \"socket\" to the given \"host:port\". Note that *0.0.0.0*
   will listen on all devices.

"),

("Base","send","send(socket::UDPSocket, host::IPv4, port::Integer, msg)

   Send \"msg\" over \"socket to >>``<<host:port\".

"),

("Base","recv","recv(socket::UDPSocket)

   Read a UDP packet from the specified socket, and return the bytes
   received. This call blocks.

"),

("Base","recvfrom","recvfrom(socket::UDPSocket) -> (address, data)

   Read a UDP packet from the specified socket, returning a tuple of
   (address, data), where address will be either IPv4 or IPv6 as
   appropriate.

"),

("Base","setopt","setopt(sock::UDPSocket; multicast_loop = nothing, multicast_ttl=nothing, enable_broadcast=nothing, ttl=nothing)

   Set UDP socket options. \"multicast_loop\": loopback for multicast
   packets (default: true). \"multicast_ttl\": TTL for multicast
   packets. \"enable_broadcast\": flag must be set to true if socket
   will be used for broadcast messages, or else the UDP system will
   return an access error (default: false). \"ttl\": Time-to-live of
   packets sent on the socket.

"),

("Base","ntoh","ntoh(x)

   Converts the endianness of a value from Network byte order (big-
   endian) to that used by the Host.

"),

("Base","hton","hton(x)

   Converts the endianness of a value from that used by the Host to
   Network byte order (big-endian).

"),

("Base","ltoh","ltoh(x)

   Converts the endianness of a value from Little-endian to that used
   by the Host.

"),

("Base","htol","htol(x)

   Converts the endianness of a value from that used by the Host to
   Little-endian.

"),

("Base","ENDIAN_BOM","ENDIAN_BOM

   The 32-bit byte-order-mark indicates the native byte order of the
   host machine. Little-endian machines will contain the value
   0x04030201. Big-endian machines will contain the value 0x01020304.

"),

("Libc","malloc","malloc(size::Integer) -> Ptr{Void}

   Call \"malloc\" from the C standard library.

"),

("Libc","calloc","calloc(num::Integer, size::Integer) -> Ptr{Void}

   Call \"calloc\" from the C standard library.

"),

("Libc","realloc","realloc(addr::Ptr, size::Integer) -> Ptr{Void}

   Call \"realloc\" from the C standard library.

   See warning in the documentation for \"free\" regarding only using
   this on memory originally obtained from \"malloc\".

"),

("Libc","free","free(addr::Ptr)

   Call \"free\" from the C standard library. Only use this on memory
   obtained from \"malloc\", not on pointers retrieved from other C
   libraries. \"Ptr\" objects obtained from C libraries should be
   freed by the free functions defined in that library, to avoid
   assertion failures if multiple \"libc\" libraries exist on the
   system.

"),

("Libc","errno","errno([code])

   Get the value of the C library's \"errno\". If an argument is
   specified, it is used to set the value of \"errno\".

   The value of \"errno\" is only valid immediately after a \"ccall\"
   to a C library routine that sets it. Specifically, you cannot call
   \"errno\" at the next prompt in a REPL, because lots of code is
   executed between prompts.

"),

("Libc","strerror","strerror(n)

   Convert a system call error code to a descriptive string

"),

("Libc","time","time(t::TmStruct)

   Converts a \"TmStruct\" struct to a number of seconds since the
   epoch.

"),

("Libc","strftime","strftime([format], time)

   Convert time, given as a number of seconds since the epoch or a
   \"TmStruct\", to a formatted string using the given format.
   Supported formats are the same as those in the standard C library.

"),

("Libc","strptime","strptime([format], timestr)

   Parse a formatted time string into a \"TmStruct\" giving the
   seconds, minute, hour, date, etc. Supported formats are the same as
   those in the standard C library. On some platforms, timezones will
   not be parsed correctly. If the result of this function will be
   passed to \"time\" to convert it to seconds since the epoch, the
   \"isdst\" field should be filled in manually. Setting it to \"-1\"
   will tell the C library to use the current system settings to
   determine the timezone.

"),

("Libc","TmStruct","TmStruct([seconds])

   Convert a number of seconds since the epoch to broken-down format,
   with fields \"sec\", \"min\", \"hour\", \"mday\", \"month\",
   \"year\", \"wday\", \"yday\", and \"isdst\".

"),

("Libc","flush_cstdio","flush_cstdio()

   Flushes the C \"stdout\" and \"stderr\" streams (which may have
   been written to by external C code).

"),

("Libc","msync","msync(ptr, len[, flags])

   Forces synchronization of the \"mmap()\"ped memory region from
   \"ptr\" to \"ptr+len\". Flags defaults to \"MS_SYNC\", but can be a
   combination of \"MS_ASYNC\", \"MS_SYNC\", or \"MS_INVALIDATE\". See
   your platform man page for specifics. The flags argument is not
   valid on Windows.

   You may not need to call \"msync\", because synchronization is
   performed at intervals automatically by the operating system.
   However, you can call this directly if, for example, you are
   concerned about losing the result of a long-running calculation.

"),

("Libc","MS_ASYNC","MS_ASYNC

   Enum constant for \"msync()\". See your platform man page for
   details. (not available on Windows).

"),

("Libc","MS_SYNC","MS_SYNC

   Enum constant for \"msync()\". See your platform man page for
   details. (not available on Windows).

"),

("Libc","MS_INVALIDATE","MS_INVALIDATE

   Enum constant for \"msync()\". See your platform man page for
   details. (not available on Windows).

"),

("Libc","mmap","mmap(len, prot, flags, fd, offset)

   Low-level interface to the \"mmap\" system call. See the man page.

"),

("Libc","munmap","munmap(pointer, len)

   Low-level interface for unmapping memory (see the man page). With
   \"mmap_array()\" you do not need to call this directly; the memory
   is unmapped for you when the array goes out of scope.

"),

("Libdl","dlopen","dlopen(libfile::AbstractString[, flags::Integer])

   Load a shared library, returning an opaque handle.

   The optional flags argument is a bitwise-or of zero or more of
   \"RTLD_LOCAL\", \"RTLD_GLOBAL\", \"RTLD_LAZY\", \"RTLD_NOW\",
   \"RTLD_NODELETE\", \"RTLD_NOLOAD\", \"RTLD_DEEPBIND\", and
   \"RTLD_FIRST\".  These are converted to the corresponding flags of
   the POSIX (and/or GNU libc and/or MacOS) dlopen command, if
   possible, or are ignored if the specified functionality is not
   available on the current platform.  The default is
   \"RTLD_LAZY|RTLD_DEEPBIND|RTLD_LOCAL\".  An important usage of
   these flags, on POSIX platforms, is to specify
   \"RTLD_LAZY|RTLD_DEEPBIND|RTLD_GLOBAL\" in order for the library's
   symbols to be available for usage in other shared libraries, in
   situations where there are dependencies between shared libraries.

"),

("Libdl","dlopen_e","dlopen_e(libfile::AbstractString[, flags::Integer])

   Similar to \"dlopen()\", except returns a \"NULL\" pointer instead
   of raising errors.

"),

("Libdl","RTLD_DEEPBIND","RTLD_DEEPBIND

   Enum constant for \"dlopen()\". See your platform man page for
   details, if applicable.

"),

("Libdl","RTLD_FIRST","RTLD_FIRST

   Enum constant for \"dlopen()\". See your platform man page for
   details, if applicable.

"),

("Libdl","RTLD_GLOBAL","RTLD_GLOBAL

   Enum constant for \"dlopen()\". See your platform man page for
   details, if applicable.

"),

("Libdl","RTLD_LAZY","RTLD_LAZY

   Enum constant for \"dlopen()\". See your platform man page for
   details, if applicable.

"),

("Libdl","RTLD_LOCAL","RTLD_LOCAL

   Enum constant for \"dlopen()\". See your platform man page for
   details, if applicable.

"),

("Libdl","RTLD_NODELETE","RTLD_NODELETE

   Enum constant for \"dlopen()\". See your platform man page for
   details, if applicable.

"),

("Libdl","RTLD_NOLOAD","RTLD_NOLOAD

   Enum constant for \"dlopen()\". See your platform man page for
   details, if applicable.

"),

("Libdl","RTLD_NOW","RTLD_NOW

   Enum constant for \"dlopen()\". See your platform man page for
   details, if applicable.

"),

("Libdl","dlsym","dlsym(handle, sym)

   Look up a symbol from a shared library handle, return callable
   function pointer on success.

"),

("Libdl","dlsym_e","dlsym_e(handle, sym)

   Look up a symbol from a shared library handle, silently return NULL
   pointer on lookup failure.

"),

("Libdl","dlclose","dlclose(handle)

   Close shared library referenced by handle.

"),

("Libdl","find_library","find_library(names, locations)

   Searches for the first library in \"names\" in the paths in the
   \"locations\" list, \"DL_LOAD_PATH\", or system library paths (in
   that order) which can successfully be dlopen'd. On success, the
   return value will be one of the names (potentially prefixed by one
   of the paths in locations). This string can be assigned to a
   \"global const\" and used as the library name in future
   \"ccall\"'s. On failure, it returns the empty string.

"),

("Libdl","DL_LOAD_PATH","DL_LOAD_PATH

   When calling \"dlopen\", the paths in this list will be searched
   first, in order, before searching the system locations for a valid
   library handle.

"),

("Base","*","*(s, t)

   Concatenate strings. The \"*\" operator is an alias to this
   function.

      julia> \"Hello \" * \"world\"
      \"Hello world\"

"),

("Base","\\","\\(A, B)

   Matrix division using a polyalgorithm. For input matrices \"A\" and
   \"B\", the result \"X\" is such that \"A*X == B\" when \"A\" is
   square.  The solver that is used depends upon the structure of
   \"A\".  A direct solver is used for upper- or lower triangular
   \"A\".  For Hermitian \"A\" (equivalent to symmetric \"A\" for non-
   complex \"A\") the \"BunchKaufman\" factorization is used.
   Otherwise an LU factorization is used. For rectangular \"A\" the
   result is the minimum-norm least squares solution computed by a
   pivoted QR factorization of \"A\" and a rank estimate of A based on
   the R factor.

   When \"A\" is sparse, a similar polyalgorithm is used. For
   indefinite matrices, the LDLt factorization does not use pivoting
   during the numerical factorization and therefore the procedure can
   fail even for invertible matrices.

"),

("Base","dot","dot(x, y)

   ⋅(x, y)

   Compute the dot product. For complex vectors, the first vector is
   conjugated.

"),

("Base","vecdot","vecdot(x, y)

   For any iterable containers \"x\" and \"y\" (including arrays of
   any dimension) of numbers (or any element type for which \"dot\" is
   defined), compute the Euclidean dot product (the sum of
   \"dot(x[i],y[i])\") as if they were vectors.

"),

("Base","cross","cross(x, y)

   ×(x, y)

   Compute the cross product of two 3-vectors.

"),

("Base","factorize","factorize(A)

   Compute a convenient factorization (including LU, Cholesky, Bunch-
   Kaufman, LowerTriangular, UpperTriangular) of A, based upon the
   type of the input matrix. The return value can then be reused for
   efficient solving of multiple systems. For example:
   \"A=factorize(A); x=Ab; y=AC\".

"),

("Base","full","full(QRCompactWYQ[, thin=true]) -> Matrix

      Converts an orthogonal or unitary matrix stored as a
      \"QRCompactWYQ\" object, i.e. in the compact WY format
      [Bischof1987], to a dense matrix.

      Optionally takes a \"thin\" Boolean argument, which if \"true\"
      omits the columns that span the rows of \"R\" in the QR
      factorization that are zero. The resulting matrix is the \"Q\"
      in a thin QR factorization (sometimes called the reduced QR
      factorization).  If \"false\", returns a \"Q\" that spans all
      rows of \"R\" in its corresponding QR factorization.

   Reconstruct the matrix \"A\" from the factorization
   \"F=factorize(A)\".

"),

("Base","lu","lu(A) -> L, U, p

   Compute the LU factorization of \"A\", such that \"A[p,:] = L*U\".

"),

("Base","lufact","lufact(A[, pivot=Val{true}]) -> F

   Compute the LU factorization of \"A\". The return type of \"F\"
   depends on the type of \"A\". In most cases, if \"A\" is a subtype
   \"S\" of AbstractMatrix with an element type \"T`\" supporting
   \"+\", \"-\", \"*\" and \"/\" the return type is \"LU{T,S{T}}\". If
   pivoting is chosen (default) the element type should also support
   \"abs\" and \"<\". When \"A\" is sparse and have element of type
   \"Float32\", \"Float64\", \"Complex{Float32}\", or
   \"Complex{Float64}\" the return type is \"UmfpackLU\". Some
   examples are shown in the table below.

      +-------------------------+---------------------------+----------------------------------------------+
      | Type of input \"A\"     | Type of output \"F\"      | Relationship between \"F\" and \"A\"         |
      +-------------------------+---------------------------+----------------------------------------------+
      | \"Matrix()\"            | \"LU\"                    | \"F[:L]*F[:U] == A[F[:p], :]\"               |
      +-------------------------+---------------------------+----------------------------------------------+
      | \"Tridiagonal()\"       | \"LU{T,Tridiagonal{T}}\"  | N/A                                          |
      +-------------------------+---------------------------+----------------------------------------------+
      | \"SparseMatrixCSC()\"   | \"UmfpackLU\"             | \"F[:L]*F[:U] == F[:Rs] .* A[F[:p], F[:q]]\" |
      +-------------------------+---------------------------+----------------------------------------------+

   The individual components of the factorization \"F\" can be
   accessed by indexing:

      +-------------+-----------------------------------------+--------+--------------------------+---------------+
      | Component   | Description                             | \"LU\" | \"LU{T,Tridiagonal{T}}\" | \"UmfpackLU\" |
      +-------------+-----------------------------------------+--------+--------------------------+---------------+
      | \"F[:L]\"   | \"L\" (lower triangular) part of \"LU\" | ✓      |                          | ✓             |
      +-------------+-----------------------------------------+--------+--------------------------+---------------+
      | \"F[:U]\"   | \"U\" (upper triangular) part of \"LU\" | ✓      |                          | ✓             |
      +-------------+-----------------------------------------+--------+--------------------------+---------------+
      | \"F[:p]\"   | (right) permutation \"Vector\"          | ✓      |                          | ✓             |
      +-------------+-----------------------------------------+--------+--------------------------+---------------+
      | \"F[:P]\"   | (right) permutation \"Matrix\"          | ✓      |                          |               |
      +-------------+-----------------------------------------+--------+--------------------------+---------------+
      | \"F[:q]\"   | left permutation \"Vector\"             |        |                          | ✓             |
      +-------------+-----------------------------------------+--------+--------------------------+---------------+
      | \"F[:Rs]\"  | \"Vector\" of scaling factors           |        |                          | ✓             |
      +-------------+-----------------------------------------+--------+--------------------------+---------------+
      | \"F[:(:)]\" | \"(L,U,p,q,Rs)\" components             |        |                          | ✓             |
      +-------------+-----------------------------------------+--------+--------------------------+---------------+

      +--------------------+--------+--------------------------+---------------+
      | Supported function | \"LU\" | \"LU{T,Tridiagonal{T}}\" | \"UmfpackLU\" |
      +--------------------+--------+--------------------------+---------------+
      | \"/\"              | ✓      |                          |               |
      +--------------------+--------+--------------------------+---------------+
      | \"\\\\\"             | ✓      | ✓                        | ✓             |
      +--------------------+--------+--------------------------+---------------+
      | \"cond\"           | ✓      |                          | ✓             |
      +--------------------+--------+--------------------------+---------------+
      | \"det\"            | ✓      | ✓                        | ✓             |
      +--------------------+--------+--------------------------+---------------+
      | \"logdet\"         | ✓      | ✓                        |               |
      +--------------------+--------+--------------------------+---------------+
      | \"logabsdet\"      | ✓      | ✓                        |               |
      +--------------------+--------+--------------------------+---------------+
      | \"size\"           | ✓      | ✓                        |               |
      +--------------------+--------+--------------------------+---------------+

"),

("Base","lufact!","lufact!(A) -> LU

   \"lufact!\" is the same as \"lufact()\", but saves space by
   overwriting the input A, instead of creating a copy.  For sparse
   \"A\" the \"nzval\" field is not overwritten but the index fields,
   \"colptr\" and \"rowval\" are decremented in place, converting from
   1-based indices to 0-based indices.

"),

("Base","chol","chol(A[, LU]) -> F

   Compute the Cholesky factorization of a symmetric positive definite
   matrix \"A\" and return the matrix \"F\". If \"LU\" is \"Val{:U}\"
   (Upper), \"F\" is of type \"UpperTriangular\" and \"A = F'>>*<<F\".
   If \"LU\" is \"Val{:L}\" (Lower), \"F\" is of type
   \"LowerTriangular\" and \"A = F*F'\". \"LU\" defaults to
   \"Val{:U}\".

"),

("Base","cholfact","cholfact(A; shift=0, perm=Int[]) -> CHOLMOD.Factor

   Compute the Cholesky factorization of a sparse positive definite
   matrix \"A\". A fill-reducing permutation is used.  \"F =
   cholfact(A)\" is most frequently used to solve systems of equations
   with \"Fb\", but also the methods \"diag\", \"det\", \"logdet\" are
   defined for \"F\".  You can also extract individual factors from
   \"F\", using \"F[:L]\".  However, since pivoting is on by default,
   the factorization is internally represented as \"A ==
   P'>>*<<L*L'>>*<<P\" with a permutation matrix \"P\"; using just
   \"L\" without accounting for \"P\" will give incorrect answers.  To
   include the effects of permutation, it's typically preferable to
   extact \"combined\" factors like \"PtL = F[:PtL]\" (the equivalent
   of \"P'>>*<<L\") and \"LtP = F[:UP]\" (the equivalent of
   \"L'>>*<<P\").

   Setting optional \"shift\" keyword argument computes the
   factorization of \"A+shift*I\" instead of \"A\".  If the \"perm\"
   argument is nonempty, it should be a permutation of *1:size(A,1)*
   giving the ordering to use (instead of CHOLMOD's default AMD
   ordering).

   The function calls the C library CHOLMOD and many other functions
   from the library are wrapped but not exported.

"),

("Base","cholfact","cholfact(A; shift=0, perm=Int[]) -> CHOLMOD.Factor

   Compute the Cholesky factorization of a sparse positive definite
   matrix \"A\". A fill-reducing permutation is used.  \"F =
   cholfact(A)\" is most frequently used to solve systems of equations
   with \"Fb\", but also the methods \"diag\", \"det\", \"logdet\" are
   defined for \"F\".  You can also extract individual factors from
   \"F\", using \"F[:L]\".  However, since pivoting is on by default,
   the factorization is internally represented as \"A ==
   P'>>*<<L*L'>>*<<P\" with a permutation matrix \"P\"; using just
   \"L\" without accounting for \"P\" will give incorrect answers.  To
   include the effects of permutation, it's typically preferable to
   extact \"combined\" factors like \"PtL = F[:PtL]\" (the equivalent
   of \"P'>>*<<L\") and \"LtP = F[:UP]\" (the equivalent of
   \"L'>>*<<P\").

   Setting optional \"shift\" keyword argument computes the
   factorization of \"A+shift*I\" instead of \"A\".  If the \"perm\"
   argument is nonempty, it should be a permutation of *1:size(A,1)*
   giving the ordering to use (instead of CHOLMOD's default AMD
   ordering).

   The function calls the C library CHOLMOD and many other functions
   from the library are wrapped but not exported.

"),

("Base","cholfact!","cholfact!(A [,LU=:U [,pivot=Val{false}]][;tol=-1.0]) -> Cholesky

   \"cholfact!\" is the same as \"cholfact()\", but saves space by
   overwriting the input \"A\", instead of creating a copy.
   \"cholfact!\" can also reuse the symbolic factorization from a
   different matrix \"F\" with the same structure when used as:
   \"cholfact!(F::CholmodFactor, A)\".

"),

("Base","ldltfact","ldltfact(A; shift=0, perm=Int[]) -> CHOLMOD.Factor

   Compute the LDLt factorization of a sparse symmetric or Hermitian
   matrix \"A\". A fill-reducing permutation is used.  \"F =
   ldltfact(A)\" is most frequently used to solve systems of equations
   with \"Fb\", but also the methods \"diag\", \"det\", \"logdet\" are
   defined for \"F\". You can also extract individual factors from
   \"F\", using \"F[:L]\".  However, since pivoting is on by default,
   the factorization is internally represented as \"A ==
   P'>>*<<L*D*L'>>*<<P\" with a permutation matrix \"P\"; using just
   \"L\" without accounting for \"P\" will give incorrect answers.  To
   include the effects of permutation, it's typically preferable to
   extact \"combined\" factors like \"PtL = F[:PtL]\" (the equivalent
   of \"P'>>*<<L\") and \"LtP = F[:UP]\" (the equivalent of
   \"L'>>*<<P\").  The complete list of supported factors is \":L,
   :PtL, :D, :UP, :U, :LD, :DU, :PtLD, :DUP\".

   Setting optional \"shift\" keyword argument computes the
   factorization of \"A+shift*I\" instead of \"A\".  If the \"perm\"
   argument is nonempty, it should be a permutation of *1:size(A,1)*
   giving the ordering to use (instead of CHOLMOD's default AMD
   ordering).

   The function calls the C library CHOLMOD and many other functions
   from the library are wrapped but not exported.

"),

("Base","ldltfact","ldltfact(A; shift=0, perm=Int[]) -> CHOLMOD.Factor

   Compute the LDLt factorization of a sparse symmetric or Hermitian
   matrix \"A\". A fill-reducing permutation is used.  \"F =
   ldltfact(A)\" is most frequently used to solve systems of equations
   with \"Fb\", but also the methods \"diag\", \"det\", \"logdet\" are
   defined for \"F\". You can also extract individual factors from
   \"F\", using \"F[:L]\".  However, since pivoting is on by default,
   the factorization is internally represented as \"A ==
   P'>>*<<L*D*L'>>*<<P\" with a permutation matrix \"P\"; using just
   \"L\" without accounting for \"P\" will give incorrect answers.  To
   include the effects of permutation, it's typically preferable to
   extact \"combined\" factors like \"PtL = F[:PtL]\" (the equivalent
   of \"P'>>*<<L\") and \"LtP = F[:UP]\" (the equivalent of
   \"L'>>*<<P\").  The complete list of supported factors is \":L,
   :PtL, :D, :UP, :U, :LD, :DU, :PtLD, :DUP\".

   Setting optional \"shift\" keyword argument computes the
   factorization of \"A+shift*I\" instead of \"A\".  If the \"perm\"
   argument is nonempty, it should be a permutation of *1:size(A,1)*
   giving the ordering to use (instead of CHOLMOD's default AMD
   ordering).

   The function calls the C library CHOLMOD and many other functions
   from the library are wrapped but not exported.

"),

("Base","qr","qr(A[, pivot=Val{false}][;thin=true]) -> Q, R, [p]

   Compute the (pivoted) QR factorization of \"A\" such that either
   \"A = Q*R\" or \"A[:,p] = Q*R\". Also see \"qrfact\". The default
   is to compute a thin factorization. Note that \"R\" is not extended
   with zeros when the full \"Q\" is requested.

"),

("Base","qrfact","qrfact(A) -> SPQR.Factorization

   Compute the QR factorization of a sparse matrix \"A\". A fill-
   reducing permutation is used. The main application of this type is
   to solve least squares problems with \"\". The function calls the C
   library SPQR and a few additional functions from the library are
   wrapped but not exported.

"),

("Base","qrfact","qrfact(A) -> SPQR.Factorization

   Compute the QR factorization of a sparse matrix \"A\". A fill-
   reducing permutation is used. The main application of this type is
   to solve least squares problems with \"\". The function calls the C
   library SPQR and a few additional functions from the library are
   wrapped but not exported.

"),

("Base","qrfact!","qrfact!(A[, pivot=Val{false}])

   \"qrfact!\" is the same as \"qrfact()\" when A is a subtype of
   \"StridedMatrix\", but saves space by overwriting the input \"A\",
   instead of creating a copy.

"),

("Base","full","full(QRCompactWYQ[, thin=true]) -> Matrix

   Converts an orthogonal or unitary matrix stored as a
   \"QRCompactWYQ\" object, i.e. in the compact WY format
   [Bischof1987], to a dense matrix.

   Optionally takes a \"thin\" Boolean argument, which if \"true\"
   omits the columns that span the rows of \"R\" in the QR
   factorization that are zero. The resulting matrix is the \"Q\" in a
   thin QR factorization (sometimes called the reduced QR
   factorization).  If \"false\", returns a \"Q\" that spans all rows
   of \"R\" in its corresponding QR factorization.

"),

("Base","bkfact","bkfact(A) -> BunchKaufman

   Compute the Bunch-Kaufman [Bunch1977] factorization of a real
   symmetric or complex Hermitian matrix \"A\" and return a
   \"BunchKaufman\" object. The following functions are available for
   \"BunchKaufman\" objects: \"size\", \"\", \"inv\", \"issym\",
   \"ishermitian\".

"),

("Base","bkfact!","bkfact!(A) -> BunchKaufman

   \"bkfact!\" is the same as \"bkfact()\", but saves space by
   overwriting the input \"A\", instead of creating a copy.

"),

("Base","sqrtm","sqrtm(A)

   Compute the matrix square root of \"A\". If \"B = sqrtm(A)\", then
   \"B*B == A\" within roundoff error.

   \"sqrtm\" uses a polyalgorithm, computing the matrix square root
   using Schur factorizations (\"schurfact()\") unless it detects the
   matrix to be Hermitian or real symmetric, in which case it computes
   the matrix square root from an eigendecomposition (\"eigfact()\").
   In the latter situation for positive definite matrices, the matrix
   square root has \"Real\" elements, otherwise it has \"Complex\"
   elements.

"),

("Base","eig","eig(A, B) -> D, V

   Computes generalized eigenvalues and vectors of \"A\" with respect
   to \"B\".

   \"eig\" is a wrapper around \"eigfact()\", extracting all parts of
   the factorization to a tuple; where possible, using \"eigfact()\"
   is recommended.

"),

("Base","eig","eig(A, B) -> D, V

   Computes generalized eigenvalues and vectors of \"A\" with respect
   to \"B\".

   \"eig\" is a wrapper around \"eigfact()\", extracting all parts of
   the factorization to a tuple; where possible, using \"eigfact()\"
   is recommended.

"),

("Base","eigvals","eigvals(A,[irange,][vl,][vu])

   Returns the eigenvalues of \"A\". If \"A\" is \"Symmetric\",
   \"Hermitian\" or \"SymTridiagonal\", it is possible to calculate
   only a subset of the eigenvalues by specifying either a
   \"UnitRange\" \"irange\" covering indices of the sorted
   eigenvalues, or a pair \"vl\" and \"vu\" for the lower and upper
   boundaries of the eigenvalues.

   For general non-symmetric matrices it is possible to specify how
   the matrix is balanced before the eigenvector calculation. The
   option \"permute=true\" permutes the matrix to become closer to
   upper triangular, and \"scale=true\" scales the matrix by its
   diagonal elements to make rows and columns more equal in norm. The
   default is \"true\" for both options.

"),

("Base","eigmax","eigmax(A)

   Returns the largest eigenvalue of \"A\".

"),

("Base","eigmin","eigmin(A)

   Returns the smallest eigenvalue of \"A\".

"),

("Base","eigvecs","eigvecs(A, [eigvals,][permute=true,][scale=true]) -> Matrix

   Returns a matrix \"M\" whose columns are the eigenvectors of \"A\".
   (The \"k\"th eigenvector can be obtained from the slice \"M[:,
   k]\".) The \"permute\" and \"scale\" keywords are the same as for
   \"eigfact()\".

   For \"SymTridiagonal\" matrices, if the optional vector of
   eigenvalues \"eigvals\" is specified, returns the specific
   corresponding eigenvectors.

"),

("Base","eigfact","eigfact(A, B) -> GeneralizedEigen

   Computes the generalized eigenvalue decomposition of \"A\" and
   \"B\", returning a \"GeneralizedEigen\" factorization object \"F\"
   which contains the generalized eigenvalues in \"F[:values]\" and
   the generalized eigenvectors in the columns of the matrix
   \"F[:vectors]\". (The \"k\"th generalized eigenvector can be
   obtained from the slice \"F[:vectors][:, k]\".)

"),

("Base","eigfact","eigfact(A, B) -> GeneralizedEigen

   Computes the generalized eigenvalue decomposition of \"A\" and
   \"B\", returning a \"GeneralizedEigen\" factorization object \"F\"
   which contains the generalized eigenvalues in \"F[:values]\" and
   the generalized eigenvectors in the columns of the matrix
   \"F[:vectors]\". (The \"k\"th generalized eigenvector can be
   obtained from the slice \"F[:vectors][:, k]\".)

"),

("Base","eigfact!","eigfact!(A[, B])

   Same as \"eigfact()\", but saves space by overwriting the input
   \"A\" (and \"B\"), instead of creating a copy.

"),

("Base","hessfact","hessfact(A)

   Compute the Hessenberg decomposition of \"A\" and return a
   \"Hessenberg\" object. If \"F\" is the factorization object, the
   unitary matrix can be accessed with \"F[:Q]\" and the Hessenberg
   matrix with \"F[:H]\". When \"Q\" is extracted, the resulting type
   is the \"HessenbergQ\" object, and may be converted to a regular
   matrix with \"full()\".

"),

("Base","hessfact!","hessfact!(A)

   \"hessfact!\" is the same as \"hessfact()\", but saves space by
   overwriting the input A, instead of creating a copy.

"),

("Base","schurfact","schurfact(A, B) -> GeneralizedSchur

   Computes the Generalized Schur (or QZ) factorization of the
   matrices \"A\" and \"B\". The (quasi) triangular Schur factors can
   be obtained from the \"Schur\" object \"F\" with \"F[:S]\" and
   \"F[:T]\", the left unitary/orthogonal Schur vectors can be
   obtained with \"F[:left]\" or \"F[:Q]\" and the right
   unitary/orthogonal Schur vectors can be obtained with \"F[:right]\"
   or \"F[:Z]\" such that \"A=F[:left]*F[:S]*F[:right]'\" and
   \"B=F[:left]*F[:T]*F[:right]'\". The generalized eigenvalues of
   \"A\" and \"B\" can be obtained with \"F[:alpha]./F[:beta]\".

"),

("Base","schurfact!","schurfact!(A)

   Computes the Schur factorization of \"A\", overwriting \"A\" in the
   process. See \"schurfact()\"

"),

("Base","schur","schur(A, B) -> GeneralizedSchur[:S], GeneralizedSchur[:T], GeneralizedSchur[:Q], GeneralizedSchur[:Z]

   See \"schurfact()\"

"),

("Base","ordschur","ordschur(GS, select) -> GeneralizedSchur

   Reorders the Generalized Schur factorization of a Generalized Schur
   object.  See \"ordschur()\".

"),

("Base","ordschur!","ordschur!(GS, select) -> GeneralizedSchur

   Reorders the Generalized Schur factorization of a Generalized Schur
   object by overwriting the object with the new factorization.  See
   \"ordschur()\".

"),

("Base","ordschur","ordschur(GS, select) -> GeneralizedSchur

   Reorders the Generalized Schur factorization of a Generalized Schur
   object.  See \"ordschur()\".

"),

("Base","ordschur!","ordschur!(GS, select) -> GeneralizedSchur

   Reorders the Generalized Schur factorization of a Generalized Schur
   object by overwriting the object with the new factorization.  See
   \"ordschur()\".

"),

("Base","schurfact","schurfact(A, B) -> GeneralizedSchur

   Computes the Generalized Schur (or QZ) factorization of the
   matrices \"A\" and \"B\". The (quasi) triangular Schur factors can
   be obtained from the \"Schur\" object \"F\" with \"F[:S]\" and
   \"F[:T]\", the left unitary/orthogonal Schur vectors can be
   obtained with \"F[:left]\" or \"F[:Q]\" and the right
   unitary/orthogonal Schur vectors can be obtained with \"F[:right]\"
   or \"F[:Z]\" such that \"A=F[:left]*F[:S]*F[:right]'\" and
   \"B=F[:left]*F[:T]*F[:right]'\". The generalized eigenvalues of
   \"A\" and \"B\" can be obtained with \"F[:alpha]./F[:beta]\".

"),

("Base","schur","schur(A, B) -> GeneralizedSchur[:S], GeneralizedSchur[:T], GeneralizedSchur[:Q], GeneralizedSchur[:Z]

   See \"schurfact()\"

"),

("Base","ordschur","ordschur(GS, select) -> GeneralizedSchur

   Reorders the Generalized Schur factorization of a Generalized Schur
   object.  See \"ordschur()\".

"),

("Base","ordschur!","ordschur!(GS, select) -> GeneralizedSchur

   Reorders the Generalized Schur factorization of a Generalized Schur
   object by overwriting the object with the new factorization.  See
   \"ordschur()\".

"),

("Base","ordschur","ordschur(GS, select) -> GeneralizedSchur

   Reorders the Generalized Schur factorization of a Generalized Schur
   object.  See \"ordschur()\".

"),

("Base","ordschur!","ordschur!(GS, select) -> GeneralizedSchur

   Reorders the Generalized Schur factorization of a Generalized Schur
   object by overwriting the object with the new factorization.  See
   \"ordschur()\".

"),

("Base","svdfact","svdfact(A, B) -> GeneralizedSVD

   Compute the generalized SVD of \"A\" and \"B\", returning a
   \"GeneralizedSVD\" Factorization object \"F\", such that \"A =
   F[:U]*F[:D1]*F[:R0]*F[:Q]'\" and \"B =
   F[:V]*F[:D2]*F[:R0]*F[:Q]'\".

"),

("Base","svdfact!","svdfact!(A[, thin=true]) -> SVD

   \"svdfact!\" is the same as \"svdfact()\", but saves space by
   overwriting the input A, instead of creating a copy. If \"thin\" is
   \"true\", an economy mode decomposition is returned. The default is
   to produce a thin decomposition.

"),

("Base","svd","svd(A, B) -> U, V, Q, D1, D2, R0

   Wrapper around \"svdfact\" extracting all parts the factorization
   to a tuple. Direct use of \"svdfact\" is therefore generally more
   efficient. The function returns the generalized SVD of \"A\" and
   \"B\", returning \"U\", \"V\", \"Q\", \"D1\", \"D2\", and \"R0\"
   such that \"A = U*D1*R0*Q'\" and \"B = V*D2*R0*Q'\".

"),

("Base","svdvals","svdvals(A, B)

   Return only the singular values from the generalized singular value
   decomposition of \"A\" and \"B\".

"),

("Base","svdvals!","svdvals!(A)

   Returns the singular values of \"A\", while saving space by
   overwriting the input.

"),

("Base","svdfact","svdfact(A, B) -> GeneralizedSVD

   Compute the generalized SVD of \"A\" and \"B\", returning a
   \"GeneralizedSVD\" Factorization object \"F\", such that \"A =
   F[:U]*F[:D1]*F[:R0]*F[:Q]'\" and \"B =
   F[:V]*F[:D2]*F[:R0]*F[:Q]'\".

"),

("Base","svd","svd(A, B) -> U, V, Q, D1, D2, R0

   Wrapper around \"svdfact\" extracting all parts the factorization
   to a tuple. Direct use of \"svdfact\" is therefore generally more
   efficient. The function returns the generalized SVD of \"A\" and
   \"B\", returning \"U\", \"V\", \"Q\", \"D1\", \"D2\", and \"R0\"
   such that \"A = U*D1*R0*Q'\" and \"B = V*D2*R0*Q'\".

"),

("Base","svdvals","svdvals(A, B)

   Return only the singular values from the generalized singular value
   decomposition of \"A\" and \"B\".

"),

("Base","triu","triu(M, k)

   Returns the upper triangle of \"M\" starting from the \"k\"th
   superdiagonal.

"),

("Base","triu","triu(M, k)

   Returns the upper triangle of \"M\" starting from the \"k\"th
   superdiagonal.

"),

("Base","triu!","triu!(M, k)

   Returns the upper triangle of \"M\" starting from the \"k\"th
   superdiagonal, overwriting \"M\" in the process.

"),

("Base","triu!","triu!(M, k)

   Returns the upper triangle of \"M\" starting from the \"k\"th
   superdiagonal, overwriting \"M\" in the process.

"),

("Base","tril","tril(M, k)

   Returns the lower triangle of \"M\" starting from the \"k\"th
   subdiagonal.

"),

("Base","tril","tril(M, k)

   Returns the lower triangle of \"M\" starting from the \"k\"th
   subdiagonal.

"),

("Base","tril!","tril!(M, k)

   Returns the lower triangle of \"M\" starting from the \"k\"th
   subdiagonal, overwriting \"M\" in the process.

"),

("Base","tril!","tril!(M, k)

   Returns the lower triangle of \"M\" starting from the \"k\"th
   subdiagonal, overwriting \"M\" in the process.

"),

("Base","diagind","diagind(M[, k])

   A \"Range\" giving the indices of the \"k\"th diagonal of the
   matrix \"M\".

"),

("Base","diag","diag(M[, k])

   The \"k\"th diagonal of a matrix, as a vector. Use \"diagm\" to
   construct a diagonal matrix.

"),

("Base","diagm","diagm(v[, k])

   Construct a diagonal matrix and place \"v\" on the \"k\"th
   diagonal.

"),

("Base","scale","scale(b, A)

   Scale an array \"A\" by a scalar \"b\", returning a new array.

   If \"A\" is a matrix and \"b\" is a vector, then \"scale(A,b)\"
   scales each column \"i\" of \"A\" by \"b[i]\" (similar to
   \"A*diagm(b)\"), while \"scale(b,A)\" scales each row \"i\" of
   \"A\" by \"b[i]\" (similar to \"diagm(b)*A\"), returning a new
   array.

   Note: for large \"A\", \"scale\" can be much faster than \"A .* b\"
   or \"b .* A\", due to the use of BLAS.

"),

("Base","scale","scale(b, A)

   Scale an array \"A\" by a scalar \"b\", returning a new array.

   If \"A\" is a matrix and \"b\" is a vector, then \"scale(A,b)\"
   scales each column \"i\" of \"A\" by \"b[i]\" (similar to
   \"A*diagm(b)\"), while \"scale(b,A)\" scales each row \"i\" of
   \"A\" by \"b[i]\" (similar to \"diagm(b)*A\"), returning a new
   array.

   Note: for large \"A\", \"scale\" can be much faster than \"A .* b\"
   or \"b .* A\", due to the use of BLAS.

"),

("Base","scale!","scale!(b, A)

   Scale an array \"A\" by a scalar \"b\", similar to \"scale()\" but
   overwriting \"A\" in-place.

   If \"A\" is a matrix and \"b\" is a vector, then \"scale!(A,b)\"
   scales each column \"i\" of \"A\" by \"b[i]\" (similar to
   \"A*diagm(b)\"), while \"scale!(b,A)\" scales each row \"i\" of
   \"A\" by \"b[i]\" (similar to \"diagm(b)*A\"), again operating in-
   place on \"A\".

"),

("Base","scale!","scale!(b, A)

   Scale an array \"A\" by a scalar \"b\", similar to \"scale()\" but
   overwriting \"A\" in-place.

   If \"A\" is a matrix and \"b\" is a vector, then \"scale!(A,b)\"
   scales each column \"i\" of \"A\" by \"b[i]\" (similar to
   \"A*diagm(b)\"), while \"scale!(b,A)\" scales each row \"i\" of
   \"A\" by \"b[i]\" (similar to \"diagm(b)*A\"), again operating in-
   place on \"A\".

"),

("Base","Tridiagonal","Tridiagonal(dl, d, du)

   Construct a tridiagonal matrix from the lower diagonal, diagonal,
   and upper diagonal, respectively.  The result is of type
   \"Tridiagonal\" and provides efficient specialized linear solvers,
   but may be converted into a regular matrix with \"full()\".

"),

("Base","Bidiagonal","Bidiagonal(dv, ev, isupper)

   Constructs an upper (\"isupper=true\") or lower (\"isupper=false\")
   bidiagonal matrix using the given diagonal (\"dv\") and off-
   diagonal (\"ev\") vectors.  The result is of type \"Bidiagonal\"
   and provides efficient specialized linear solvers, but may be
   converted into a regular matrix with \"full()\".

"),

("Base","SymTridiagonal","SymTridiagonal(d, du)

   Construct a real symmetric tridiagonal matrix from the diagonal and
   upper diagonal, respectively. The result is of type
   \"SymTridiagonal\" and provides efficient specialized eigensolvers,
   but may be converted into a regular matrix with \"full()\".

"),

("Base","rank","rank(M)

   Compute the rank of a matrix.

"),

("Base","norm","norm(A[, p])

   Compute the \"p\"-norm of a vector or the operator norm of a matrix
   \"A\", defaulting to the \"p=2\"-norm.

   For vectors, \"p\" can assume any numeric value (even though not
   all values produce a mathematically valid vector norm). In
   particular, \"norm(A, Inf)\" returns the largest value in
   \"abs(A)\", whereas \"norm(A, -Inf)\" returns the smallest.

   For matrices, valid values of \"p\" are \"1\", \"2\", or \"Inf\".
   (Note that for sparse matrices, \"p=2\" is currently not
   implemented.) Use \"vecnorm()\" to compute the Frobenius norm.

"),

("Base","vecnorm","vecnorm(A[, p])

   For any iterable container \"A\" (including arrays of any
   dimension) of numbers (or any element type for which \"norm\" is
   defined), compute the \"p\"-norm (defaulting to \"p=2\") as if
   \"A\" were a vector of the corresponding length.

   For example, if \"A\" is a matrix and \"p=2\", then this is
   equivalent to the Frobenius norm.

"),

("Base","cond","cond(M[, p])

   Condition number of the matrix \"M\", computed using the operator
   \"p\"-norm. Valid values for \"p\" are \"1\", \"2\" (default), or
   \"Inf\".

"),

("Base","condskeel","condskeel(M[, x, p])

      condskeel(M[, x, p])

      \\kappa_S(M, p) & = \\left\\Vert \\left\\vert M \\right\\vert
      \\left\\vert M^{-1} \\right\\vert  \\right\\Vert_p \\\\
      \\kappa_S(M, x, p) & = \\left\\Vert \\left\\vert M \\right\\vert
      \\left\\vert M^{-1} \\right\\vert \\left\\vert x \\right\\vert
      \\right\\Vert_p

   Skeel condition number kappa_S of the matrix \"M\", optionally with
   respect to the vector \"x\", as computed using the operator
   \"p\"-norm. \"p\" is \"Inf\" by default, if not provided. Valid
   values for \"p\" are \"1\", \"2\", or \"Inf\".

   This quantity is also known in the literature as the Bauer
   condition number, relative condition number, or componentwise
   relative condition number.

"),

("Base","trace","trace(M)

   Matrix trace

"),

("Base","det","det(M)

   Matrix determinant

"),

("Base","logabsdet","logabsdet(M)

   Log of absolute value of determinant of real matrix. Equivalent to
   \"(log(abs(det(M))), sign(det(M)))\", but may provide increased
   accuracy and/or speed.

"),

("Base","logabsdet","logabsdet(M)

   Log of absolute value of determinant of real matrix. Equivalent to
   \"(log(abs(det(M))), sign(det(M)))\", but may provide increased
   accuracy and/or speed.

"),

("Base","inv","inv(M)

   Matrix inverse

"),

("Base","pinv","pinv(M[, tol])

   Computes the Moore-Penrose pseudoinverse.

   For matrices \"M\" with floating point elements, it is convenient
   to compute the pseudoinverse by inverting only singular values
   above a given threshold, \"tol\".

   The optimal choice of \"tol\" varies both with the value of \"M\"
   and the intended application of the pseudoinverse. The default
   value of \"tol\" is
   \"eps(real(float(one(eltype(M)))))*maximum(size(A))\", which is
   essentially machine epsilon for the real part of a matrix element
   multiplied by the larger matrix dimension. For inverting dense ill-
   conditioned matrices in a least-squares sense, \"tol =
   sqrt(eps(real(float(one(eltype(M))))))\" is recommended.

   For more information, see [8859], [B96], [S84], [KY88].

   [8859] Issue 8859, \"Fix least squares\",
   https://github.com/JuliaLang/julia/pull/8859

   [B96] Åke Björck, \"Numerical Methods for Least Squares
   Problems\", SIAM Press, Philadelphia, 1996, \"Other Titles in
   Applied Mathematics\", Vol. 51. doi:10.1137/1.9781611971484

   [S84] G. W. Stewart, \"Rank Degeneracy\", SIAM Journal on
   Scientific and Statistical Computing, 5(2), 1984, 403-413.
   doi:10.1137/0905030

   [KY88] Konstantinos Konstantinides and Kung Yao,
   \"Statistical analysis of effective singular values in
   matrix rank determination\", IEEE Transactions on Acoustics,
   Speech and Signal Processing, 36(5), 1988, 757-763.
   doi:10.1109/29.1585

"),

("Base","nullspace","nullspace(M)

   Basis for nullspace of \"M\".

"),

("Base","repmat","repmat(A, n, m)

   Construct a matrix by repeating the given matrix \"n\" times in
   dimension 1 and \"m\" times in dimension 2.

"),

("Base","repeat","repeat(A, inner = Int[], outer = Int[])

   Construct an array by repeating the entries of \"A\". The i-th
   element of \"inner\" specifies the number of times that the
   individual entries of the i-th dimension of \"A\" should be
   repeated. The i-th element of \"outer\" specifies the number of
   times that a slice along the i-th dimension of \"A\" should be
   repeated.

"),

("Base","kron","kron(A, B)

   Kronecker tensor product of two vectors or two matrices.

"),

("Base","blkdiag","blkdiag(A...)

   Concatenate matrices block-diagonally. Currently only implemented
   for sparse matrices.

"),

("Base","linreg","linreg(x, y, w)

   Weighted least-squares linear regression.

"),

("Base","linreg","linreg(x, y, w)

   Weighted least-squares linear regression.

"),

("Base","expm","expm(A)

   Matrix exponential.

"),

("Base","lyap","lyap(A, C)

   Computes the solution \"X\" to the continuous Lyapunov equation
   \"AX + XA' + C = 0\", where no eigenvalue of \"A\" has a zero real
   part and no two eigenvalues are negative complex conjugates of each
   other.

"),

("Base","sylvester","sylvester(A, B, C)

   Computes the solution \"X\" to the Sylvester equation \"AX + XB + C
   = 0\", where \"A\", \"B\" and \"C\" have compatible dimensions and
   \"A\" and \"-B\" have no eigenvalues with equal real part.

"),

("Base","issym","issym(A) -> Bool

   Test whether a matrix is symmetric.

"),

("Base","isposdef","isposdef(A) -> Bool

   Test whether a matrix is positive definite.

"),

("Base","isposdef!","isposdef!(A) -> Bool

   Test whether a matrix is positive definite, overwriting \"A\" in
   the processes.

"),

("Base","istril","istril(A) -> Bool

   Test whether a matrix is lower triangular.

"),

("Base","istriu","istriu(A) -> Bool

   Test whether a matrix is upper triangular.

"),

("Base","isdiag","isdiag(A) -> Bool

   Test whether a matrix is diagonal.

"),

("Base","ishermitian","ishermitian(A) -> Bool

   Test whether a matrix is Hermitian.

"),

("Base","transpose","transpose(A)

   The transposition operator (\".'\").

"),

("Base","transpose!","transpose!(dest, src)

   Transpose array \"src\" and store the result in the preallocated
   array \"dest\", which should have a size corresponding to
   \"(size(src,2),size(src,1))\". No in-place transposition is
   supported and unexpected results will happen if *src* and *dest*
   have overlapping memory regions.

"),

("Base","ctranspose","ctranspose(A)

   The conjugate transposition operator (\"'\").

"),

("Base","ctranspose!","ctranspose!(dest, src)

   Conjugate transpose array \"src\" and store the result in the
   preallocated array \"dest\", which should have a size corresponding
   to \"(size(src,2),size(src,1))\". No in-place transposition is
   supported and unexpected results will happen if *src* and *dest*
   have overlapping memory regions.

"),

("Base","eigs","eigs(A[, B], ; nev=6, which=\"LM\", tol=0.0, maxiter=300, sigma=nothing, ritzvec=true, v0=zeros((0, ))) -> (d[, v], nconv, niter, nmult, resid)

   Computes eigenvalues \"d\" of \"A\" using Lanczos or Arnoldi
   iterations for real symmetric or general nonsymmetric matrices
   respectively. If \"B\" is provided, the generalized eigenproblem is
   solved.

   The following keyword arguments are supported:     * \"nev\":
   Number of eigenvalues

      * \"ncv\": Number of Krylov vectors used in the computation;
        should satisfy

           \"nev+1 <= ncv <= n\" for real symmetric problems and
           \"nev+2 <= ncv <= n\" for other problems, where \"n\" is
           the size of the input matrix \"A\". The default is \"ncv =
           max(20,2*nev+1)\". Note that these restrictions limit the
           input matrix \"A\" to be of dimension at least 2.

      * \"which\": type of eigenvalues to compute. See the note
        below.

        +-----------+-----------------------------------------------------------------------------------------------------------------------------+
        | \"which\" | type of eigenvalues                                                                                                         |
        +-----------+-----------------------------------------------------------------------------------------------------------------------------+
        | \":LM\"   | eigenvalues of largest magnitude (default)                                                                                  |
        +-----------+-----------------------------------------------------------------------------------------------------------------------------+
        | \":SM\"   | eigenvalues of smallest magnitude                                                                                           |
        +-----------+-----------------------------------------------------------------------------------------------------------------------------+
        | \":LR\"   | eigenvalues of largest real part                                                                                            |
        +-----------+-----------------------------------------------------------------------------------------------------------------------------+
        | \":SR\"   | eigenvalues of smallest real part                                                                                           |
        +-----------+-----------------------------------------------------------------------------------------------------------------------------+
        | \":LI\"   | eigenvalues of largest imaginary part (nonsymmetric or complex \"A\" only)                                                  |
        +-----------+-----------------------------------------------------------------------------------------------------------------------------+
        | \":SI\"   | eigenvalues of smallest imaginary part (nonsymmetric or complex \"A\" only)                                                 |
        +-----------+-----------------------------------------------------------------------------------------------------------------------------+
        | \":BE\"   | compute half of the eigenvalues from each end of the spectrum, biased in favor of the high end. (real symmetric \"A\" only) |
        +-----------+-----------------------------------------------------------------------------------------------------------------------------+

      * \"tol\": tolerance (tol \\le 0.0 defaults to
        \"DLAMCH('EPS')\")

      * \"maxiter\": Maximum number of iterations (default = 300)

      * \"sigma\": Specifies the level shift used in inverse
        iteration. If \"nothing\" (default), defaults to ordinary
        (forward) iterations. Otherwise, find eigenvalues close to
        \"sigma\" using shift and invert iterations.

      * \"ritzvec\": Returns the Ritz vectors \"v\" (eigenvectors)
        if \"true\"

      * \"v0\": starting vector from which to start the iterations

   \"eigs\" returns the \"nev\" requested eigenvalues in \"d\", the
   corresponding Ritz vectors \"v\" (only if \"ritzvec=true\"), the
   number of converged eigenvalues \"nconv\", the number of iterations
   \"niter\" and the number of matrix vector multiplications
   \"nmult\", as well as the final residual vector \"resid\".

   Note: The \"sigma\" and \"which\" keywords interact: the
   description of eigenvalues searched for by \"which\" do _not_
   necessarily refer to the eigenvalues of \"A\", but rather the
   linear operator constructed by the specification of the iteration
   mode implied by \"sigma\".

   +––––––––-+––––––––––––––––––+––––––––––––––––––+   | \"sigma\"
   | iteration mode                     | \"which\" refers to
   eigenvalues of |
   +––––––––-+––––––––––––––––––+––––––––––––––––––+   | \"nothing\"
   | ordinary (forward)                 | A
   |   +––––––––-+––––––––––––––––––+––––––––––––––––––+   | real or
   complex | inverse with level shift \"sigma\" | (A - sigma I )^{-1}
   |   +––––––––-+––––––––––––––––––+––––––––––––––––––+

"),

("Base","svds","svds(A; nsv=6, ritzvec=true, tol=0.0, maxiter=1000) -> (left_sv, s, right_sv, nconv, niter, nmult, resid)

   \"svds\" computes largest singular values \"s\" of \"A\" using
   Lanczos or Arnoldi iterations. Uses \"eigs()\" underneath.

   Inputs are:     * \"A\": Linear operator. It can either subtype of
   \"AbstractArray\" (e.g., sparse matrix) or duck typed. For
   duck typing \"A\" has to support \"size(A)\", \"eltype(A)\",
   \"A * vector\" and \"A' * vector\".

      * \"nsv\": Number of singular values.

      * \"ritzvec\": Whether to return the left and right singular
        vectors \"left_sv\" and \"right_sv\", default is \"true\". If
        \"false\" the singular vectors are omitted from the output.

      * \"tol\": tolerance, see \"eigs()\".

      * \"maxiter\": Maximum number of iterations, see \"eigs()\".

   **Example**:

      X = sprand(10, 5, 0.2)
      svds(X, nsv = 2)

"),

("Base","peakflops","peakflops(n; parallel=false)

   \"peakflops\" computes the peak flop rate of the computer by using
   double precision \"Base.LinAlg.BLAS.gemm!()\". By default, if no
   arguments are specified, it multiplies a matrix of size \"n x n\",
   where \"n = 2000\". If the underlying BLAS is using multiple
   threads, higher flop rates are realized. The number of BLAS threads
   can be set with \"blas_set_num_threads(n)\".

   If the keyword argument \"parallel\" is set to \"true\",
   \"peakflops\" is run in parallel on all the worker processors. The
   flop rate of the entire parallel computer is returned. When running
   in parallel, only 1 BLAS thread is used. The argument \"n\" still
   refers to the size of the problem that is solved on each processor.

"),

("Base.LinAlg.BLAS","dot","dot(n, X, incx, Y, incy)

   Dot product of two vectors consisting of \"n\" elements of array
   \"X\" with stride \"incx\" and \"n\" elements of array \"Y\" with
   stride \"incy\".

"),

("Base.LinAlg.BLAS","dotu","dotu(n, X, incx, Y, incy)

   Dot function for two complex vectors.

"),

("Base.LinAlg.BLAS","dotc","dotc(n, X, incx, U, incy)

   Dot function for two complex vectors conjugating the first vector.

"),

("Base.LinAlg.BLAS","blascopy!","blascopy!(n, X, incx, Y, incy)

   Copy \"n\" elements of array \"X\" with stride \"incx\" to array
   \"Y\" with stride \"incy\".  Returns \"Y\".

"),

("Base.LinAlg.BLAS","nrm2","nrm2(n, X, incx)

   2-norm of a vector consisting of \"n\" elements of array \"X\" with
   stride \"incx\".

"),

("Base.LinAlg.BLAS","asum","asum(n, X, incx)

   sum of the absolute values of the first \"n\" elements of array
   \"X\" with stride \"incx\".

"),

("Base.LinAlg.BLAS","axpy!","axpy!(a, X, Y)

   Overwrite \"Y\" with \"a*X + Y\".  Returns \"Y\".

"),

("Base.LinAlg.BLAS","scal!","scal!(n, a, X, incx)

   Overwrite \"X\" with \"a*X\".  Returns \"X\".

"),

("Base.LinAlg.BLAS","scal","scal(n, a, X, incx)

   Returns \"a*X\".

"),

("Base.LinAlg.BLAS","ger!","ger!(alpha, x, y, A)

   Rank-1 update of the matrix \"A\" with vectors \"x\" and \"y\" as
   \"alpha*x*y' + A\".

"),

("Base.LinAlg.BLAS","syr!","syr!(uplo, alpha, x, A)

   Rank-1 update of the symmetric matrix \"A\" with vector \"x\" as
   \"alpha*x*x.' + A\".  When \"uplo\" is 'U' the upper triangle of
   \"A\" is updated ('L' for lower triangle). Returns \"A\".

"),

("Base.LinAlg.BLAS","syrk!","syrk!(uplo, trans, alpha, A, beta, C)

   Rank-k update of the symmetric matrix \"C\" as \"alpha*A*A.' +
   beta*C\" or \"alpha*A.'>>*<<A + beta*C\" according to whether
   \"trans\" is 'N' or 'T'.  When \"uplo\" is 'U' the upper triangle
   of \"C\" is updated ('L' for lower triangle).  Returns \"C\".

"),

("Base.LinAlg.BLAS","syrk","syrk(uplo, trans, alpha, A)

   Returns either the upper triangle or the lower triangle, according
   to \"uplo\" ('U' or 'L'), of \"alpha*A*A.'\" or
   \"alpha*A.'>>*<<A\", according to \"trans\" ('N' or 'T').

"),

("Base.LinAlg.BLAS","her!","her!(uplo, alpha, x, A)

   Methods for complex arrays only.  Rank-1 update of the Hermitian
   matrix \"A\" with vector \"x\" as \"alpha*x*x' + A\".  When
   \"uplo\" is 'U' the upper triangle of \"A\" is updated ('L' for
   lower triangle). Returns \"A\".

"),

("Base.LinAlg.BLAS","herk!","herk!(uplo, trans, alpha, A, beta, C)

   Methods for complex arrays only.  Rank-k update of the Hermitian
   matrix \"C\" as \"alpha*A*A' + beta*C\" or \"alpha*A'>>*<<A +
   beta*C\" according to whether \"trans\" is 'N' or 'T'.  When
   \"uplo\" is 'U' the upper triangle of \"C\" is updated ('L' for
   lower triangle). Returns \"C\".

"),

("Base.LinAlg.BLAS","herk","herk(uplo, trans, alpha, A)

   Methods for complex arrays only.  Returns either the upper triangle
   or the lower triangle, according to \"uplo\" ('U' or 'L'), of
   \"alpha*A*A'\" or \"alpha*A'>>*<<A\", according to \"trans\" ('N'
   or 'T').

"),

("Base.LinAlg.BLAS","gbmv!","gbmv!(trans, m, kl, ku, alpha, A, x, beta, y)

   Update vector \"y\" as \"alpha*A*x + beta*y\" or \"alpha*A'>>*<<x +
   beta*y\" according to \"trans\" ('N' or 'T').  The matrix \"A\" is
   a general band matrix of dimension \"m\" by \"size(A,2)\" with
   \"kl\" sub-diagonals and \"ku\" super-diagonals. Returns the
   updated \"y\".

"),

("Base.LinAlg.BLAS","gbmv","gbmv(trans, m, kl, ku, alpha, A, x, beta, y)

   Returns \"alpha*A*x\" or \"alpha*A'>>*<<x\" according to \"trans\"
   ('N' or 'T'). The matrix \"A\" is a general band matrix of
   dimension \"m\" by \"size(A,2)\" with \"kl\" sub-diagonals and
   \"ku\" super- diagonals.

"),

("Base.LinAlg.BLAS","sbmv!","sbmv!(uplo, k, alpha, A, x, beta, y)

   Update vector \"y\" as \"alpha*A*x + beta*y\" where \"A\" is a a
   symmetric band matrix of order \"size(A,2)\" with \"k\" super-
   diagonals stored in the argument \"A\".  The storage layout for
   \"A\" is described the reference BLAS module, level-2 BLAS at
   http://www.netlib.org/lapack/explore-html/.

   Returns the updated \"y\".

"),

("Base.LinAlg.BLAS","sbmv","sbmv(uplo, k, A, x)

   Returns \"A*x\" where \"A\" is a symmetric band matrix of order
   \"size(A,2)\" with \"k\" super-diagonals stored in the argument
   \"A\".

"),

("Base.LinAlg.BLAS","sbmv","sbmv(uplo, k, A, x)

   Returns \"A*x\" where \"A\" is a symmetric band matrix of order
   \"size(A,2)\" with \"k\" super-diagonals stored in the argument
   \"A\".

"),

("Base.LinAlg.BLAS","gemm!","gemm!(tA, tB, alpha, A, B, beta, C)

   Update \"C\" as \"alpha*A*B + beta*C\" or the other three variants
   according to \"tA\" (transpose \"A\") and \"tB\".  Returns the
   updated \"C\".

"),

("Base.LinAlg.BLAS","gemm","gemm(tA, tB, A, B)

   Returns \"A*B\" or the other three variants according to \"tA\"
   (transpose \"A\") and \"tB\".

"),

("Base.LinAlg.BLAS","gemm","gemm(tA, tB, A, B)

   Returns \"A*B\" or the other three variants according to \"tA\"
   (transpose \"A\") and \"tB\".

"),

("Base.LinAlg.BLAS","gemv!","gemv!(tA, alpha, A, x, beta, y)

   Update the vector \"y\" as \"alpha*A*x + beta*y\" or \"alpha*A'x +
   beta*y\" according to \"tA\" (transpose \"A\"). Returns the updated
   \"y\".

"),

("Base.LinAlg.BLAS","gemv","gemv(tA, A, x)

   Returns \"A*x\" or \"A'x\" according to \"tA\" (transpose \"A\").

"),

("Base.LinAlg.BLAS","gemv","gemv(tA, A, x)

   Returns \"A*x\" or \"A'x\" according to \"tA\" (transpose \"A\").

"),

("Base.LinAlg.BLAS","symm!","symm!(side, ul, alpha, A, B, beta, C)

   Update \"C\" as \"alpha*A*B + beta*C\" or \"alpha*B*A + beta*C\"
   according to \"side\". \"A\" is assumed to be symmetric.  Only the
   \"ul\" triangle of \"A\" is used.  Returns the updated \"C\".

"),

("Base.LinAlg.BLAS","symm","symm(tA, tB, alpha, A, B)

   Returns \"alpha*A*B\" or the other three variants according to
   \"tA\" (transpose \"A\") and \"tB\".

"),

("Base.LinAlg.BLAS","symm","symm(tA, tB, alpha, A, B)

   Returns \"alpha*A*B\" or the other three variants according to
   \"tA\" (transpose \"A\") and \"tB\".

"),

("Base.LinAlg.BLAS","symm","symm(tA, tB, alpha, A, B)

   Returns \"alpha*A*B\" or the other three variants according to
   \"tA\" (transpose \"A\") and \"tB\".

"),

("Base.LinAlg.BLAS","symv!","symv!(ul, alpha, A, x, beta, y)

   Update the vector \"y\" as \"alpha*A*x + beta*y\". \"A\" is assumed
   to be symmetric.  Only the \"ul\" triangle of \"A\" is used.
   Returns the updated \"y\".

"),

("Base.LinAlg.BLAS","symv","symv(ul, A, x)

   Returns \"A*x\".  \"A\" is assumed to be symmetric.  Only the
   \"ul\" triangle of \"A\" is used.

"),

("Base.LinAlg.BLAS","symv","symv(ul, A, x)

   Returns \"A*x\".  \"A\" is assumed to be symmetric.  Only the
   \"ul\" triangle of \"A\" is used.

"),

("Base.LinAlg.BLAS","trmm!","trmm!(side, ul, tA, dA, alpha, A, B)

   Update \"B\" as \"alpha*A*B\" or one of the other three variants
   determined by \"side\" (A on left or right) and \"tA\" (transpose
   A). Only the \"ul\" triangle of \"A\" is used.  \"dA\" indicates if
   \"A\" is unit-triangular (the diagonal is assumed to be all ones).
   Returns the updated \"B\".

"),

("Base.LinAlg.BLAS","trmm","trmm(side, ul, tA, dA, alpha, A, B)

   Returns \"alpha*A*B\" or one of the other three variants determined
   by \"side\" (A on left or right) and \"tA\" (transpose A). Only the
   \"ul\" triangle of \"A\" is used.  \"dA\" indicates if \"A\" is
   unit-triangular (the diagonal is assumed to be all ones).

"),

("Base.LinAlg.BLAS","trsm!","trsm!(side, ul, tA, dA, alpha, A, B)

   Overwrite \"B\" with the solution to \"A*X = alpha*B\" or one of
   the other three variants determined by \"side\" (A on left or right
   of \"X\") and \"tA\" (transpose A). Only the \"ul\" triangle of
   \"A\" is used.  \"dA\" indicates if \"A\" is unit-triangular (the
   diagonal is assumed to be all ones).  Returns the updated \"B\".

"),

("Base.LinAlg.BLAS","trsm","trsm(side, ul, tA, dA, alpha, A, B)

   Returns the solution to \"A*X = alpha*B\" or one of the other three
   variants determined by \"side\" (A on left or right of \"X\") and
   \"tA\" (transpose A). Only the \"ul\" triangle of \"A\" is used.
   \"dA\" indicates if \"A\" is unit-triangular (the diagonal is
   assumed to be all ones).

"),

("Base.LinAlg.BLAS","trmv!","trmv!(side, ul, tA, dA, alpha, A, b)

   Update \"b\" as \"alpha*A*b\" or one of the other three variants
   determined by \"side\" (A on left or right) and \"tA\" (transpose
   A). Only the \"ul\" triangle of \"A\" is used.  \"dA\" indicates if
   \"A\" is unit-triangular (the diagonal is assumed to be all ones).
   Returns the updated \"b\".

"),

("Base.LinAlg.BLAS","trmv","trmv(side, ul, tA, dA, alpha, A, b)

   Returns \"alpha*A*b\" or one of the other three variants determined
   by \"side\" (A on left or right) and \"tA\" (transpose A). Only the
   \"ul\" triangle of \"A\" is used.  \"dA\" indicates if \"A\" is
   unit-triangular (the diagonal is assumed to be all ones).

"),

("Base.LinAlg.BLAS","trsv!","trsv!(ul, tA, dA, A, b)

   Overwrite \"b\" with the solution to \"A*x = b\" or one of the
   other two variants determined by \"tA\" (transpose A) and \"ul\"
   (triangle of \"A\" used).  \"dA\" indicates if \"A\" is unit-
   triangular (the diagonal is assumed to be all ones).  Returns the
   updated \"b\".

"),

("Base.LinAlg.BLAS","trsv","trsv(ul, tA, dA, A, b)

   Returns the solution to \"A*x = b\" or one of the other two
   variants determined by \"tA\" (transpose A) and \"ul\" (triangle of
   \"A\" is used.) \"dA\" indicates if \"A\" is unit-triangular (the
   diagonal is assumed to be all ones).

"),

("Base.LinAlg.BLAS","blas_set_num_threads","blas_set_num_threads(n)

   Set the number of threads the BLAS library should use.

"),

("Base.LinAlg.BLAS","I","I

   An object of type \"UniformScaling\", representing an identity
   matrix of any size.

"),

("Base","-","-(x, y)

   Subtraction operator.

"),

("Base","+","+(x, y...)

   Addition operator. \"x+y+z+...\" calls this function with all
   arguments, i.e. \"+(x, y, z, ...)\".

"),

("Base","-","-(x, y)

   Subtraction operator.

"),

("Base","*","*(s, t)

   Concatenate strings. The \"*\" operator is an alias to this
   function.

      julia> \"Hello \" * \"world\"
      \"Hello world\"

"),

("Base","/","/(x, y)

   Right division operator: multiplication of \"x\" by the inverse of
   \"y\" on the right. Gives floating-point results for integer
   arguments.

"),

("Base","\\","\\(x, y)

   Left division operator: multiplication of \"y\" by the inverse of
   \"x\" on the left. Gives floating-point results for integer
   arguments.

"),

("Base","^","^(s, n)

   Repeat \"n\" times the string \"s\". The \"^\" operator is an alias
   to this function.

      julia> \"Test \"^3
      \"Test Test Test \"

"),

("Base",".+",".+(x, y)

   Element-wise addition operator.

"),

("Base",".-",".-(x, y)

   Element-wise subtraction operator.

"),

("Base",".*",".*(x, y)

   Element-wise multiplication operator.

"),

("Base","./","./(x, y)

   Element-wise right division operator.

"),

("Base",".\\",".\\(x, y)

   Element-wise left division operator.

"),

("Base",".^",".^(x, y)

   Element-wise exponentiation operator.

"),

("Base","fma","fma(x, y, z)

   Computes \"x*y+z\" without rounding the intermediate result
   \"x*y\". On some systems this is significantly more expensive than
   \"x*y+z\". \"fma\" is used to improve accuracy in certain
   algorithms. See \"muladd\".

"),

("Base","muladd","muladd(x, y, z)

   Combined multiply-add, computes \"x*y+z\" in an efficient manner.
   This may on some systems be equivalent to \"x*y+z\", or to
   \"fma(x,y,z)\". \"muladd\" is used to improve performance. See
   \"fma\".

"),

("Base","div","div(x, y)

   ÷(x, y)

   The quotient from Euclidean division. Computes \"x/y\", truncated
   to an integer.

"),

("Base","fld","fld(x, y)

   Largest integer less than or equal to \"x/y\".

"),

("Base","cld","cld(x, y)

   Smallest integer larger than or equal to \"x/y\".

"),

("Base","mod","mod(x, y)

   Modulus after division, returning in the range [0,``y``), if \"y\"
   is positive, or (\"y\",0] if \"y\" is negative.

"),

("Base","mod2pi","mod2pi(x)

   Modulus after division by 2pi, returning in the range [0,2pi).

   This function computes a floating point representation of the
   modulus after division by numerically exact 2pi, and is therefore
   not exactly the same as mod(x,2pi), which would compute the modulus
   of x relative to division by the floating-point number 2pi.

"),

("Base","rem","rem(x, y)

   %(x, y)

   Remainder from Euclidean division, returning a value of the same
   sign as``x``, and smaller in magnitude than \"y\". This value is
   always exact.

"),

("Base","divrem","divrem(x, y)

   The quotient and remainder from Euclidean division. Equivalent to
   \"(x÷y, x%y)\".

"),

("Base","fldmod","fldmod(x, y)

   The floored quotient and modulus after division. Equivalent to
   \"(fld(x,y), mod(x,y))\".

"),

("Base","mod1","mod1(x, m)

   Modulus after division, returning in the range (0,m]

"),

("Base","rem1","rem1(x, m)

   Remainder after division, returning in the range (0,m]

"),

("Base","//","//(num, den)

   Divide two integers or rational numbers, giving a \"Rational\"
   result.

"),

("Base","rationalize","rationalize([Type=Int], x; tol=eps(x))

   Approximate floating point number \"x\" as a Rational number with
   components of the given integer type. The result will differ from
   \"x\" by no more than \"tol\".

"),

("Base","num","num(x)

   Numerator of the rational representation of \"x\"

"),

("Base","den","den(x)

   Denominator of the rational representation of \"x\"

"),

("Base","<<","<<(x, n)

   Left bit shift operator.

"),

("Base",">>",">>(x, n)

   Right bit shift operator, preserving the sign of \"x\".

"),

("Base",">>>",">>>(x, n)

   Unsigned right bit shift operator.

"),

("Base",":",":(start[, step], stop)

   Range operator. \"a:b\" constructs a range from \"a\" to \"b\" with
   a step size of 1, and \"a:s:b\" is similar but uses a step size of
   \"s\". These syntaxes call the function \"colon\". The colon is
   also used in indexing to select whole dimensions.

"),

("Base","colon","colon(start[, step], stop)

   Called by \":\" syntax for constructing ranges.

"),

("Base","range","range(start[, step], length)

   Construct a range by length, given a starting value and optional
   step (defaults to 1).

"),

("Base","==","==(x, y)

   Generic equality operator, giving a single \"Bool\" result. Falls
   back to \"===\". Should be implemented for all types with a notion
   of equality, based on the abstract value that an instance
   represents. For example, all numeric types are compared by numeric
   value, ignoring type. Strings are compared as sequences of
   characters, ignoring encoding.

   Follows IEEE semantics for floating-point numbers.

   Collections should generally implement \"==\" by calling \"==\"
   recursively on all contents.

   New numeric types should implement this function for two arguments
   of the new type, and handle comparison to other types via promotion
   rules where possible.

"),

("Base","!=","!=(x, y)

   ≠(x, y)

   Not-equals comparison operator. Always gives the opposite answer as
   \"==\". New types should generally not implement this, and rely on
   the fallback definition \"!=(x,y) = !(x==y)\" instead.

"),

("Base","===","===(x, y)

   ≡(x, y)

   See the \"is()\" operator

"),

("Base","!==","!==(x, y)

   ≢(x, y)

   Equivalent to \"!is(x, y)\"

"),

("Base","<","<(x, y)

   Less-than comparison operator. New numeric types should implement
   this function for two arguments of the new type. Because of the
   behavior of floating-point NaN values, \"<\" implements a partial
   order. Types with a canonical partial order should implement \"<\",
   and types with a canonical total order should implement \"isless\".

"),

("Base","<=","<=(x, y)

   ≤(x, y)

   Less-than-or-equals comparison operator.

"),

("Base",">",">(x, y)

   Greater-than comparison operator. Generally, new types should
   implement \"<\" instead of this function, and rely on the fallback
   definition \">(x,y) = y<x\".

"),

("Base",">=",">=(x, y)

   ≥(x, y)

   Greater-than-or-equals comparison operator.

"),

("Base",".==",".==(x, y)

   Element-wise equality comparison operator.

"),

("Base",".!=",".!=(x, y)

   .≠(x, y)

   Element-wise not-equals comparison operator.

"),

("Base",".<",".<(x, y)

   Element-wise less-than comparison operator.

"),

("Base",".<=",".<=(x, y)

   .≤(x, y)

   Element-wise less-than-or-equals comparison operator.

"),

("Base",".>",".>(x, y)

   Element-wise greater-than comparison operator.

"),

("Base",".>=",".>=(x, y)

   .≥(x, y)

   Element-wise greater-than-or-equals comparison operator.

"),

("Base","cmp","cmp(x, y)

   Return -1, 0, or 1 depending on whether \"x\" is less than, equal
   to, or greater than \"y\", respectively. Uses the total order
   implemented by \"isless\". For floating-point numbers, uses \"<\"
   but throws an error for unordered arguments.

"),

("Base","~","~(x)

   Bitwise not

"),

("Base","&","&(x, y)

   Bitwise and

"),

("Base","|","|(x, y)

   Bitwise or

"),

("Base","\$","\$(x, y)

   Bitwise exclusive or

"),

("Base","!","!(x)

   Boolean not

"),

("","x && y","x && y

   Short-circuiting boolean and

"),

("","x || y","x || y

   Short-circuiting boolean or

"),

("Base","A_ldiv_Bc","A_ldiv_Bc(a, b)

   Matrix operator A B^H

"),

("Base","A_ldiv_Bt","A_ldiv_Bt(a, b)

   Matrix operator A B^T

"),

("Base","A_mul_B!","A_mul_B!(Y, A, B) -> Y

   Calculates the matrix-matrix or matrix-vector product *A B* and
   stores the result in *Y*, overwriting the existing value of *Y*.

      julia> A=[1.0 2.0; 3.0 4.0]; B=[1.0 1.0; 1.0 1.0]; A_mul_B!(B, A, B);

      julia> B
      2x2 Array{Float64,2}:
       3.0  3.0
       7.0  7.0

"),

("Base","A_mul_Bc","A_mul_Bc(...)

   Matrix operator A B^H

"),

("Base","A_mul_Bt","A_mul_Bt(...)

   Matrix operator A B^T

"),

("Base","A_rdiv_Bc","A_rdiv_Bc(...)

   Matrix operator A / B^H

"),

("Base","A_rdiv_Bt","A_rdiv_Bt(a, b)

   Matrix operator A / B^T

"),

("Base","Ac_ldiv_B","Ac_ldiv_B(...)

   Matrix operator A^H B

"),

("Base","Ac_ldiv_Bc","Ac_ldiv_Bc(...)

   Matrix operator A^H B^H

"),

("Base","Ac_mul_B","Ac_mul_B(...)

   Matrix operator A^H B

"),

("Base","Ac_mul_Bc","Ac_mul_Bc(...)

   Matrix operator A^H B^H

"),

("Base","Ac_rdiv_B","Ac_rdiv_B(a, b)

   Matrix operator A^H / B

"),

("Base","Ac_rdiv_Bc","Ac_rdiv_Bc(a, b)

   Matrix operator A^H / B^H

"),

("Base","At_ldiv_B","At_ldiv_B(...)

   Matrix operator A^T B

"),

("Base","At_ldiv_Bt","At_ldiv_Bt(...)

   Matrix operator A^T B^T

"),

("Base","At_mul_B","At_mul_B(...)

   Matrix operator A^T B

"),

("Base","At_mul_Bt","At_mul_Bt(...)

   Matrix operator A^T B^T

"),

("Base","At_rdiv_B","At_rdiv_B(a, b)

   Matrix operator A^T / B

"),

("Base","At_rdiv_Bt","At_rdiv_Bt(a, b)

   Matrix operator A^T / B^T

"),

("Base","isapprox","isapprox(x::Number, y::Number; rtol::Real=cbrt(maxeps), atol::Real=sqrt(maxeps))

   Inexact equality comparison - behaves slightly different depending
   on types of input args:

      * For \"FloatingPoint\" numbers, \"isapprox\" returns \"true\"
        if   \"abs(x-y) <= atol + rtol*max(abs(x), abs(y))\".

      * For \"Integer\" and \"Rational\" numbers, \"isapprox\"
        returns \"true\" if \"abs(x-y) <= atol\". The *rtol* argument
        is ignored.   If one of \"x\" and \"y\" is \"FloatingPoint\",
        the other is   promoted, and the method above is called
        instead.

      * For \"Complex\" numbers, the distance in the complex plane
        is compared, using the same criterion as above.

   For default tolerance arguments, \"maxeps = max(eps(abs(x)),
   eps(abs(y)))\".

"),

("Base","sin","sin(x)

   Compute sine of \"x\", where \"x\" is in radians

"),

("Base","cos","cos(x)

   Compute cosine of \"x\", where \"x\" is in radians

"),

("Base","tan","tan(x)

   Compute tangent of \"x\", where \"x\" is in radians

"),

("Base","sind","sind(x)

   Compute sine of \"x\", where \"x\" is in degrees

"),

("Base","cosd","cosd(x)

   Compute cosine of \"x\", where \"x\" is in degrees

"),

("Base","tand","tand(x)

   Compute tangent of \"x\", where \"x\" is in degrees

"),

("Base","sinpi","sinpi(x)

   Compute sin(pi x) more accurately than \"sin(pi*x)\", especially
   for large \"x\".

"),

("Base","cospi","cospi(x)

   Compute cos(pi x) more accurately than \"cos(pi*x)\", especially
   for large \"x\".

"),

("Base","sinh","sinh(x)

   Compute hyperbolic sine of \"x\"

"),

("Base","cosh","cosh(x)

   Compute hyperbolic cosine of \"x\"

"),

("Base","tanh","tanh(x)

   Compute hyperbolic tangent of \"x\"

"),

("Base","asin","asin(x)

   Compute the inverse sine of \"x\", where the output is in radians

"),

("Base","acos","acos(x)

   Compute the inverse cosine of \"x\", where the output is in radians

"),

("Base","atan","atan(x)

   Compute the inverse tangent of \"x\", where the output is in
   radians

"),

("Base","atan2","atan2(y, x)

   Compute the inverse tangent of \"y/x\", using the signs of both
   \"x\" and \"y\" to determine the quadrant of the return value.

"),

("Base","asind","asind(x)

   Compute the inverse sine of \"x\", where the output is in degrees

"),

("Base","acosd","acosd(x)

   Compute the inverse cosine of \"x\", where the output is in degrees

"),

("Base","atand","atand(x)

   Compute the inverse tangent of \"x\", where the output is in
   degrees

"),

("Base","sec","sec(x)

   Compute the secant of \"x\", where \"x\" is in radians

"),

("Base","csc","csc(x)

   Compute the cosecant of \"x\", where \"x\" is in radians

"),

("Base","cot","cot(x)

   Compute the cotangent of \"x\", where \"x\" is in radians

"),

("Base","secd","secd(x)

   Compute the secant of \"x\", where \"x\" is in degrees

"),

("Base","cscd","cscd(x)

   Compute the cosecant of \"x\", where \"x\" is in degrees

"),

("Base","cotd","cotd(x)

   Compute the cotangent of \"x\", where \"x\" is in degrees

"),

("Base","asec","asec(x)

   Compute the inverse secant of \"x\", where the output is in radians

"),

("Base","acsc","acsc(x)

   Compute the inverse cosecant of \"x\", where the output is in
   radians

"),

("Base","acot","acot(x)

   Compute the inverse cotangent of \"x\", where the output is in
   radians

"),

("Base","asecd","asecd(x)

   Compute the inverse secant of \"x\", where the output is in degrees

"),

("Base","acscd","acscd(x)

   Compute the inverse cosecant of \"x\", where the output is in
   degrees

"),

("Base","acotd","acotd(x)

   Compute the inverse cotangent of \"x\", where the output is in
   degrees

"),

("Base","sech","sech(x)

   Compute the hyperbolic secant of \"x\"

"),

("Base","csch","csch(x)

   Compute the hyperbolic cosecant of \"x\"

"),

("Base","coth","coth(x)

   Compute the hyperbolic cotangent of \"x\"

"),

("Base","asinh","asinh(x)

   Compute the inverse hyperbolic sine of \"x\"

"),

("Base","acosh","acosh(x)

   Compute the inverse hyperbolic cosine of \"x\"

"),

("Base","atanh","atanh(x)

   Compute the inverse hyperbolic tangent of \"x\"

"),

("Base","asech","asech(x)

   Compute the inverse hyperbolic secant of \"x\"

"),

("Base","acsch","acsch(x)

   Compute the inverse hyperbolic cosecant of \"x\"

"),

("Base","acoth","acoth(x)

   Compute the inverse hyperbolic cotangent of \"x\"

"),

("Base","sinc","sinc(x)

   Compute sin(pi x) / (pi x) if x neq 0, and 1 if x = 0.

"),

("Base","cosc","cosc(x)

   Compute cos(pi x) / x - sin(pi x) / (pi x^2) if x neq 0, and 0 if x
   = 0. This is the derivative of \"sinc(x)\".

"),

("Base","deg2rad","deg2rad(x)

   Convert \"x\" from degrees to radians

"),

("Base","rad2deg","rad2deg(x)

   Convert \"x\" from radians to degrees

"),

("Base","hypot","hypot(x, y)

   Compute the sqrt{x^2+y^2} avoiding overflow and underflow

"),

("Base","log","log(b, x)

   Compute the base \"b\" logarithm of \"x\". Throws \"DomainError\"
   for negative \"Real\" arguments.

"),

("Base","log","log(b, x)

   Compute the base \"b\" logarithm of \"x\". Throws \"DomainError\"
   for negative \"Real\" arguments.

"),

("Base","log2","log2(x)

   Compute the logarithm of \"x\" to base 2. Throws \"DomainError\"
   for negative \"Real\" arguments.

"),

("Base","log10","log10(x)

   Compute the logarithm of \"x\" to base 10. Throws \"DomainError\"
   for negative \"Real\" arguments.

"),

("Base","log1p","log1p(x)

   Accurate natural logarithm of \"1+x\".  Throws \"DomainError\" for
   \"Real\" arguments less than -1.

   There is an experimental variant in the \"Base.Math.JuliaLibm\"
   module, which is typically faster and more accurate.

"),

("Base","frexp","frexp(val)

   Return \"(x,exp)\" such that \"x\" has a magnitude in the interval
   \"[1/2, 1)\" or 0, and val = x times 2^{exp}.

"),

("Base","exp","exp(x)

   Compute e^x

"),

("Base","exp2","exp2(x)

   Compute 2^x

"),

("Base","exp10","exp10(x)

   Compute 10^x

"),

("Base","ldexp","ldexp(x, n)

   Compute x times 2^n

"),

("Base","modf","modf(x)

   Return a tuple (fpart,ipart) of the fractional and integral parts
   of a number. Both parts have the same sign as the argument.

"),

("Base","expm1","expm1(x)

   Accurately compute e^x-1

"),

("Base","round","round(z, RoundingModeReal, RoundingModeImaginary)

   Returns the nearest integral value of the same type as the complex-
   valued \"z\" to \"z\", breaking ties using the specified
   \"RoundingMode\"s. The first \"RoundingMode\" is used for rounding
   the real components while the second is used for rounding the
   imaginary components.

      julia> round(pi, 2) 3.14

      julia> round(pi, 3, 2) 3.125

   Note: Rounding to specified digits in bases other than 2 can be
     inexact when operating on binary floating point numbers. For
     example, the \"Float64\" value represented by \"1.15\" is
     actually *less* than 1.15, yet will be rounded to 1.2.

        julia> x = 1.15
        1.15

        julia> @sprintf \"%.20f\" x
        \"1.14999999999999991118\"

        julia> x < 115//100
        true

        julia> round(x, 1)
        1.2

"),

("Base","RoundingMode","RoundingMode

   A type which controls rounding behavior. Currently supported
   rounding modes are:

   * \"RoundNearest\" (default)

   * \"RoundNearestTiesAway\"

   * \"RoundNearestTiesUp\"

   * \"RoundToZero\"

   * \"RoundUp\"

   * \"RoundDown\"

"),

("Base","RoundNearest","RoundNearest

   The default rounding mode. Rounds to the nearest integer, with ties
   (fractional values of 0.5) being rounded to the nearest even
   integer.

"),

("Base","RoundNearestTiesAway","RoundNearestTiesAway

   Rounds to nearest integer, with ties rounded away from zero (C/C++
   \"round()\" behaviour).

"),

("Base","RoundNearestTiesUp","RoundNearestTiesUp

   Rounds to nearest integer, with ties rounded toward positive
   infinity (Java/JavaScript \"round()\" behaviour).

"),

("Base","RoundToZero","RoundToZero

   \"round()\" using this rounding mode is an alias for \"trunc()\".

"),

("Base","RoundUp","RoundUp

   \"round()\" using this rounding mode is an alias for \"ceil()\".

"),

("Base","RoundDown","RoundDown

   \"round()\" using this rounding mode is an alias for \"floor()\".

"),

("Base","round","round(z, RoundingModeReal, RoundingModeImaginary)

   Returns the nearest integral value of the same type as the complex-
   valued \"z\" to \"z\", breaking ties using the specified
   \"RoundingMode\"s. The first \"RoundingMode\" is used for rounding
   the real components while the second is used for rounding the
   imaginary components.

"),

("Base","ceil","ceil([T], x[, digits[, base]])

   \"ceil(x)\" returns the nearest integral value of the same type as
   \"x\" that is greater than or equal to \"x\".

   \"ceil(T, x)\" converts the result to type \"T\", throwing an
   \"InexactError\" if the value is not representable.

   \"digits\" and \"base\" work as for \"round()\".

"),

("Base","floor","floor([T], x[, digits[, base]])

   \"floor(x)\" returns the nearest integral value of the same type as
   \"x\" that is less than or equal to \"x\".

   \"floor(T, x)\" converts the result to type \"T\", throwing an
   \"InexactError\" if the value is not representable.

   \"digits\" and \"base\" work as for \"round()\".

"),

("Base","trunc","trunc([T], x[, digits[, base]])

   \"trunc(x)\" returns the nearest integral value of the same type as
   \"x\" whose absolute value is less than or equal to \"x\".

   \"trunc(T, x)\" converts the result to type \"T\", throwing an
   \"InexactError\" if the value is not representable.

   \"digits\" and \"base\" work as for \"round()\".

"),

("Base","unsafe_trunc","unsafe_trunc(T, x)

   \"unsafe_trunc(T, x)\" returns the nearest integral value of type
   \"T\" whose absolute value is less than or equal to \"x\". If the
   value is not representable by \"T\", an arbitrary value will be
   returned.

"),

("Base","signif","signif(x, digits[, base])

   Rounds (in the sense of \"round\") \"x\" so that there are
   \"digits\" significant digits, under a base \"base\"
   representation, default 10. E.g., \"signif(123.456, 2)\" is
   \"120.0\", and \"signif(357.913, 4, 2)\" is \"352.0\".

"),

("Base","min","min(x, y, ...)

   Return the minimum of the arguments. Operates elementwise over
   arrays.

"),

("Base","max","max(x, y, ...)

   Return the maximum of the arguments. Operates elementwise over
   arrays.

"),

("Base","minmax","minmax(x, y)

   Return \"(min(x,y), max(x,y))\". See also: \"extrema()\" that
   returns \"(minimum(x), maximum(x))\"

"),

("Base","clamp","clamp(x, lo, hi)

   Return x if \"lo <= x <= hi\". If \"x < lo\", return \"lo\". If \"x
   > hi\", return \"hi\". Arguments are promoted to a common type.
   Operates elementwise over \"x\" if it is an array.

"),

("Base","bin","bin(n[, pad])

   Convert an integer to a binary string, optionally specifying a
   number of digits to pad to.

"),

("Base","hex","hex(n[, pad])

   Convert an integer to a hexadecimal string, optionally specifying a
   number of digits to pad to.

"),

("Base","dec","dec(n[, pad])

   Convert an integer to a decimal string, optionally specifying a
   number of digits to pad to.

"),

("Base","oct","oct(n[, pad])

   Convert an integer to an octal string, optionally specifying a
   number of digits to pad to.

"),

("Base","base","base(base, n[, pad])

   Convert an integer to a string in the given base, optionally
   specifying a number of digits to pad to. The base can be specified
   as either an integer, or as a \"UInt8\" array of character values
   to use as digit symbols.

"),

("Base","digits","digits(n[, base][, pad])

   Returns an array of the digits of \"n\" in the given base,
   optionally padded with zeros to a specified size. More significant
   digits are at higher indexes, such that \"n ==
   sum([digits[k]*base^(k-1) for k=1:length(digits)])\".

"),

("Base","digits!","digits!(array, n[, base])

   Fills an array of the digits of \"n\" in the given base. More
   significant digits are at higher indexes. If the array length is
   insufficient, the least significant digits are filled up to the
   array length. If the array length is excessive, the excess portion
   is filled with zeros.

"),

("Base","bits","bits(n)

   A string giving the literal bit representation of a number.

"),

("Base","parse","parse(type, str[, base])

   Parse a string as a number. If the type is an integer type, then a
   base can be specified (the default is 10). If the type is a
   floating point type, the string is parsed as a decimal floating
   point number. If the string does not contain a valid number, an
   error is raised.

"),

("Base","tryparse","tryparse(type, str[, base])

   Like \"parse\", but returns a \"Nullable\" of the requested type.
   The result will be null if the string does not contain a valid
   number.

"),

("Base","big","big(x)

   Convert a number to a maximum precision representation (typically
   \"BigInt\" or \"BigFloat\"). See \"BigFloat\" for information about
   some pitfalls with floating-point numbers.

"),

("Base","signed","signed(x)

   Convert a number to a signed integer. If the argument is unsigned,
   it is reinterpreted as signed without checking for overflow.

"),

("Base","unsigned","unsigned(x) -> Unsigned

   Convert a number to an unsigned integer. If the argument is signed,
   it is reinterpreted as unsigned without checking for negative
   values.

"),

("Base","float","float(x)

   Convert a number, array, or string to a \"FloatingPoint\" data
   type. For numeric data, the smallest suitable \"FloatingPoint\"
   type is used. Converts strings to \"Float64\".

"),

("Base","significand","significand(x)

   Extract the significand(s) (a.k.a. mantissa), in binary
   representation, of a floating-point number or array. If \"x\" is a
   non-zero finite number, than the result will be a number of the
   same type on the interval [1,2). Otherwise \"x\" is returned.

      julia> significand(15.2)/15.2
      0.125

      julia> significand(15.2)*8
      15.2

"),

("Base","exponent","exponent(x) -> Int

   Get the exponent of a normalized floating-point number.

"),

("Base","complex","complex(r[, i])

   Convert real numbers or arrays to complex. \"i\" defaults to zero.

"),

("Base","bswap","bswap(n)

   Byte-swap an integer

"),

("Base","num2hex","num2hex(f)

   Get a hexadecimal string of the binary representation of a floating
   point number

"),

("Base","hex2num","hex2num(str)

   Convert a hexadecimal string to the floating point number it
   represents

"),

("Base","hex2bytes","hex2bytes(s::ASCIIString)

   Convert an arbitrarily long hexadecimal string to its binary
   representation. Returns an Array{UInt8, 1}, i.e. an array of bytes.

"),

("Base","bytes2hex","bytes2hex(bin_arr::Array{UInt8, 1})

   Convert an array of bytes to its hexadecimal representation. All
   characters are in lower-case. Returns an ASCIIString.

"),

("Base","one","one(x)

   Get the multiplicative identity element for the type of x (x can
   also specify the type itself). For matrices, returns an identity
   matrix of the appropriate size and type.

"),

("Base","zero","zero(x)

   Get the additive identity element for the type of x (x can also
   specify the type itself).

"),

("Base","pi","pi
π

   The constant pi

"),

("Base","im","im

   The imaginary unit

"),

("Base","e","e
eu

   The constant e

"),

("Base","catalan","catalan

   Catalan's constant

"),

("Base","γ","γ
eulergamma

   Euler's constant

"),

("Base","φ","φ
golden

   The golden ratio

"),

("Base","Inf","Inf

   Positive infinity of type Float64

"),

("Base","Inf32","Inf32

   Positive infinity of type Float32

"),

("Base","Inf16","Inf16

   Positive infinity of type Float16

"),

("Base","NaN","NaN

   A not-a-number value of type Float64

"),

("Base","NaN32","NaN32

   A not-a-number value of type Float32

"),

("Base","NaN16","NaN16

   A not-a-number value of type Float16

"),

("Base","issubnormal","issubnormal(f) -> Bool

   Test whether a floating point number is subnormal

"),

("Base","isfinite","isfinite(f) -> Bool

   Test whether a number is finite

"),

("Base","isinf","isinf(f) -> Bool

   Test whether a number is infinite

"),

("Base","isnan","isnan(f) -> Bool

   Test whether a floating point number is not a number (NaN)

"),

("Base","inf","inf(f)

   Returns positive infinity of the floating point type \"f\" or of
   the same floating point type as \"f\"

"),

("Base","nan","nan(f)

   Returns NaN (not-a-number) of the floating point type \"f\" or of
   the same floating point type as \"f\"

"),

("Base","nextfloat","nextfloat(f)

   Get the next floating point number in lexicographic order

"),

("Base","prevfloat","prevfloat(f) -> FloatingPoint

   Get the previous floating point number in lexicographic order

"),

("Base","isinteger","isinteger(x) -> Bool

   Test whether \"x\" or all its elements are numerically equal to
   some integer

"),

("Base","isreal","isreal(x) -> Bool

   Test whether \"x\" or all its elements are numerically equal to
   some real number

"),

("Base","Float32","Float32(x[, mode::RoundingMode])

   Create a Float32 from \"x\". If \"x\" is not exactly representable
   then \"mode\" determines how \"x\" is rounded.

      julia> Float32(1/3, RoundDown)
      0.3333333f0

      julia> Float32(1/3, RoundUp)
      0.33333334f0

   See \"get_rounding\" for available rounding modes.

"),

("Base","Float64","Float64(x[, mode::RoundingMode])

   Create a Float64 from \"x\". If \"x\" is not exactly representable
   then \"mode\" determines how \"x\" is rounded.

      julia> Float64(pi, RoundDown)
      3.141592653589793

      julia> Float64(pi, RoundUp)
      3.1415926535897936

   See \"get_rounding\" for available rounding modes.

"),

("Base","BigInt","BigInt(x)

   Create an arbitrary precision integer. \"x\" may be an \"Int\" (or
   anything that can be converted to an \"Int\").  The usual
   mathematical operators are defined for this type, and results are
   promoted to a \"BigInt\".

   Instances can be constructed from strings via \"parse()\", or using
   the \"big\" string literal.

"),

("Base","BigFloat","BigFloat(x)

   Create an arbitrary precision floating point number. \"x\" may be
   an \"Integer\", a \"Float64\" or a \"BigInt\". The usual
   mathematical operators are defined for this type, and results are
   promoted to a \"BigFloat\".

   Note that because decimal literals are converted to floating point
   numbers when parsed, \"BigFloat(2.1)\" may not yield what you
   expect. You may instead prefer to initialize constants from strings
   via \"parse()\", or using the \"big\" string literal.

      julia> big\"2.1\"
      2.099999999999999999999999999999999999999999999999999999999999999999999999999986e+00 with 256 bits of precision

"),

("Base","get_rounding","get_rounding(T)

   Get the current floating point rounding mode for type \"T\",
   controlling the rounding of basic arithmetic functions (\"+()\",
   \"-()\", \">>*<<()\", \"/()\" and \"sqrt()\") and type conversion.

   Valid modes are \"RoundNearest\", \"RoundToZero\", \"RoundUp\",
   \"RoundDown\", and \"RoundFromZero\" (\"BigFloat\" only).

"),

("Base","set_rounding","set_rounding(T, mode)

   Set the rounding mode of floating point type \"T\", controlling the
   rounding of basic arithmetic functions (\"+()\", \"-()\",
   \">>*<<()\", \"/()\" and \"sqrt()\") and type conversion.

   Note that this may affect other types, for instance changing the
   rounding mode of \"Float64\" will change the rounding mode of
   \"Float32\". See \"get_rounding\" for available modes

"),

("Base","with_rounding","with_rounding(f::Function, T, mode)

   Change the rounding mode of floating point type \"T\" for the
   duration of \"f\". It is logically equivalent to:

      old = get_rounding(T)
      set_rounding(T, mode)
      f()
      set_rounding(T, old)

   See \"get_rounding\" for available rounding modes.

"),

("Base","count_ones","count_ones(x::Integer) -> Integer

   Number of ones in the binary representation of \"x\".

      julia> count_ones(7)
      3

"),

("Base","count_zeros","count_zeros(x::Integer) -> Integer

   Number of zeros in the binary representation of \"x\".

      julia> count_zeros(Int32(2 ^ 16 - 1))
      16

"),

("Base","leading_zeros","leading_zeros(x::Integer) -> Integer

   Number of zeros leading the binary representation of \"x\".

      julia> leading_zeros(Int32(1))
      31

"),

("Base","leading_ones","leading_ones(x::Integer) -> Integer

   Number of ones leading the binary representation of \"x\".

      julia> leading_ones(UInt32(2 ^ 32 - 2))
      31

"),

("Base","trailing_zeros","trailing_zeros(x::Integer) -> Integer

   Number of zeros trailing the binary representation of \"x\".

      julia> trailing_zeros(2)
      1

"),

("Base","trailing_ones","trailing_ones(x::Integer) -> Integer

   Number of ones trailing the binary representation of \"x\".

      julia> trailing_ones(3)
      2

"),

("Base","isprime","isprime(x::BigInt[, reps = 25]) -> Bool

   Probabilistic primality test. Returns \"true\" if \"x\" is prime;
   and \"false\" if \"x\" is not prime with high probability. The
   false positive rate is about \"0.25^reps\". \"reps = 25\" is
   considered safe for cryptographic applications (Knuth,
   Seminumerical Algorithms).

      julia> isprime(big(3))
      true

"),

("Base","isprime","isprime(x::BigInt[, reps = 25]) -> Bool

   Probabilistic primality test. Returns \"true\" if \"x\" is prime;
   and \"false\" if \"x\" is not prime with high probability. The
   false positive rate is about \"0.25^reps\". \"reps = 25\" is
   considered safe for cryptographic applications (Knuth,
   Seminumerical Algorithms).

      julia> isprime(big(3))
      true

"),

("Base","primes","primes(n)

   Returns a collection of the prime numbers <= \"n\".

"),

("Base","isodd","isodd(x::Integer) -> Bool

   Returns \"true\" if \"x\" is odd (that is, not divisible by 2), and
   \"false\" otherwise.

      julia> isodd(9)
      true

      julia> isodd(10)
      false

"),

("Base","iseven","iseven(x::Integer) -> Bool

   Returns \"true\" is \"x\" is even (that is, divisible by 2), and
   \"false\" otherwise.

      julia> iseven(9)
      false

      julia> iseven(10)
      true

"),

("Base","precision","precision(num::FloatingPoint)

   Get the precision of a floating point number, as defined by the
   effective number of bits in the mantissa.

"),

("Base","get_bigfloat_precision","get_bigfloat_precision()

   Get the precision (in bits) currently used for BigFloat arithmetic.

"),

("Base","set_bigfloat_precision","set_bigfloat_precision(x::Int64)

   Set the precision (in bits) to be used to BigFloat arithmetic.

"),

("Base","with_bigfloat_precision","with_bigfloat_precision(f::Function, precision::Integer)

   Change the BigFloat arithmetic precision (in bits) for the duration
   of \"f\". It is logically equivalent to:

      old = get_bigfloat_precision()
      set_bigfloat_precision(precision)
      f()
      set_bigfloat_precision(old)

"),

("Base","srand","srand([rng][, seed])

   Reseed the random number generator. If a \"seed\" is provided, the
   RNG will give a reproducible sequence of numbers, otherwise Julia
   will get entropy from the system. For \"MersenneTwister\", the
   \"seed\" may be a non-negative integer, a vector of \"UInt32\"
   integers or a filename, in which case the seed is read from a file.
   \"RandomDevice\" does not support seeding.

"),

("Base","MersenneTwister","MersenneTwister([seed])

   Create a \"MersenneTwister\" RNG object. Different RNG objects can
   have their own seeds, which may be useful for generating different
   streams of random numbers.

"),

("Base","RandomDevice","RandomDevice()

   Create a \"RandomDevice\" RNG object. Two such objects will always
   generate different streams of random numbers.

"),

("Base","rand","rand([rng][, S][, dims...])

   Pick a random element or array of random elements from the set of
   values specified by \"S\"; \"S\" can be

      * an indexable collection (for example \"1:n\" or
        \"['x','y','z']\"), or

      * a type: the set of values to pick from is then equivalent to
        \"typemin(S):typemax(S)\" for integers (this is not applicable
        to   \"BigInt\"), and to [0,1) for floating point numbers;

   \"S\" defaults to \"Float64\".

"),

("Base","rand!","rand!([rng], A[, coll])

   Populate the array A with random values. If the indexable
   collection \"coll\" is specified, the values are picked randomly
   from \"coll\". This is equivalent to \"copy!(A, rand(rng, coll,
   size(A)))\" or \"copy!(A, rand(rng, eltype(A), size(A)))\" but
   without allocating a new array.

"),

("Base","bitrand","bitrand([rng][, dims...])

   Generate a \"BitArray\" of random boolean values.

"),

("Base","randn","randn([rng][, dims...])

   Generate a normally-distributed random number with mean 0 and
   standard deviation 1. Optionally generate an array of normally-
   distributed random numbers.

"),

("Base","randn!","randn!([rng], A::Array{Float64, N})

   Fill the array A with normally-distributed (mean 0, standard
   deviation 1) random numbers. Also see the rand function.

"),

("Base","randexp","randexp([rng][, dims...])

   Generate a random number according to the exponential distribution
   with scale 1. Optionally generate an array of such random numbers.

"),

("Base","randexp!","randexp!([rng], A::Array{Float64, N})

   Fill the array A with random numbers following the exponential
   distribution (with scale 1).

"),

("Base","Task","Task(func)

   Create a \"Task\" (i.e. thread, or coroutine) to execute the given
   function (which must be callable with no arguments). The task exits
   when this function returns.

"),

("Base","yieldto","yieldto(task, arg = nothing)

   Switch to the given task. The first time a task is switched to, the
   task's function is called with no arguments. On subsequent
   switches, \"arg\" is returned from the task's last call to
   \"yieldto\". This is a low-level call that only switches tasks, not
   considering states or scheduling in any way. Its use is
   discouraged.

"),

("Base","current_task","current_task()

   Get the currently running Task.

"),

("Base","istaskdone","istaskdone(task) -> Bool

   Tell whether a task has exited.

"),

("Base","istaskstarted","istaskstarted(task) -> Bool

   Tell whether a task has started executing.

"),

("Base","consume","consume(task, values...)

   Receive the next value passed to \"produce\" by the specified task.
   Additional arguments may be passed, to be returned from the last
   \"produce\" call in the producer.

"),

("Base","produce","produce(value)

   Send the given value to the last \"consume\" call, switching to the
   consumer task. If the next \"consume\" call passes any values, they
   are returned by \"produce\".

"),

("Base","yield","yield()

   Switch to the scheduler to allow another scheduled task to run. A
   task that calls this function is still runnable, and will be
   restarted immediately if there are no other runnable tasks.

"),

("Base","task_local_storage","task_local_storage(body, symbol, value)

   Call the function \"body\" with a modified task-local storage, in
   which \"value\" is assigned to \"symbol\"; the previous value of
   \"symbol\", or lack thereof, is restored afterwards. Useful for
   emulating dynamic scoping.

"),

("Base","task_local_storage","task_local_storage(body, symbol, value)

   Call the function \"body\" with a modified task-local storage, in
   which \"value\" is assigned to \"symbol\"; the previous value of
   \"symbol\", or lack thereof, is restored afterwards. Useful for
   emulating dynamic scoping.

"),

("Base","task_local_storage","task_local_storage(body, symbol, value)

   Call the function \"body\" with a modified task-local storage, in
   which \"value\" is assigned to \"symbol\"; the previous value of
   \"symbol\", or lack thereof, is restored afterwards. Useful for
   emulating dynamic scoping.

"),

("Base","Condition","Condition()

   Create an edge-triggered event source that tasks can wait for.
   Tasks that call \"wait\" on a \"Condition\" are suspended and
   queued. Tasks are woken up when \"notify\" is later called on the
   \"Condition\". Edge triggering means that only tasks waiting at the
   time \"notify\" is called can be woken up. For level-triggered
   notifications, you must keep extra state to keep track of whether a
   notification has happened. The \"RemoteRef\" type does this, and so
   can be used for level-triggered events.

"),

("Base","notify","notify(condition, val=nothing; all=true, error=false)

   Wake up tasks waiting for a condition, passing them \"val\". If
   \"all\" is true (the default), all waiting tasks are woken,
   otherwise only one is. If \"error\" is true, the passed value is
   raised as an exception in the woken tasks.

"),

("Base","schedule","schedule(t::Task, [val]; error=false)

   Add a task to the scheduler's queue. This causes the task to run
   constantly when the system is otherwise idle, unless the task
   performs a blocking operation such as \"wait\".

   If a second argument is provided, it will be passed to the task
   (via the return value of \"yieldto\") when it runs again. If
   \"error\" is true, the value is raised as an exception in the woken
   task.

"),

("Base","@schedule","@schedule()

   Wrap an expression in a Task and add it to the scheduler's queue.

"),

("Base","@task","@task()

   Wrap an expression in a Task without executing it, and return the
   Task. This only creates a task, and does not run it.

"),

("Base","sleep","sleep(seconds)

   Block the current task for a specified number of seconds. The
   minimum sleep time is 1 millisecond or input of \"0.001\".

"),

("Base","ReentrantLock","ReentrantLock()

   Creates a reentrant lock. The same task can acquire the lock as
   many times as required. Each lock must be matched with an unlock.

"),

("Base","lock","lock(l::ReentrantLock)

   Associates \"l\" with the current task. If \"l\" is already locked
   by a different task, waits for it to become available. The same
   task can acquire the lock multiple times. Each \"lock\" must be
   matched by an \"unlock\"

"),

("Base","unlock","unlock(l::ReentrantLock)

   Releases ownership of the lock by the current task. If the lock had
   been acquired before, it just decrements an internal counter and
   returns immediately.

"),

("Base","addprocs","addprocs(manager::ClusterManager; kwargs...) -> List of process identifiers

   Launches worker processes via the specified cluster manager.

   For example Beowulf clusters are  supported via a custom cluster
   manager implemented in  package \"ClusterManagers\".

   The number of seconds a newly launched worker waits for connection
   establishment from the master can be specified via variable
   \"JULIA_WORKER_TIMEOUT\" in the worker process's environment.
   Relevant only when using TCP/IP as transport.

"),

("Base","addprocs","addprocs(manager::ClusterManager; kwargs...) -> List of process identifiers

   Launches worker processes via the specified cluster manager.

   For example Beowulf clusters are  supported via a custom cluster
   manager implemented in  package \"ClusterManagers\".

   The number of seconds a newly launched worker waits for connection
   establishment from the master can be specified via variable
   \"JULIA_WORKER_TIMEOUT\" in the worker process's environment.
   Relevant only when using TCP/IP as transport.

"),

("Base","addprocs","addprocs(manager::ClusterManager; kwargs...) -> List of process identifiers

   Launches worker processes via the specified cluster manager.

   For example Beowulf clusters are  supported via a custom cluster
   manager implemented in  package \"ClusterManagers\".

   The number of seconds a newly launched worker waits for connection
   establishment from the master can be specified via variable
   \"JULIA_WORKER_TIMEOUT\" in the worker process's environment.
   Relevant only when using TCP/IP as transport.

"),

("Base","addprocs","addprocs(manager::ClusterManager; kwargs...) -> List of process identifiers

   Launches worker processes via the specified cluster manager.

   For example Beowulf clusters are  supported via a custom cluster
   manager implemented in  package \"ClusterManagers\".

   The number of seconds a newly launched worker waits for connection
   establishment from the master can be specified via variable
   \"JULIA_WORKER_TIMEOUT\" in the worker process's environment.
   Relevant only when using TCP/IP as transport.

"),

("Base","nprocs","nprocs()

   Get the number of available processes.

"),

("Base","nworkers","nworkers()

   Get the number of available worker processes. This is one less than
   nprocs(). Equal to nprocs() if nprocs() == 1.

"),

("Base","procs","procs(S::SharedArray)

   Get the vector of processes that have mapped the shared array

"),

("Base","workers","workers()

   Returns a list of all worker process identifiers.

"),

("Base","rmprocs","rmprocs(pids...)

   Removes the specified workers.

"),

("Base","interrupt","interrupt([pids...])

   Interrupt the current executing task on the specified workers. This
   is equivalent to pressing Ctrl-C on the local machine. If no
   arguments are given, all workers are interrupted.

"),

("Base","myid","myid()

   Get the id of the current process.

"),

("Base","pmap","pmap(f, lsts...; err_retry=true, err_stop=false, pids=workers())

   Transform collections \"lsts\" by applying \"f\" to each element in
   parallel. If \"nprocs() > 1\", the calling process will be
   dedicated to assigning tasks. All other available processes will be
   used as parallel workers, or on the processes specified by
   \"pids\".

   If \"err_retry\" is true, it retries a failed application of \"f\"
   on a different worker. If \"err_stop\" is true, it takes precedence
   over the value of \"err_retry\" and \"pmap\" stops execution on the
   first error.

"),

("Base","remotecall","remotecall(id, func, args...)

   Call a function asynchronously on the given arguments on the
   specified process. Returns a \"RemoteRef\".

"),

("Base","wait","wait([x])

   Block the current task until some event occurs, depending on the
   type of the argument:

      * \"RemoteRef\": Wait for a value to become available for the
        specified remote reference.

      * \"Condition\": Wait for \"notify\" on a condition.

      * \"Process\": Wait for a process or process chain to exit.
        The \"exitcode\" field of a process can be used to determine
        success   or failure.

      * \"Task\": Wait for a \"Task\" to finish, returning its
        result value. If the task fails with an exception, the
        exception is propagated (re-thrown in the task that called
        \"wait\").

      * \"RawFD\": Wait for changes on a file descriptor (see
        *poll_fd*   for keyword arguments and return code)

   If no argument is passed, the task blocks for an undefined period.
   If the task's state is set to \":waiting\", it can only be
   restarted by an explicit call to \"schedule\" or \"yieldto\". If
   the task's state is \":runnable\", it might be restarted
   unpredictably.

   Often \"wait\" is called within a \"while\" loop to ensure a
   waited-for condition is met before proceeding.

"),

("Base","fetch","fetch(RemoteRef)

   Wait for and get the value of a remote reference.

"),

("Base","remotecall_wait","remotecall_wait(id, func, args...)

   Perform \"wait(remotecall(...))\" in one message.

"),

("Base","remotecall_fetch","remotecall_fetch(id, func, args...)

   Perform \"fetch(remotecall(...))\" in one message.

"),

("Base","put!","put!(RemoteRef, value)

   Store a value to a remote reference. Implements \"shared queue of
   length 1\" semantics: if a value is already present, blocks until
   the value is removed with \"take!\". Returns its first argument.

"),

("Base","take!","take!(RemoteRef)

   Fetch the value of a remote reference, removing it so that the
   reference is empty again.

"),

("Base","isready","isready(r::RemoteRef)

   Determine whether a \"RemoteRef\" has a value stored to it. Note
   that this function can cause race conditions, since by the time you
   receive its result it may no longer be true. It is recommended that
   this function only be used on a \"RemoteRef\" that is assigned
   once.

   If the argument \"RemoteRef\" is owned by a different node, this
   call will block to wait for the answer. It is recommended to wait
   for \"r\" in a separate task instead, or to use a local
   \"RemoteRef\" as a proxy:

      rr = RemoteRef()
      @async put!(rr, remotecall_fetch(p, long_computation))
      isready(rr)  # will not block

"),

("Base","RemoteRef","RemoteRef(n)

   Make an uninitialized remote reference on process \"n\".

"),

("Base","RemoteRef","RemoteRef(n)

   Make an uninitialized remote reference on process \"n\".

"),

("Base","timedwait","timedwait(testcb::Function, secs::Float64; pollint::Float64=0.1)

   Waits till \"testcb\" returns \"true\" or for \"secs`\" seconds,
   whichever is earlier. \"testcb\" is polled every \"pollint\"
   seconds.

"),

("Base","@spawn","@spawn()

   Execute an expression on an automatically-chosen process, returning
   a \"RemoteRef\" to the result.

"),

("Base","@spawnat","@spawnat()

   Accepts two arguments, \"p\" and an expression, and runs the
   expression asynchronously on process \"p\", returning a
   \"RemoteRef\" to the result.

"),

("Base","@fetch","@fetch()

   Equivalent to \"fetch(@spawn expr)\".

"),

("Base","@fetchfrom","@fetchfrom()

   Equivalent to \"fetch(@spawnat p expr)\".

"),

("Base","@async","@async()

   Schedule an expression to run on the local machine, also adding it
   to the set of items that the nearest enclosing \"@sync\" waits for.

"),

("Base","@sync","@sync()

   Wait until all dynamically-enclosed uses of \"@async\", \"@spawn\",
   \"@spawnat\" and \"@parallel\" are complete.

"),

("Base","@parallel","@parallel()

   A parallel for loop of the form

      @parallel [reducer] for var = range
          body
      end

   The specified range is partitioned and locally executed across all
   workers. In case an optional reducer function is specified,
   @parallel performs local reductions on each worker with a final
   reduction on the calling process.

   Note that without a reducer function, @parallel executes
   asynchronously, i.e. it spawns independent tasks on all available
   workers and returns immediately without waiting for completion. To
   wait for completion, prefix the call with \"@sync\", like

      @sync @parallel for var = range
          body
      end

"),

("Base","SharedArray","SharedArray(T::Type, dims::NTuple; init=false, pids=Int[])

   Construct a SharedArray of a bitstype \"T\"  and size \"dims\"
   across the processes specified by \"pids\" - all of which have to
   be on the same host.

   If \"pids\" is left unspecified, the shared array will be mapped
   across all processes on the current host, including the master.
   But, \"localindexes\" and \"indexpids\" will only refer to worker
   processes. This facilitates work distribution code to use workers
   for actual computation with the master process acting as a driver.

   If an \"init\" function of the type \"initfn(S::SharedArray)\" is
   specified, it is called on all the participating workers.

"),

("Base","procs","procs(S::SharedArray)

   Get the vector of processes that have mapped the shared array

"),

("Base","sdata","sdata(S::SharedArray)

   Returns the actual \"Array\" object backing \"S\"

"),

("Base","indexpids","indexpids(S::SharedArray)

   Returns the index of the current worker into the \"pids\" vector,
   i.e., the list of workers mapping the SharedArray

"),

("Base","launch","launch(manager::FooManager, params::Dict, launched::Vector{WorkerConfig}, launch_ntfy::Condition)

   Implemented by cluster managers. For every Julia worker launched by
   this function, it should append a \"WorkerConfig\" entry to
   \"launched\" and notify \"launch_ntfy\". The function MUST exit
   once all workers, requested by \"manager\" have been launched.
   \"params\" is a dictionary of all keyword arguments \"addprocs\"
   was called with.

"),

("Base","manage","manage(manager::FooManager, pid::Int, config::WorkerConfig. op::Symbol)

   Implemented by cluster managers. It is called on the master
   process, during a worker's lifetime, with appropriate \"op\"
   values:

      * with \":register\"/\":deregister\" when a worker is added /
        removed from the Julia worker pool.

      * with \":interrupt\" when \"interrupt(workers)\" is called.
        The \"ClusterManager\" should signal the appropriate worker
        with an interrupt signal.

      * with \":finalize\" for cleanup purposes.

"),

("Base","kill","kill(manager::FooManager, pid::Int, config::WorkerConfig)

   Implemented by cluster managers. It is called on the master
   process, by \"rmprocs\". It should cause the remote worker
   specified by \"pid\" to exit.
   \"Base.kill(manager::ClusterManager.....)\" executes a remote
   \"exit()\" on \"pid\"

"),

("Base","init_worker","init_worker(manager::FooManager)

   Called by cluster managers implementing custom transports. It
   initializes a newly launched process as a worker. Command line
   argument \"–worker\" has the effect of initializing a process as a
   worker using TCP/IP sockets for transport.

"),

("Base","connect","connect(manager::FooManager, pid::Int, config::WorkerConfig) -> (instrm::AsyncStream, outstrm::AsyncStream)

   Implemented by cluster managers using custom transports. It should
   establish a logical connection to worker with id \"pid\", specified
   by \"config\" and return a pair of \"AsyncStream\" objects.
   Messages from \"pid\" to current process will be read off
   \"instrm\", while messages to be sent to \"pid\" will be written to
   \"outstrm\". The custom transport implementation must ensure that
   messages are delivered and received completely and in order.
   \"Base.connect(manager::ClusterManager.....)\" sets up TCP/IP
   socket connections in-between workers.

"),

("Base","Base","Base.process_messages(instrm::AsyncStream, outstrm::AsyncStream)

   Called by cluster managers using custom transports. It should be
   called when the custom transport implementation receives the first
   message from a remote worker. The custom transport must manage a
   logical connection to the remote worker and provide two AsyncStream
   objects, one for incoming messages and the other for messages
   addressed to the remote worker.

"),

("Base.Pkg","dir","dir(names...) -> AbstractString

   Equivalent to \"normpath(Pkg.dir(),names...)\" – i.e. it appends
   path components to the package directory and normalizes the
   resulting path. In particular, \"Pkg.dir(pkg)\" returns the path to
   the package \"pkg\".

"),

("Base.Pkg","dir","dir(names...) -> AbstractString

   Equivalent to \"normpath(Pkg.dir(),names...)\" – i.e. it appends
   path components to the package directory and normalizes the
   resulting path. In particular, \"Pkg.dir(pkg)\" returns the path to
   the package \"pkg\".

"),

("Base.Pkg","init","init(meta::AbstractString=DEFAULT_META, branch::AbstractString=META_BRANCH)

   Initialize \"Pkg.dir()\" as a package directory. This will be done
   automatically when the \"JULIA_PKGDIR\" is not set and
   \"Pkg.dir()\" uses its default value. As part of this process,
   clones a local METADATA git repository from the site and branch
   specified by its arguments, which are typically not provided.
   Explicit (non-default) arguments can be used to support a custom
   METADATA setup.

"),

("Base.Pkg","resolve","resolve()

   Determines an optimal, consistent set of package versions to
   install or upgrade to. The optimal set of package versions is based
   on the contents of \"Pkg.dir(\"REQUIRE\")\" and the state of
   installed packages in \"Pkg.dir()\", Packages that are no longer
   required are moved into \"Pkg.dir(\".trash\")\".

"),

("Base.Pkg","edit","edit()

   Opens \"Pkg.dir(\"REQUIRE\")\" in the editor specified by the
   \"VISUAL\" or \"EDITOR\" environment variables; when the editor
   command returns, it runs \"Pkg.resolve()\" to determine and install
   a new optimal set of installed package versions.

"),

("Base.Pkg","add","add(pkg, vers...)

   Add a requirement entry for \"pkg\" to \"Pkg.dir(\"REQUIRE\")\" and
   call \"Pkg.resolve()\". If \"vers\" are given, they must be
   \"VersionNumber\" objects and they specify acceptable version
   intervals for \"pkg\".

"),

("Base.Pkg","rm","rm(pkg)

   Remove all requirement entries for \"pkg\" from
   \"Pkg.dir(\"REQUIRE\")\" and call \"Pkg.resolve()\".

"),

("Base.Pkg","clone","clone(pkg)

   If \"pkg\" has a URL registered in \"Pkg.dir(\"METADATA\")\", clone
   it from that URL on the default branch. The package does not need
   to have any registered versions.

"),

("Base.Pkg","clone","clone(pkg)

   If \"pkg\" has a URL registered in \"Pkg.dir(\"METADATA\")\", clone
   it from that URL on the default branch. The package does not need
   to have any registered versions.

"),

("Base.Pkg","available","available(pkg) -> Vector{VersionNumber}

   Returns the version numbers available for package \"pkg\".

"),

("Base.Pkg","available","available(pkg) -> Vector{VersionNumber}

   Returns the version numbers available for package \"pkg\".

"),

("Base.Pkg","installed","installed(pkg) -> Void | VersionNumber

   If \"pkg\" is installed, return the installed version number,
   otherwise return \"nothing\".

"),

("Base.Pkg","installed","installed(pkg) -> Void | VersionNumber

   If \"pkg\" is installed, return the installed version number,
   otherwise return \"nothing\".

"),

("Base.Pkg","status","status()

   Prints out a summary of what packages are installed and what
   version and state they're in.

"),

("Base.Pkg","update","update()

   Update package the metadata repo – kept in
   \"Pkg.dir(\"METADATA\")\" – then update any fixed packages that can
   safely be pulled from their origin; then call \"Pkg.resolve()\" to
   determine a new optimal set of packages versions.

"),

("Base.Pkg","checkout","checkout(pkg[, branch=\"master\"])

   Checkout the \"Pkg.dir(pkg)\" repo to the branch \"branch\".
   Defaults to checking out the \"master\" branch. To go back to using
   the newest compatible released version, use \"Pkg.free(pkg)\"

"),

("Base.Pkg","pin","pin(pkg, version)

   Pin \"pkg\" at registered version \"version\".

"),

("Base.Pkg","pin","pin(pkg, version)

   Pin \"pkg\" at registered version \"version\".

"),

("Base.Pkg","free","free(pkg)

   Free the package \"pkg\" to be managed by the package manager
   again. It calls \"Pkg.resolve()\" to determine optimal package
   versions after. This is an inverse for both \"Pkg.checkout\" and
   \"Pkg.pin\".

   You can also supply an iterable collection of package names, e.g.,
   \"Pkg.free((\"Pkg1\", \"Pkg2\"))\" to free multiple packages at
   once.

"),

("Base.Pkg","build","build(pkgs...)

   Run the build script in \"deps/build.jl\" for each package in
   \"pkgs\" and all of their dependencies in depth-first recursive
   order. This is called automatically by \"Pkg.resolve()\" on all
   installed or updated packages.

"),

("Base.Pkg","build","build(pkgs...)

   Run the build script in \"deps/build.jl\" for each package in
   \"pkgs\" and all of their dependencies in depth-first recursive
   order. This is called automatically by \"Pkg.resolve()\" on all
   installed or updated packages.

"),

("Base.Pkg","generate","generate(pkg, license)

   Generate a new package named \"pkg\" with one of these license
   keys: \"\"MIT\"\", \"\"BSD\"\" or \"\"ASL\"\". If you want to make
   a package with a different license, you can edit it afterwards.
   Generate creates a git repo at \"Pkg.dir(pkg)\" for the package and
   inside it \"LICENSE.md\", \"README.md\", the julia entrypoint
   \"\$pkg/src/\$pkg.jl\", and a travis test file, \".travis.yml\".

"),

("Base.Pkg","register","register(pkg[, url])

   Register \"pkg\" at the git URL \"url\", defaulting to the
   configured origin URL of the git repo \"Pkg.dir(pkg)\".

"),

("Base.Pkg","tag","tag(pkg[, ver[, commit]])

   Tag \"commit\" as version \"ver\" of package \"pkg\" and create a
   version entry in \"METADATA\". If not provided, \"commit\" defaults
   to the current commit of the \"pkg\" repo. If \"ver\" is one of the
   symbols \":patch\", \":minor\", \":major\" the next patch, minor or
   major version is used. If \"ver\" is not provided, it defaults to
   \":patch\".

"),

("Base.Pkg","publish","publish()

   For each new package version tagged in \"METADATA\" not already
   published, make sure that the tagged package commits have been
   pushed to the repo at the registered URL for the package and if
   they all have, open a pull request to \"METADATA\".

"),

("Base.Pkg","test","test(pkgs...)

   Run the tests for each package in \"pkgs\" ensuring that each
   package's test dependencies are installed for the duration of the
   test. A package is tested by running its \"test/runtests.jl\" file
   and test dependencies are specified in \"test/REQUIRE\".

"),

("Base.Pkg","test","test(pkgs...)

   Run the tests for each package in \"pkgs\" ensuring that each
   package's test dependencies are installed for the duration of the
   test. A package is tested by running its \"test/runtests.jl\" file
   and test dependencies are specified in \"test/REQUIRE\".

"),

("Base","@profile","@profile()

   \"@profile <expression>\" runs your expression while taking
   periodic backtraces.  These are appended to an internal buffer of
   backtraces.

"),

("Base.Profile","clear","clear()

   Clear any existing backtraces from the internal buffer.

"),

("Base.Profile","print","print([io::IO = STDOUT], data::Vector, lidict::Dict; format = :tree, combine = true, cols = tty_cols())

   Prints profiling results to \"io\". This variant is used to examine
   results exported by a previous call to \"retrieve()\". Supply the
   vector \"data\" of backtraces and a dictionary \"lidict\" of line
   information.

"),

("Base.Profile","print","print([io::IO = STDOUT], data::Vector, lidict::Dict; format = :tree, combine = true, cols = tty_cols())

   Prints profiling results to \"io\". This variant is used to examine
   results exported by a previous call to \"retrieve()\". Supply the
   vector \"data\" of backtraces and a dictionary \"lidict\" of line
   information.

"),

("Base.Profile","init","init(; n::Integer, delay::Float64)

   Configure the \"delay\" between backtraces (measured in seconds),
   and the number \"n\" of instruction pointers that may be stored.
   Each instruction pointer corresponds to a single line of code;
   backtraces generally consist of a long list of instruction
   pointers. Default settings can be obtained by calling this function
   with no arguments, and each can be set independently using keywords
   or in the order \"(n, delay)\".

"),

("Base.Profile","fetch","fetch() -> data

   Returns a reference to the internal buffer of backtraces. Note that
   subsequent operations, like \"clear()\", can affect \"data\" unless
   you first make a copy. Note that the values in \"data\" have
   meaning only on this machine in the current session, because it
   depends on the exact memory addresses used in JIT-compiling. This
   function is primarily for internal use; \"retrieve()\" may be a
   better choice for most users.

"),

("Base.Profile","retrieve","retrieve() -> data, lidict

   \"Exports\" profiling results in a portable format, returning the
   set of all backtraces (\"data\") and a dictionary that maps the
   (session-specific) instruction pointers in \"data\" to \"LineInfo\"
   values that store the file name, function name, and line number.
   This function allows you to save profiling results for future
   analysis.

"),

("Base.Profile","callers","callers(funcname[, data, lidict][, filename=<filename>][, linerange=<start:stop>]) -> Vector{Tuple{count, linfo}}

   Given a previous profiling run, determine who called a particular
   function. Supplying the filename (and optionally, range of line
   numbers over which the function is defined) allows you to
   disambiguate an overloaded method. The returned value is a vector
   containing a count of the number of calls and line information
   about the caller.  One can optionally supply backtrace data
   obtained from \"retrieve()\"; otherwise, the current internal
   profile buffer is used.

"),

("Base.Profile","clear_malloc_data","clear_malloc_data()

   Clears any stored memory allocation data when running julia with \"
   –track-allocation\".  Execute the command(s) you want to test (to
   force JIT-compilation), then call \"clear_malloc_data()\". Then
   execute your command(s) again, quit Julia, and examine the
   resulting \">>*<<.mem\" files.

"),


("Base","sort!","sort!(v, [alg=<algorithm>,] [by=<transform>,] [lt=<comparison>,] [rev=false])

   Sort the vector \"v\" in place. \"QuickSort\" is used by default
   for numeric arrays while \"MergeSort\" is used for other arrays.
   You can specify an algorithm to use via the \"alg\" keyword (see
   Sorting Algorithms for available algorithms). The \"by\" keyword
   lets you provide a function that will be applied to each element
   before comparison; the \"lt\" keyword allows providing a custom
   \"less than\" function; use \"rev=true\" to reverse the sorting
   order. These options are independent and can be used together in
   all possible combinations: if both \"by\" and \"lt\" are specified,
   the \"lt\" function is applied to the result of the \"by\"
   function; \"rev=true\" reverses whatever ordering specified via the
   \"by\" and \"lt\" keywords.

"),

("Base","sort","sort(A, dim, [alg=<algorithm>,] [by=<transform>,] [lt=<comparison>,] [rev=false])

   Sort a multidimensional array \"A\" along the given dimension.

"),

("Base","sort","sort(A, dim, [alg=<algorithm>,] [by=<transform>,] [lt=<comparison>,] [rev=false])

   Sort a multidimensional array \"A\" along the given dimension.

"),

("Base","sortperm","sortperm(v, [alg=<algorithm>,] [by=<transform>,] [lt=<comparison>,] [rev=false])

   Return a permutation vector of indices of \"v\" that puts it in
   sorted order. Specify \"alg\" to choose a particular sorting
   algorithm (see Sorting Algorithms). \"MergeSort\" is used by
   default, and since it is stable, the resulting permutation will be
   the lexicographically first one that puts the input array into
   sorted order – i.e. indices of equal elements appear in ascending
   order. If you choose a non-stable sorting algorithm such as
   \"QuickSort\", a different permutation that puts the array into
   order may be returned. The order is specified using the same
   keywords as \"sort!\".

   See also \"sortperm!()\"

"),

("Base","sortperm!","sortperm!(ix, v, [alg=<algorithm>,] [by=<transform>,] [lt=<comparison>,] [rev=false,] [initialized=false])

   Like \"sortperm\", but accepts a preallocated index vector \"ix\".
   If \"initialized\" is \"false\" (the default), ix is initialized to
   contain the values \"1:length(v)\".

   See also \"sortperm()\"

"),

("Base","sortrows","sortrows(A, [alg=<algorithm>,] [by=<transform>,] [lt=<comparison>,] [rev=false])

   Sort the rows of matrix \"A\" lexicographically.

"),

("Base","sortcols","sortcols(A, [alg=<algorithm>,] [by=<transform>,] [lt=<comparison>,] [rev=false])

   Sort the columns of matrix \"A\" lexicographically.

"),

("Base","issorted","issorted(v, [by=<transform>,] [lt=<comparison>,] [rev=false])

   Test whether a vector is in sorted order. The \"by\", \"lt\" and
   \"rev\" keywords modify what order is considered to be sorted just
   as they do for \"sort\".

"),

("Base","searchsorted","searchsorted(a, x, [by=<transform>,] [lt=<comparison>,] [rev=false])

   Returns the range of indices of \"a\" which compare as equal to
   \"x\" according to the order specified by the \"by\", \"lt\" and
   \"rev\" keywords, assuming that \"a\" is already sorted in that
   order. Returns an empty range located at the insertion point if
   \"a\" does not contain values equal to \"x\".

"),

("Base","searchsortedfirst","searchsortedfirst(a, x, [by=<transform>,] [lt=<comparison>,] [rev=false])

   Returns the index of the first value in \"a\" greater than or equal
   to \"x\", according to the specified order. Returns \"length(a)+1\"
   if \"x\" is greater than all values in \"a\".

"),

("Base","searchsortedlast","searchsortedlast(a, x, [by=<transform>,] [lt=<comparison>,] [rev=false])

   Returns the index of the last value in \"a\" less than or equal to
   \"x\", according to the specified order. Returns \"0\" if \"x\" is
   less than all values in \"a\".

"),

("Base","select!","select!(v, k, [by=<transform>,] [lt=<comparison>,] [rev=false])

   Partially sort the vector \"v\" in place, according to the order
   specified by \"by\", \"lt\" and \"rev\" so that the value at index
   \"k\" (or range of adjacent values if \"k\" is a range) occurs at
   the position where it would appear if the array were fully sorted
   via a non-stable algorithm. If \"k\" is a single index, that value
   is returned; if \"k\" is a range, an array of values at those
   indices is returned. Note that \"select!\" does not fully sort the
   input array.

"),

("Base","select","select(v, k, [by=<transform>,] [lt=<comparison>,] [rev=false])

   Variant of \"select!\" which copies \"v\" before partially sorting
   it, thereby returning the same thing as \"select!\" but leaving
   \"v\" unmodified.

"),

("Base","length","length(s)

   The number of characters in string \"s\".

"),

("Base","sizeof","sizeof(s::AbstractString)

   The number of bytes in string \"s\".

"),

("Base","*","*(s, t)

   Concatenate strings. The \"*\" operator is an alias to this
   function.

      julia> \"Hello \" * \"world\"
      \"Hello world\"


          julia> \"Hello \" * \"world\"
          \"Hello world\"

"),

("Base","^","^(s, n)

   Repeat \"n\" times the string \"s\". The \"^\" operator is an alias
   to this function.

      julia> \"Test \"^3
      \"Test Test Test \"

"),

("Base","string","string(xs...)

   Create a string from any values using the \"print\" function.

"),

("Base","repr","repr(x)

   Create a string from any value using the \"showall\" function.

"),

("Base","bytestring","bytestring(s)

   Convert a string to a contiguous byte array representation
   appropriate for passing it to C functions. The string will be
   encoded as either ASCII or UTF-8.

"),

("Base","bytestring","bytestring(s)

   Convert a string to a contiguous byte array representation
   appropriate for passing it to C functions. The string will be
   encoded as either ASCII or UTF-8.

"),

("Base","ascii","ascii(::Ptr{UInt8}[, length])

   Create an ASCII string from the address of a C (0-terminated)
   string encoded in ASCII. A copy is made; the ptr can be safely
   freed. If \"length\" is specified, the string does not have to be
   0-terminated.

"),

("Base","ascii","ascii(::Ptr{UInt8}[, length])

   Create an ASCII string from the address of a C (0-terminated)
   string encoded in ASCII. A copy is made; the ptr can be safely
   freed. If \"length\" is specified, the string does not have to be
   0-terminated.

"),

("Base","ascii","ascii(::Ptr{UInt8}[, length])

   Create an ASCII string from the address of a C (0-terminated)
   string encoded in ASCII. A copy is made; the ptr can be safely
   freed. If \"length\" is specified, the string does not have to be
   0-terminated.

"),

("Base","utf8","utf8(s)

   Convert a string to a contiguous UTF-8 string (all characters must
   be valid UTF-8 characters).

"),

("Base","utf8","utf8(s)

   Convert a string to a contiguous UTF-8 string (all characters must
   be valid UTF-8 characters).

"),

("Base","utf8","utf8(s)

   Convert a string to a contiguous UTF-8 string (all characters must
   be valid UTF-8 characters).

"),

("Base","normalize_string","normalize_string(s, normalform::Symbol)

   Normalize the string \"s\" according to one of the four \"normal
   forms\" of the Unicode standard: \"normalform\" can be \":NFC\",
   \":NFD\", \":NFKC\", or \":NFKD\".  Normal forms C (canonical
   composition) and D (canonical decomposition) convert different
   visually identical representations of the same abstract string into
   a single canonical form, with form C being more compact.  Normal
   forms KC and KD additionally canonicalize \"compatibility
   equivalents\": they convert characters that are abstractly similar
   but visually distinct into a single canonical choice (e.g. they
   expand ligatures into the individual characters), with form KC
   being more compact.

   Alternatively, finer control and additional transformations may be
   be obtained by calling *normalize_string(s; keywords...)*, where
   any number of the following boolean keywords options (which all
   default to \"false\" except for \"compose\") are specified:

      * \"compose=false\": do not perform canonical composition

      * \"decompose=true\": do canonical decomposition instead of
        canonical composition (\"compose=true\" is ignored if present)

      * \"compat=true\": compatibility equivalents are canonicalized

      * \"casefold=true\": perform Unicode case folding, e.g. for
        case-   insensitive string comparison

      * \"newline2lf=true\", \"newline2ls=true\", or
        \"newline2ps=true\": convert various newline sequences (LF,
        CRLF,   CR, NEL) into a linefeed (LF), line-separation (LS),
        or   paragraph-separation (PS) character, respectively

      * \"stripmark=true\": strip diacritical marks (e.g. accents)

      * \"stripignore=true\": strip Unicode's \"default ignorable\"
        characters (e.g. the soft hyphen or the left-to-right marker)

      * \"stripcc=true\": strip control characters; horizontal tabs
        and   form feeds are converted to spaces; newlines are also
        converted   to spaces unless a newline-conversion flag was
        specified

      * \"rejectna=true\": throw an error if unassigned code points
        are   found

      * \"stable=true\": enforce Unicode Versioning Stability

   For example, NFKC corresponds to the options \"compose=true,
   compat=true, stable=true\".

"),

("Base","graphemes","graphemes(s) -> iterator over substrings of s

   Returns an iterator over substrings of \"s\" that correspond to the
   extended graphemes in the string, as defined by Unicode UAX #29.
   (Roughly, these are what users would perceive as single characters,
   even though they may contain more than one codepoint; for example a
   letter combined with an accent mark is a single grapheme.)

"),

("Base","isvalid","isvalid(str, i)

   Tells whether index \"i\" is valid for the given string

"),

("Base","isvalid","isvalid(str, i)

   Tells whether index \"i\" is valid for the given string

"),

("Base","is_assigned_char","is_assigned_char(c) -> Bool

   Returns true if the given char or integer is an assigned Unicode
   code point.

"),

("Base","ismatch","ismatch(r::Regex, s::AbstractString) -> Bool

   Test whether a string contains a match of the given regular
   expression.

"),

("Base","match","match(r::Regex, s::AbstractString[, idx::Integer[, addopts]])

   Search for the first match of the regular expression \"r\" in \"s\"
   and return a RegexMatch object containing the match, or nothing if
   the match failed. The matching substring can be retrieved by
   accessing \"m.match\" and the captured sequences can be retrieved
   by accessing \"m.captures\" The optional \"idx\" argument specifies
   an index at which to start the search.

"),

("Base","eachmatch","eachmatch(r::Regex, s::AbstractString[, overlap::Bool=false])

   Search for all matches of a the regular expression \"r\" in \"s\"
   and return a iterator over the matches. If overlap is true, the
   matching sequences are allowed to overlap indices in the original
   string, otherwise they must be from distinct character ranges.

"),

("Base","matchall","matchall(r::Regex, s::AbstractString[, overlap::Bool=false]) -> Vector{AbstractString}

   Return a vector of the matching substrings from eachmatch.

"),

("Base","lpad","lpad(string, n, p)

   Make a string at least \"n\" columns wide when printed, by padding
   on the left with copies of \"p\".

"),

("Base","rpad","rpad(string, n, p)

   Make a string at least \"n\" columns wide when printed, by padding
   on the right with copies of \"p\".

"),

("Base","search","search(string, chars[, start])

   Search for the first occurrence of the given characters within the
   given string. The second argument may be a single character, a
   vector or a set of characters, a string, or a regular expression
   (though regular expressions are only allowed on contiguous strings,
   such as ASCII or UTF-8 strings). The third argument optionally
   specifies a starting index. The return value is a range of indexes
   where the matching sequence is found, such that \"s[search(s,x)] ==
   x\":

   \"search(string, \"substring\")\" = \"start:end\" such that
   \"string[start:end] == \"substring\"\", or \"0:-1\" if unmatched.

   \"search(string, 'c')\"         = \"index\" such that
   \"string[index] == 'c'\", or \"0\" if unmatched.

"),

("Base","rsearch","rsearch(string, chars[, start])

   Similar to \"search\", but returning the last occurrence of the
   given characters within the given string, searching in reverse from
   \"start\".

"),

("Base","searchindex","searchindex(string, substring[, start])

   Similar to \"search\", but return only the start index at which the
   substring is found, or 0 if it is not.

"),

("Base","rsearchindex","rsearchindex(string, substring[, start])

   Similar to \"rsearch\", but return only the start index at which
   the substring is found, or 0 if it is not.

"),

("Base","contains","contains(haystack, needle)

   Determine whether the second argument is a substring of the first.

"),

("Base","replace","replace(string, pat, r[, n])

   Search for the given pattern \"pat\", and replace each occurrence
   with \"r\". If \"n\" is provided, replace at most \"n\"
   occurrences.  As with search, the second argument may be a single
   character, a vector or a set of characters, a string, or a regular
   expression. If \"r\" is a function, each occurrence is replaced
   with \"r(s)\" where \"s\" is the matched substring.

"),

("Base","split","split(string, [chars]; limit=0, keep=true)

   Return an array of substrings by splitting the given string on
   occurrences of the given character delimiters, which may be
   specified in any of the formats allowed by \"search\"'s second
   argument (i.e. a single character, collection of characters,
   string, or regular expression). If \"chars\" is omitted, it
   defaults to the set of all space characters, and \"keep\" is taken
   to be false. The two keyword arguments are optional: they are are a
   maximum size for the result and a flag determining whether empty
   fields should be kept in the result.

"),

("Base","rsplit","rsplit(string, [chars]; limit=0, keep=true)

   Similar to \"split\", but starting from the end of the string.

"),

("Base","strip","strip(string[, chars])

   Return \"string\" with any leading and trailing whitespace removed.
   If \"chars\" (a character, or vector or set of characters) is
   provided, instead remove characters contained in it.

"),

("Base","lstrip","lstrip(string[, chars])

   Return \"string\" with any leading whitespace removed. If \"chars\"
   (a character, or vector or set of characters) is provided, instead
   remove characters contained in it.

"),

("Base","rstrip","rstrip(string[, chars])

   Return \"string\" with any trailing whitespace removed. If
   \"chars\" (a character, or vector or set of characters) is
   provided, instead remove characters contained in it.

"),

("Base","startswith","startswith(string, prefix | chars)

   Returns \"true\" if \"string\" starts with \"prefix\". If the
   second argument is a vector or set of characters, tests whether the
   first character of \"string\" belongs to that set.

"),

("Base","endswith","endswith(string, suffix | chars)

   Returns \"true\" if \"string\" ends with \"suffix\". If the second
   argument is a vector or set of characters, tests whether the last
   character of \"string\" belongs to that set.

"),

("Base","uppercase","uppercase(string)

   Returns \"string\" with all characters converted to uppercase.

"),

("Base","lowercase","lowercase(string)

   Returns \"string\" with all characters converted to lowercase.

"),

("Base","ucfirst","ucfirst(string)

   Returns \"string\" with the first character converted to uppercase.

"),

("Base","lcfirst","lcfirst(string)

   Returns \"string\" with the first character converted to lowercase.

"),

("Base","join","join(strings, delim[, last])

   Join an array of \"strings\" into a single string, inserting the
   given delimiter between adjacent strings. If \"last\" is given, it
   will be used instead of \"delim\" between the last two strings. For
   example, \"join([\"apples\", \"bananas\", \"pineapples\"], \", \",
   \" and \") == \"apples, bananas and pineapples\"\".

   \"strings\" can be any iterable over elements \"x\" which are
   convertible to strings via \"print(io::IOBuffer, x)\".

"),

("Base","chop","chop(string)

   Remove the last character from a string

"),

("Base","chomp","chomp(string)

   Remove a trailing newline from a string

"),

("Base","ind2chr","ind2chr(string, i)

   Convert a byte index to a character index

"),

("Base","chr2ind","chr2ind(string, i)

   Convert a character index to a byte index

"),

("Base","isvalid","isvalid(str, i)

   Tells whether index \"i\" is valid for the given string

"),

("Base","nextind","nextind(str, i)

   Get the next valid string index after \"i\". Returns a value
   greater than \"endof(str)\" at or after the end of the string.

"),

("Base","prevind","prevind(str, i)

   Get the previous valid string index before \"i\". Returns a value
   less than \"1\" at the beginning of the string.

"),

("Base","randstring","randstring([rng], len=8)

   Create a random ASCII string of length \"len\", consisting of
   upper- and lower-case letters and the digits 0-9. The optional
   \"rng\" argument specifies a random number generator, see *Random
   Numbers*.

"),

("Base","charwidth","charwidth(c)

   Gives the number of columns needed to print a character.

"),

("Base","strwidth","strwidth(s)

   Gives the number of columns needed to print a string.

"),

("Base","isalnum","isalnum(c::Union{Char, AbstractString}) -> Bool

   Tests whether a character is alphanumeric, or whether this is true
   for all elements of a string.  A character is classified as
   alphabetic if it belongs to the Unicode general category Letter or
   Number, i.e. a character whose category code begins with 'L' or
   'N'.

"),

("Base","isalpha","isalpha(c::Union{Char, AbstractString}) -> Bool

   Tests whether a character is alphabetic, or whether this is true
   for all elements of a string. A character is classified as
   alphabetic if it belongs to the Unicode general category Letter,
   i.e. a character whose category code begins with 'L'.

"),

("Base","isascii","isascii(c::Union{Char, AbstractString}) -> Bool

   Tests whether a character belongs to the ASCII character set, or
   whether this is true for all elements of a string.

"),

("Base","iscntrl","iscntrl(c::Union{Char, AbstractString}) -> Bool

   Tests whether a character is a control character, or whether this
   is true for all elements of a string.  Control characters are the
   non-printing characters of the Latin-1 subset of Unicode.

"),

("Base","isdigit","isdigit(c::Union{Char, AbstractString}) -> Bool

   Tests whether a character is a numeric digit (0-9), or whether this
   is true for all elements of a string.

"),

("Base","isgraph","isgraph(c::Union{Char, AbstractString}) -> Bool

   Tests whether a character is printable, and not a space, or whether
   this is true for all elements of a string.  Any character that
   would cause a printer to use ink should be classified with
   isgraph(c)==true.

"),

("Base","islower","islower(c::Union{Char, AbstractString}) -> Bool

   Tests whether a character is a lowercase letter, or whether this is
   true for all elements of a string.  A character is classified as
   lowercase if it belongs to Unicode category Ll, Letter: Lowercase.

"),

("Base","isnumber","isnumber(c::Union{Char, AbstractString}) -> Bool

   Tests whether a character is numeric, or whether this is true for
   all elements of a string.   A character is classified as numeric if
   it belongs to the Unicode general category Number, i.e. a character
   whose category code begins with 'N'.

"),

("Base","isprint","isprint(c::Union{Char, AbstractString}) -> Bool

   Tests whether a character is printable, including spaces, but not a
   control character. For strings, tests whether this is true for all
   elements of the string.

"),

("Base","ispunct","ispunct(c::Union{Char, AbstractString}) -> Bool

   Tests whether a character belongs to the Unicode general category
   Punctuation, i.e. a character whose category code begins with 'P'.
   For strings, tests whether this is true for all elements of the
   string.

"),

("Base","isspace","isspace(c::Union{Char, AbstractString}) -> Bool

   Tests whether a character is any whitespace character.  Includes
   ASCII characters 't', 'n', 'v', 'f', 'r', and ' ', Latin-1
   character U+0085, and characters in Unicode category Zs. For
   strings, tests whether this    is true for all elements of the
   string.

"),

("Base","isupper","isupper(c::Union{Char, AbstractString}) -> Bool

   Tests whether a character is an uppercase letter, or whether this
   is true for all elements of a string.    A character is classified
   as uppercase if it belongs to Unicode category Lu, Letter:
   Uppercase, or Lt, Letter: Titlecase.

"),

("Base","isxdigit","isxdigit(c::Union{Char, AbstractString}) -> Bool

   Tests whether a character is a valid hexadecimal digit, or whether
   this is true for all elements of a string.

"),

("Base","symbol","symbol(x...) -> Symbol

   Create a \"Symbol\" by concatenating the string representations of
   the arguments together.

"),

("Base","escape_string","escape_string(str::AbstractString) -> AbstractString

   General escaping of traditional C and Unicode escape sequences. See
   \"print_escaped()\" for more general escaping.

"),

("Base","unescape_string","unescape_string(s::AbstractString) -> AbstractString

   General unescaping of traditional C and Unicode escape sequences.
   Reverse of \"escape_string()\". See also \"print_unescaped()\".

"),

("Base","utf16","utf16(::Union{Ptr{UInt16}, Ptr{Int16}}[, length])

   Create a string from the address of a NUL-terminated UTF-16 string.
   A copy is made; the pointer can be safely freed. If \"length\" is
   specified, the string does not have to be NUL-terminated.

"),

("Base","utf16","utf16(::Union{Ptr{UInt16}, Ptr{Int16}}[, length])

   Create a string from the address of a NUL-terminated UTF-16 string.
   A copy is made; the pointer can be safely freed. If \"length\" is
   specified, the string does not have to be NUL-terminated.

"),

("Base","wstring","wstring(s)

   This is a synonym for either \"utf32(s)\" or \"utf16(s)\",
   depending on whether \"Cwchar_t\" is 32 or 16 bits, respectively.
   The synonym \"WString\" for \"UTF32String\" or \"UTF16String\" is
   also provided.

"),

("Base","wstring","wstring(s)

   This is a synonym for either \"utf32(s)\" or \"utf16(s)\",
   depending on whether \"Cwchar_t\" is 32 or 16 bits, respectively.
   The synonym \"WString\" for \"UTF32String\" or \"UTF16String\" is
   also provided.

"),

("Base","wstring","wstring(s)

   This is a synonym for either \"utf32(s)\" or \"utf16(s)\",
   depending on whether \"Cwchar_t\" is 32 or 16 bits, respectively.
   The synonym \"WString\" for \"UTF32String\" or \"UTF16String\" is
   also provided.

"),

("Base","runtests","runtests([tests=[\"all\"][, numcores=iceil(CPU_CORES/2)]])

   Run the Julia unit tests listed in \"tests\", which can be either a
   string or an array of strings, using \"numcores\" processors. (not
   exported)

"),

("Base.Test","@test","@test(ex)

   Test the expression \"ex\" and calls the current handler to handle
   the result.

"),

("Base.Test","@test_throws","@test_throws(extype, ex)

   Test that the expression \"ex\" throws an exception of type
   \"extype\" and calls the current handler to handle the result.

"),

("Base.Test","@test_approx_eq","@test_approx_eq(a, b)

   Test two floating point numbers \"a\" and \"b\" for equality taking
   in account small numerical errors.

"),

("Base.Test","@test_approx_eq_eps","@test_approx_eq_eps(a, b, tol)

   Test two floating point numbers \"a\" and \"b\" for equality taking
   in account a margin of tolerance given by \"tol\".

"),

("Base.Test","with_handler","with_handler(f, handler)

   Run the function \"f\" using the \"handler\" as the handler.

"),


]
