import Base.Sort: QuickSort, MergeSort, InsertionSort

Pkg.add("SortingAlgorithms")
using SortingAlgorithms #Provides the other sorting algorithms

include("../perfutil.jl")

sorts = [InsertionSort, QuickSort, MergeSort, HeapSort, RadixSort, TimSort]

randstr_fn!(str_len::Int) = d -> (for i = 1:length(d); d[i] = randstring(str_len); end; d)
randint_fn!(m::Int) = d -> rand!(d, 1:m)

# If we're reporting to codespeed, only do a few tests.
if codespeed
    for (T, typename, randfn!) in [(Int, string(Int), randint_fn!(10)),
                                    (AbstractString, "String_10", randstr_fn!(10))]
        for size in [2^6,2^16]
            for s in sorts
                if s == InsertionSort && size != 2^6; continue; end
                data = Array(T, size)
                gc()

                ## Random
                name = "$(typename)_$(size)_$(string(s)[1:end-5])_random"
                desc = "$(string(s)) run on $(size) $(typename) elements in random order"
                @timeit_init(sort!(data, alg=s), randfn!(data), name, "")

                name = "$(typename)_$(size)_$(string(s)[1:end-5])_append"
                desc = "$(string(s)) run on $(size) $(typename) elements pre-sorted 10 random elements appended"
                @timeit_init(sort!(data, alg=s), begin data[end-9:end]=randfn!(Array(T,10)) end, name, "")
            end
        end
    end
else
    for (T, typename, randfn!) in [(Int, string(Int), randint_fn!(10)),
                                    (Float64, string(Float64), rand!),
                                    (AbstractString, "String_05", randstr_fn!(5)),
                                    (AbstractString, "String_10", randstr_fn!(10))]
        for logsize = 6:2:18
            size = 2^logsize
            for s in sorts
                if s == RadixSort && T == AbstractString continue end      #Radix sort not implemented
                if s == InsertionSort && logsize >=14 continue end #Too slow
                println(s, s==RadixSort, s, typename, typename==AbstractString, logsize)
                data = Array(T, size)
                gc()

                ## Random
                name = "$(typename)_$(logsize)_$(string(s)[1:end-5])_random"
                @timeit_init(sort!(data, alg=s), randfn!(data), name, "")

                ## Sorted
                name = "$(typename)_$(logsize)_$(string(s)[1:end-5])_sorted"
                @timeit(sort!(data, alg=s), name, "")

                ## Reverse sorted
                name = "$(typename)_$(logsize)_$(string(s)[1:end-5])_reversed"
                @timeit_init(sort!(data, alg=s), reverse!(data), name, "")

                ## Sorted with 3 exchanges
                name = "$(typename)_$(logsize)_$(string(s)[1:end-5])_3exchanges"
                @timeit_init(sort!(data, alg=s),
                             begin
                                 for i = 1:3
                                     n1 = rand(1:size)
                                     n2 = rand(1:size)
                                     data[n1], data[n2] = data[n2], data[n1]
                                 end
                             end,
                             name, "")

                ## Sorted with 10 unsorted values appended
                name = "$(typename)_$(logsize)_$(string(s)[1:end-5])_append"
                @timeit_init(sort!(data, alg=s), begin data[end-9:end]=randfn!(Array(T,10)) end, name, "")

                ## Random data with 4 unique values
                name = "$(typename)_$(logsize)_$(string(s)[1:end-5])_4unique"
                @timeit_init(sort!(data4, alg=s), begin data4=data[rand(1:4,size)] end, name, "")

                ## All values equal
                name = "$(typename)_$(logsize)_$(string(s)[1:end-5])_allequal"
                data1 = data[ones(Int, size)]
                @timeit(sort!(data1, alg=s), name, "")

                ## QuickSort median killer
                if s == QuickSort && logsize > 16; continue; end  # too slow!

                name = "$(typename)_$(logsize)_$(string(s)[1:end-5])_qsortkiller"
                data = data[1:size>>1]
                data = sort!(data, alg=s)
                data = vcat(reverse(data), data)
                @timeit_init(sort!(qdata, alg=s), begin qdata=copy(data) end, name, "")
            end
        end
    end
end
