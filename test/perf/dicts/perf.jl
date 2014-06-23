## Sparse matrix performance
include("../perfutil.jl")
ntrials = 5

# TODO: remove the two macros 
macro output_timings(t,name,desc,group)
    quote
        # If we weren't given anything for the test group, infer off of file path!
        # test_group = length($group) == 0 ? basename(dirname(Base.source_path())) : $group[1]
        if codespeed
            submit_to_codespeed( $t, $name, $desc, "seconds", test_group )
        elseif print_output
            @printf "%-6.2f,  %-6.2f,  %-6.2f,  %-6.2f,  %s\n" minimum($t) maximum($t) mean($t) std($t)  $name 
        end
        gc()        
    end
end

macro timeit(ex,name,desc,group...)
    quote
        t = zeros(ntrials)
        for i=0:ntrials
            e = 1000*(@elapsed $(esc(ex)))
            if i > 0
                # warm up on first iteration
                t[i] = e
            end
        end
        @output_timings t $name $desc $group
    end
end
# TODO: end remove

macro gc_disable(ex)
    quote
        gc_disable()
        $ex
        gc_enable()
    end
end

# TODO: update:
DictsToTest = [Dict, ObjectIdDict] #, WeakKeyDict, Base.ObjectIdDict2, Base.WeakObjectIdDict, Base.OrderedDict]
srand(1)
obidtest = true  # if set to false test for ObjectIdDict will error

function dict_unittests(DictToTest)
    # dict unittests from ../../collections.jl with bits commented
    # which do not pass in v0.2.1

    gc_disable()  # needed to work with Weak-dicts
    # dict
    h = DictToTest()
    for i=1:10000
        h[i] = i+1
    end
    for i=1:10000
        (h[i] == i+1)
    end
    for i=1:2:10000
        delete!(h, i)
    end
    for i=1:2:10000
        h[i] = i+1
    end
    for i=1:10000
        (h[i] == i+1)
    end
    for i=1:10000
        delete!(h, i)
    end
    isempty(h)
    h[77] = 100
    h[77]==100
    for i=1:10000
        h[i] = i+1
    end
    for i=1:2:10000
        delete!(h, i)
    end
    for i=10001:20000
        h[i] = i+1
    end
    for i=2:2:10000
        h[i]==i+1
    end
    for i=10000:20000
        h[i]==i+1
    end
    h = {"a" => 3}
    h["a"] == 3

    let
        z = DictToTest()
        get_KeyError = false
        try
            z["a"]
        catch _e123_
            get_KeyError = isa(_e123_,KeyError)
        end
        get_KeyError
    end

    _d = {"a"=>0}
    isa([k for k in filter(x->length(x)==1, collect(keys(_d)))], Vector{Any})

    # issue #1821
    if !(obidtest)
        let
            d = DictToTest{UTF8String, Vector{Int}}()
            d["a"] = [1, 2]
            d["b"] = [1]
            isa(repr(d), String)  # check that printable without error
        end
    end

    # issue #2344
    let
        local bar
        bestkey(d, key) = key
        bestkey{K<:String,V}(d::Associative{K,V}, key) = string(key)
        bar(x) = bestkey(x, :y)
        bar([:x => [1,2,5]]) == :y
        bar(["x" => [1,2,5]]) == "y"
    end


    isequal(DictToTest(), DictToTest())
    isequal({1 => 1}, {1 => 1})
    !isequal({1 => 1}, {})
    !isequal({1 => 1}, {1 => 2})
    !isequal({1 => 1}, {2 => 1})

    # Generate some data to populate dicts to be compared
    data_in = [ (rand(1:1000), randstring(2)) for _ in 1:1001 ]

    # Populate the first dict
    if obidtest
        d1 = DictToTest()
    else
        d1 = DictToTest{Int, String}()
    end
    for (k,v) in data_in
        d1[k] = v
    end
    data_in = collect(d1)
    # shuffle the data
    for i in 1:length(data_in)
        j = rand(1:length(data_in))
        data_in[i], data_in[j] = data_in[j], data_in[i]
    end
    # Inserting data in different (shuffled) order should result in
    # equivalent dict.
    if obidtest
        d2 = DictToTest()
    else
        d2 = DictToTest{Int, String}()
    end
    for (k,v) in data_in
        d2[k] = v
    end

    isequal(d1, d2)
    d3 = copy(d2)
    d4 = copy(d2)
    # Removing an item gives different dict
    delete!(d1, data_in[rand(1:length(data_in))][1])
    !isequal(d1, d2)
    # Changing a value gives different dict
    d3[data_in[rand(1:length(data_in))][1]] = randstring(3)
    !isequal(d1, d3)
    # Adding a pair gives different dict
    d4[1001] = randstring(3)
    !isequal(d1, d4)

    if !(obidtest)
        isequal(DictToTest(), sizehint(DictToTest(),96))
    end

    # Here is what currently happens when dictionaries of different types
    # are compared. This is not necessarily desirable. These tests are
    # descriptive rather than proscriptive.
    !isequal({1 => 2}, {"dog" => "bone"})
    if !(obidtest)
        isequal(DictToTest{Int, Int}(), DictToTest{String, String}())
    end

    # get! (get with default values assigned to the given location)

    # let f(x) = x^2,
    #     d = {8=>19},
    #     def = {}

    #     # get!(d, 8, 5) == 19
    #     # get!(d, 19, 2) == 2

    #     get!(d, 42) do  # d is updated with f(2)
    #         f(2)
    #     end == 4

    #     get!(d, 42) do  # d is not updated
    #         f(200)
    #     end == 4

    #     get(d, 13) do   # d is not updated
    #         f(4)
    #     end == 16

    #     d == {8=>19, 19=>2, 42=>4}
    # end

    # # show
    # for d in (["\n" => "\n", "1" => "\n", "\n" => "2"],
    #           [string(i) => i for i = 1:30],
    #           [reshape(1:i^2,i,i) => reshape(1:i^2,i,i) for i = 1:24],
    #           [utf8(Char['α':'α'+i]) => utf8(Char['α':'α'+i]) for i = (1:10)*10])
    #     for cols in (12, 40, 80), rows in (2, 10, 24)
    #         # Ensure output is limited as requested
    #         s = IOBuffer()
    #         Base.showdict(s, d, limit=true, sz=(rows, cols))
    #         out = split(takebuf_string(s),'\n')
    #         for line in out[2:end]
    #             strwidth(line) <= cols
    #         end
    #         length(out) <= rows

    #         for f in (keys, values)
    #             s = IOBuffer()
    #             Base.showkv(s, f(d), limit=true, sz=(rows, cols))
    #             out = split(takebuf_string(s),'\n')
    #             for line in out[2:end]
    #                 strwidth(line) <= cols
    #             end
    #             length(out) <= rows
    #         end
    #     end
    #     # Simply ensure these do not throw errors
    #     Base.showdict(IOBuffer(), d, limit=false)
    #     !isempty(summary(d))
    #     !isempty(summary(keys(d)))
    #     !isempty(summary(values(d)))
    # end


    # issue #2540
    d = {x => 1
         for x in ['a', 'b', 'c']}
    d == {'a'=>1, 'b'=>1, 'c'=> 1}

    # issue #2629
    d = (String => String)[ a => "foo" for a in ["a","b","c"]]
    d == ["a"=>"foo","b"=>"foo","c"=>"foo"]

    # # issue #5886
    # d5886 = DictToTest()
    # for k5886 in 1:11
    #     d5886[k5886] = 1
    # end
    # for k5886 in keys(d5886)
    #     # undefined ref if not fixed
    #     d5886[k5886] += 1
    # end

    # ############# end of dict tests #############

    gc_enable()
end


# Performance tests adapted from Kevin Squire:
# https://gist.github.com/kmsquire/5147894
n = 10^5
strs = [randstring(10) for i = 1:n]
nums = rand(Int, n)
nums2 = rand(Int, n)

randp = randperm(n)
randvec = rand(1:n, 10^5)

# performance tests
function dict_insertion_test(d::Associative, keys)
    for i = randp
        d[keys[i]] = nums[i]
    end
    d
end

function dict_deletion_test(d::Associative, keys)
    for i in randvec
        pop!(d, keys[i], 0)
    end
    d
end

function dict_ins_del_test(d::Associative, keys)
    for i in randvec
        randbool()? pop!(d, keys[i], 0) : (d[keys[i]] = nums[i])
    end
    d
end    

function dict_iterator(d::Associative)
    acc = 0
    for (k,v) in d
        acc += length(k) + v
    end
    acc
end    

## runners
function test_insert(T::Type, keys)
    d = (T==ObjectIdDict) ? T() : T{eltype(keys),Int}()
    @gc_disable @timeit dict_insertion_test(d, keys) "$T\_ins" "$T: insertion tests"
end

function test_delete(T::Type, keys)
    d = (T==ObjectIdDict) ? T() : T{eltype(keys),Int}()
    dict_insertion_test(d, keys) # fill d
    @gc_disable @timeit dict_deletion_test(d,keys) "$T\_del" "$T: deletion tests"
end

function test_insert_delete(T::Type, keys)
    d = (T==ObjectIdDict) ? T() : T{eltype(keys),Int}()
    dict_insertion_test(d, keys) # fill d
    @gc_disable @timeit dict_ins_del_test(d,keys) "$T\_ins_del" "$T: insertion and deletion tests"
end

function test_iterations(T::Type, keys)
    d = (T==ObjectIdDict) ? T() : T{eltype(keys),Int}()
    dict_insertion_test(d, keys) # fill d
    @gc_disable @timeit dict_iterator(d)  "$T\_iter" "$T: iteration tests"
end

function run(T)
    for (keys,desc) in {(strs,"str"), (nums2,"Int")}
        println(desc) # TODO remove
        for test in [test_insert, test_delete, test_insert_delete, test_iterations]
            times = test(T, keys)
        end
    end
end

@printf "min   , max    , mean   , std    , name\n" # TODO: remove 
for DictToTest in DictsToTest
    println(" ") # TODO: remove 
    @timeit dict_unittests(DictToTest) "$DictToTest\_unitt" "$DictToTest: dict-unit tests"
    run(DictToTest)
end

