n = 100000
strs = [randstring(10) for i = 1:n];
nums = rand(Int, n);

# regular map

function dict_insertion_test{T<:Associative}(d::T)
    #empty!(d)
    for i = 1:n
        d[strs[i]] = nums[i]
    end
    d
end

function dict_deletion_test{T<:Associative}(d::T, iters::Int)
    #dict_insertion_test(d)
    for i in rand(1:n, iters)
        delete!(d, strs[i], 0)
    end
    d
end

function dict_ins_del_test{T<:Associative}(d::T, iters::Int)
    #dict_insertion_test(d)
    for i in rand(1:n, iters)
        randbool()? delete!(d, strs[i], 0) : (d[strs[i]] = nums[i])
    end
    d
end    


function test_ins(T::Type, iter::Int)
    d = T{String,Int}()
    t = 0.0
    for i = 1:iter
      empty!(d)
      gc()
      t += @elapsed dict_insertion_test(d)
   end
   t
end

function test_ins_del(T::Type, iter::Int)
    d = T{String,Int}()
    t = 0.0
    for i = 1:iter
      empty!(d)
      dict_insertion_test(d)
      gc()
      t += @elapsed dict_ins_del_test(d, 100000)
   end
   t
end

function test_del(T::Type, iter::Int)
    d = T{String,Int}()
    t = 0.0
    for i = 1:iter
      empty!(d)
      dict_insertion_test(d)
      gc()
      t += @elapsed dict_deletion_test(d, 100000)
   end
   t
end

function run_all()
    for test in [test_ins, test_del, test_ins_del]
        println(test)
        println("="^length(string(test)))
        for T in [Dict, OrderedDict]
            print("$T: ")
            times = Float64[test(T, 5) for i = 1:5]
            println("$times, median=$(median(times))")
        end
        println()
    end
end