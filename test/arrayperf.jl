n_evals = 1e7
run_ref = true
run_assign = true

if run_ref
    println("#### Ref ####")
    println("Whole array operations:")
    println("Small arrays:")
    lensmall = 4
    for n_dims in 1:10
        sz = ntuple(n_dims,i->lensmall)
        A = randn(sz)
        n_el = prod(sz)
        n_r = ceil(Int,n_evals/n_el)
        print(n_dims, " dimensions (", n_r, " repeats, ", n_r*n_el, " operations): ")
        print(n_dims, " dimensions (", n_r, " repeats): ")
        @time for i = 1:n_r
            B = A[:]
        end
    end
    println("Big arrays:")
    lenbig = [1000000,1000,100,32,16,10,10]
    for n_dims in 1:length(lenbig)
        sz = ntuple(n_dims,i->lenbig[n_dims])
        A = randn(sz)
        n_el = prod(sz)
        n_r = ceil(Int,n_evals/n_el)
        print(n_dims, " dimensions (", n_r, " repeats, ", n_r*n_el, " operations): ")
        print(n_dims, " dimensions (", n_r, " repeats): ")
        @time for i = 1:n_r
            B = A[:]
        end
    end
    println("\n")

    println("Slicing with contiguous blocks:")
    println("Small arrays:")
    for n_dims in 1:10
        sz = ntuple(n_dims,i->lensmall)
        A = randn(sz)
        ind = ntuple(n_dims,i -> ((i <= ceil(n_dims/2)) ? (1:sz[i]) : (rand(1:sz[i]))))
        n_el = prod(map(length,ind))
        n_r = ceil(Int,n_evals/n_el)
        print(n_dims, " dimensions (", n_r, " repeats, ", n_r*n_el, " operations): ")
        @time for i = 1:n_r
            #        ind = ntuple(n_dims,i -> ((i <= n_dims/2) ? (1:sz[i]) : (rand(1:sz[i]))))
            B = A[ind...]
        end
    end
    println("Big arrays:")
    for n_dims in 1:length(lenbig)
        sz = ntuple(n_dims,i->lenbig[n_dims])
        A = randn(sz)
        ind = ntuple(n_dims,i -> ((i <= ceil(n_dims/2)) ? (1:sz[i]) : (rand(1:sz[i]))))
        n_el = prod(map(length,ind))
        n_r = ceil(Int,n_evals/n_el)
        print(n_dims, " dimensions (", n_r, " repeats, ", n_r*n_el, " operations): ")
        @time for i = 1:n_r
            #        ind = ntuple(n_dims,i -> ((i <= n_dims/2) ? (1:sz[i]) : (rand(1:sz[i]))))
            B = A[ind...]
        end
    end
    println("\n")

    println("Slicing with non-contiguous blocks:")
    println("Small arrays:")
    for n_dims in 1:10
        sz = ntuple(n_dims,i->lensmall)
        A = randn(sz)
        ind = ntuple(n_dims,i -> ((i > n_dims/2) ? (1:sz[i]) : (rand(1:sz[i]))))
        n_el = prod(map(length,ind))
        n_r = ceil(Int,n_evals/n_el)
        print(n_dims, " dimensions (", n_r, " repeats, ", n_r*n_el, " operations): ")
        @time for i = 1:n_r
            #        ind = ntuple(n_dims,i -> ((i <= n_dims/2) ? (1:sz[i]) : (rand(1:sz[i]))))
            B = A[ind...]
        end
    end
    println("Big arrays:")
    for n_dims in 1:length(lenbig)
        sz = ntuple(n_dims,i->lenbig[n_dims])
        A = randn(sz)
        ind = ntuple(n_dims,i -> ((i > n_dims/2) ? (1:sz[i]) : (rand(1:sz[i]))))
        n_el = prod(map(length,ind))
        n_r = ceil(Int,n_evals/n_el)
        print(n_dims, " dimensions (", n_r, " repeats, ", n_r*n_el, " operations): ")
        @time for i = 1:n_r
            #        ind = ntuple(n_dims,i -> ((i <= n_dims/2) ? (1:sz[i]) : (rand(1:sz[i]))))
            B = A[ind...]
        end
    end
    println("\n")


    println("Random operations:")
    println("Small arrays:")
    function randind(len)
        i = rand(1:6)
        indchoices = {1:len,1:ceil(Int,len/2),1:ceil(Int,3*len/4),2:2:len,1:ceil(Int,len/2):len,len:-1:1}
        return indchoices[i]
    end
    #indsmall = {1:4,1:2,1:3,2:2:4,1:3:4,4:-1:1}
    for n_dims in 1:10
        sz = ntuple(n_dims,i->lensmall)
        A = randn(sz)
        ind = ntuple(n_dims,i->randind(sz[i]))
        n_el = prod(map(length,ind))
        n_r = ceil(Int,n_evals/n_el)
        print(n_dims, " dimensions (", n_r, " repeats, ", n_r*n_el, " operations): ")
        @time for i = 1:n_r
            #        ind = ntuple(n_dims,i->randind(sz[i]))
            B = A[ind...]
        end
    end
    println("Big arrays:")
    for n_dims in 1:length(lenbig)
        sz = ntuple(n_dims,i->lenbig[n_dims])
        A = randn(sz)
        ind = ntuple(n_dims,i->randind(sz[i]))
        n_el = prod(map(length,ind))
        n_r = ceil(Int,n_evals/n_el)
        print(n_dims, " dimensions (", n_r, " repeats, ", n_r*n_el, " operations): ")
        @time for i = 1:n_r
            #        ind = ntuple(n_dims,i->randind(sz[i]))
            B = A[ind...]
        end
    end
end

if run_assign
    println("\n\n\n#### Assign ####")
    println("Whole array operations:")
    println("Small arrays:")
    for n_dims in 1:10
        sz = ntuple(n_dims,i->lensmall)
        B = zeros(sz)
        A = randn(sz)
        n_r = ceil(Int,n_evals/prod(sz))
        print(n_dims, " dimensions (", n_r, " repeats): ")
        @time for i = 1:n_r
            B[:] = A
        end
    end
    println("Big arrays:")
    for n_dims in 1:length(lenbig)
        sz = ntuple(n_dims,i->lenbig[n_dims])
        B = zeros(sz)
        A = randn(sz)
        n_r = ceil(Int,n_evals/prod(sz))
        print(n_dims, " dimensions (", n_r, " repeats): ")
        @time for i = 1:n_r
            B[:] = A
        end
    end
    println("\n")

    println("Slicing with contiguous blocks:")
    println("Small arrays:")
    for n_dims in 1:10
        sz = ntuple(n_dims,i->lensmall)
        B = zeros(sz)
        ind = ntuple(n_dims,i -> ((i <= ceil(n_dims/2)) ? (1:sz[i]) : (rand(1:sz[i]))))
        A = randn(map(length,ind))
        n_el = prod(map(length,ind))
        n_r = ceil(Int,n_evals/n_el)
        print(n_dims, " dimensions (", n_r, " repeats, ", n_r*n_el, " operations): ")
        @time for i = 1:n_r
    #        ind = ntuple(n_dims,i -> ((i <= n_dims/2) ? (1:sz[i]) : (rand(1:sz[i]))))
    #        A = randn(map(length,ind))
            B[ind...] = A
        end
    end
    println("Big arrays:")
    for n_dims in 1:length(lenbig)
        sz = ntuple(n_dims,i->lenbig[n_dims])
        B = zeros(sz)
        ind = ntuple(n_dims,i -> ((i <= ceil(n_dims/2)) ? (1:sz[i]) : (rand(1:sz[i]))))
        A = randn(map(length,ind))
        n_el = prod(map(length,ind))
        n_r = ceil(Int,n_evals/n_el)
        print(n_dims, " dimensions (", n_r, " repeats, ", n_r*n_el, " operations): ")
        @time for i = 1:n_r
#            ind = ntuple(n_dims,i -> ((i <= n_dims/2) ? (1:sz[i]) : (rand(1:sz[i]))))
            #        A = randn(map(length,ind))
            B[ind...] = A
        end
    end
    println("\n")

    println("Slicing with non-contiguous blocks:")
    println("Small arrays:")
    for n_dims in 1:10
        sz = ntuple(n_dims,i->lensmall)
        B = zeros(sz)
        ind = ntuple(n_dims,i -> ((i > n_dims/2) ? (1:sz[i]) : (rand(1:sz[i]))))
        A = randn(map(length,ind))
        n_el = prod(map(length,ind))
        n_r = ceil(Int,n_evals/n_el)
        print(n_dims, " dimensions (", n_r, " repeats, ", n_r*n_el, " operations): ")
        @time for i = 1:n_r
            #        ind = ntuple(n_dims,i -> ((i <= n_dims/2) ? (1:sz[i]) : (rand(1:sz[i]))))
            #        A = randn(map(length,ind))
            B[ind...] = A
        end
    end
    println("Big arrays:")
    for n_dims in 1:length(lenbig)
        sz = ntuple(n_dims,i->lenbig[n_dims])
        B = zeros(sz)
        ind = ntuple(n_dims,i -> ((i > n_dims/2) ? (1:sz[i]) : (rand(1:sz[i]))))
        A = randn(map(length,ind))
        n_el = prod(map(length,ind))
        n_r = ceil(Int,n_evals/n_el)
        print(n_dims, " dimensions (", n_r, " repeats, ", n_r*n_el, " operations): ")
        @time for i = 1:n_r
            #        ind = ntuple(n_dims,i -> ((i <= n_dims/2) ? (1:sz[i]) : (rand(1:sz[i]))))
            #        A = randn(map(length,ind))
            B[ind...] = A
        end
    end
    println("\n")


    println("Random operations:")
    println("Small arrays:")
    function randind(len)
        i = rand(1:6)
        indchoices = {1:len,1:ceil(Int,len/2),1:ceil(Int,3*len/4),2:2:len,1:ceil(Int,len/2):len,len:-1:1}
        return indchoices[i]
    end
    #indsmall = {1:4,1:2,1:3,2:2:4,1:3:4,4:-1:1}
    for n_dims in 1:10
        sz = ntuple(n_dims,i->lensmall)
        B = zeros(sz)
        ind = ntuple(n_dims,i->randind(sz[i]))
        A = randn(map(length,ind))
        n_el = prod(map(length,ind))
        n_r = ceil(Int,n_evals/n_el)
        print(n_dims, " dimensions (", n_r, " repeats, ", n_r*n_el, " operations): ")
        @time for i = 1:n_r
            #        ind = ntuple(n_dims,i->randind(sz[i]))
            #        A = randn(map(length,ind))
            B[ind...] = A
        end
    end
    println("Big arrays:")
    for n_dims in 1:length(lenbig)
        sz = ntuple(n_dims,i->lenbig[n_dims])
        B = zeros(sz)
        ind = ntuple(n_dims,i->randind(sz[i]))
        A = randn(map(length,ind))
        n_el = prod(map(length,ind))
        n_r = ceil(Int,n_evals/n_el)
        print(n_dims, " dimensions (", n_r, " repeats, ", n_r*n_el, " operations): ")
        @time for i = 1:n_r
            #        ind = ntuple(n_dims,i->randind(sz[i]))
            #        A = randn(map(length,ind))
            B[ind...] = A
        end
    end
end
