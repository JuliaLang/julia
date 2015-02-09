# Grigoriadis Khachiyan Matrix Games.
#
# Example run: gk(10000, [0.1])
#
# Code from Dilys Thomas <dilys@cs.stanford.edu>

##### generate skew symmetric matrix  #######
function myunifskew(n)
    A = zeros(n, n)

    #print("A[i,j] initialized with zeros \n");

    for i=1:n
        for j=1:i-1
            temp=rand()
            #print(temp)
            if (temp < 0.5)
                temp = rand()
                A[i,j]= temp
                A[j,i]= -A[i,j]
                #print("welcome");
            else
                temp = rand()
                A[j,i]= temp
                A[i,j]= -A[j,i]
                #print("welcome");
            end

        end
        if rem(i,1000) == 0
            #print(i)
            #print("\n")
        end
    end

    return A
end


############ GK Algorithm starts ##################
#@profile begin
function gk(n, myeps)

    A = myunifskew(n)

    g = length(myeps)
    iteration = zeros(g)
    times = zeros(g)

    for KK=1:g

        eps = myeps[KK]

        e = ones(n)
        X = zeros(n)
        U = zeros(n)
        p = e./n

        t = 0
        #tm=U/t
        stop = 0
        iter = 0
        epse = eps .* e

        csum = zeros(n)

#        tic()
        while(stop != 1)
            t=t+1
            iter=t
            #iteration number

            if rem(iter, 100) == 0
                #print(iter)
                #print("\n")
            end

            for i=1:n
                csum[i] = sum(p[1:i])
            end

            marker=rand()
            k=1

            for i=2:n
                if csum[i-1] <= marker && marker <= csum[i]
                    k=i
                    break
                end
            end

            X[k] += 1
            for i=1:n
                U[i] += A[i,k]
            end

            s = sum(p[1:n] .* exp((eps/2)*A[1:n,k]))
            for i=1:n
                p[i]=(p[i]*exp((eps/2)*A[i,k])) / s
            end

            u = U ./ t

            True=0
            for i=1:n
                if u[i] <= epse[i]
                    True = True+1
                end
            end

            if True == n
                stop=1
            end

        end

        times[KK] = 0#toc()
        iteration[KK] = iter

        x = X/t
        etx=sum(x)

        AX=A*X
        Ax = A*x
        error=abs(AX)-abs(U)
        #print(Ax)

        Axepse=0
        for i=1:n
            if Ax[i]<=epse[i]
                Axepse = Axepse+1
            end
        end

        if Axepse==n
            #print(" \n Ax <= eps*e  \n")
        end

        #if A*x <= eps*e
        #    print(" Ax <= eps*e \n ")
        #end

        errorlmt = 0
        for i=1:n
            if error[i]<1e-8
                errorlmt = errorlmt+1
            end
        end

        if errorlmt ==n
            #print("Assertion condition is satisfied i.e. AX-U<10^-8 \n")
        else
            print("Error:  AX-U<10^-8 not satisfied \n")
        end

        #print("welcome")

        # print("Time for \n")
        # print(eps)
        # print("\n")
        # print("is  \n")
        # print(times[KK])
        # print("\n")
        # print("Number of iteration is \n")
        # print(iteration[KK])
        # print("\n")
    end

    # print("Epsilon vector is \n")
    # print(myeps)
    # print("\n")
    # print("time vector is \n ")
    # print(times)
    # print("\n")
    # print("Iteration Vector is \n")
    # print(iteration)
    # print("\n \n")

    #out = [myeps, time, iteration]
    #print("Epsilon-Time-Iteration tradeoff \n")
    #print(out)
    #print("\n \n")

end
#end # @profile begin
