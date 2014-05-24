import Base.Test.@test, Base.parapply

### Print test

function i_will_print(N,m)
    #u="hhh"*"kkk"
    for i=1:N
        Base.lock(m)
        println(i)
        Base.unlock(m)
    end
end

let N=100
    gc_disable()
    m = Base.Mutex()
    t1 = Base.Thread(i_will_print, N,m)
    t2 = Base.Thread(i_will_print, N,m)
    Base.run(t1)
    Base.run(t2)
    Base.join(t1)
    Base.join(t2)
    gc_enable()
end

