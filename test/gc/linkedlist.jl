# This file is a part of Julia. License is MIT: https://julialang.org/license

mutable struct ListNode
    key::Int64
    next::ListNode
    ListNode() = new()
    ListNode(x)= new(x)
    ListNode(x,y) = new(x,y);
end

function list(N=16*1024^2)
    start::ListNode = ListNode(1)
    current::ListNode = start
    for i = 2:N
        current = ListNode(i,current)
    end
    return current.key
end

# Memory use is 512 MB
_ = list()

GC.gc()
