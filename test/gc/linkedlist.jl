# This file is a part of Julia. License is MIT: https://julialang.org/license

mutable struct ListNode
  key::Int64
  next::ListNode
  ListNode() = new()
  ListNode(x)= new(x)
  ListNode(x,y) = new(x,y);
end

function list(n=128)
    start::ListNode = ListNode(1)
    current::ListNode = start
    for i = 2:(n*1024^2)
        current = ListNode(i,current)
    end
    return current.key
end

_ = list()
GC.gc()
