macro eval(x)
    quote eval($expr(:quote,x)) end
end
