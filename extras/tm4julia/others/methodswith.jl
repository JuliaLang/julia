# by Tom Short (2012-Sep)
# available at git://gist.github.com/3645547.git
#
# use:
# to see all methods that have a parameter of type Dict
#    methodswith(Dict)
# to see all higher order methods
#    methodswith(Function)
#
# my edits (to allow it to work easily):
#   replace (io, .. with (io::IO
#   replace  ::Stream with ::IO
#
function methodswith(io::IO, t::Type, m::Module)
    for nm in names(m)
        try
           mt = eval(nm)
           d = mt.env.defs
           while !is(d,())
               if any(map(x -> x == t, d.sig))
                   print(io, nm)
                   show(io, d)
                   println(io)
               end
               d = d.next
           end
        end
    end
end


methodswith(t::Type, m::Module) = methodswith(OUTPUT_STREAM::IO, t::Type, m::Module)
methodswith(t::Type) = methodswith(OUTPUT_STREAM::IO, t::Type, Base)

