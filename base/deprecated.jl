macro deprecate(oldf,newf)
    oldname = expr(:quote,oldf)
    newname = expr(:quote,newf)
    expr(:toplevel,
        expr(:export,esc(oldf)),
        :(function $(esc(oldf))(args...)
              warn(strcat($oldname," is deprecated, use ",$newname," instead."))
              $(esc(newf))(args...)
          end))
end

@deprecate  push     push!
@deprecate  pop      pop!
@deprecate  grow     grow!
@deprecate  enqueue  unshift!
@deprecate  unshift  unshift!
@deprecate  shift    shift!
@deprecate  insert   insert!
@deprecate  del      delete!
@deprecate  del_all  empty!

@deprecate  numel    length
