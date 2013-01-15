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

@deprecate  push        push!
@deprecate  pop         pop!
@deprecate  grow        grow!
@deprecate  enqueue     unshift!
@deprecate  unshift     unshift!
@deprecate  shift       shift!
@deprecate  insert      insert!
@deprecate  del         delete!
@deprecate  del_all     empty!

@deprecate  load        require
@deprecate  numel       length
@deprecate  idump       xdump
@deprecate  cwd         pwd
@deprecate  strlen      length

# aliases
@deprecate  chi2rnd     randchi2
@deprecate  betarnd     randbeta
@deprecate  exprnd      randexp
@deprecate  islogical   isbool
@deprecate  csvread     readcsv
@deprecate  dlmread     readdlm
@deprecate  csvwrite    writecsv
@deprecate  dlmwrite    writedlm
@deprecate  lc          lowercase
@deprecate  uc          uppercase
@deprecate  nCr         binomial
