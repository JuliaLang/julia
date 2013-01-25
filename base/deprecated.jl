macro deprecate(oldf,newf)
    oldname = expr(:quote,oldf)
    newname = expr(:quote,newf)
    expr(:toplevel,
        expr(:export,esc(oldf)),
        :(function $(esc(oldf))(args...)
              warn(string($oldname," is deprecated, use ",$newname," instead."))
              $(esc(newf))(args...)
          end))
end

@deprecate  push          push!
@deprecate  pop           pop!
@deprecate  grow          grow!
@deprecate  enqueue       unshift!
@deprecate  unshift       unshift!
@deprecate  shift         shift!
@deprecate  insert        insert!
@deprecate  del           delete!
@deprecate  del_all       empty!
@deprecate  load          require
@deprecate  idump         xdump
@deprecate  cwd           pwd
@deprecate  strlen        length
@deprecate  lc            lowercase
@deprecate  uc            uppercase
@deprecate  nCr           binomial
@deprecate  julia_pkgdir  Pkg.dir
@deprecate  tintersect    typeintersect
@deprecate  searchsorted  searchsortedfirst
@deprecate  choose        first
@deprecate  system        run
#@deprecate  strcat        string

export strcat
strcat(xs...) = string(xs...)

# compatibility aliases
@deprecate  numel         length
@deprecate  islogical     isbool
@deprecate  csvread       readcsv
@deprecate  dlmread       readdlm
@deprecate  csvwrite      writecsv
@deprecate  dlmwrite      writedlm
@deprecate  chi2rnd       randchi2
@deprecate  betarnd       randbeta
@deprecate  exprnd        randexp
@deprecate  rot90         rotl90

export randi, randival, randexp, randg, randbeta, randchi2

randi(x...) = error("randi is deprecated. Instead use: rand(r::Range)")

randival(x...) = error("randival is deprecated. Instead use: rand(r::Range)")

randexp(x...) = error("randexp is deprecated. Instead use the Distributions package:
    using Distributions; rand(Exponential())")

randg(x...) = error("randg is deprecated. Instead use the Distributions package:
    using Distributions; rand(Gamma())")

randbeta(x...) = error("randbeta is deprecated. Instead use the Distributions package:
    using Distributions; rand(Beta)")

randchi2(x...) = error("randchi2 is deprecated. Instead use the Distributions package:
    using Distributions; rand(Chisq())")
