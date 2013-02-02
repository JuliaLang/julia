const depwarned = (ByteString=>Bool)[]

function depwarn(msg::String...)
    msg = bytestring(msg...)
    has(depwarned,msg) && return
    depwarned[msg] = true
    warn(msg)
end

macro deprecate(oldf,newf)
    oldname = expr(:quote,oldf)
    newname = expr(:quote,newf)
    expr(:toplevel,
        expr(:export,esc(oldf)),
        :(function $(esc(oldf))(args...)
              depwarn(string($oldname," is deprecated, use ",$newname," instead."))
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
@deprecate  add           add!
@deprecate  add_each      add_each!
@deprecate  del_each      del_each!
@deprecate  toggle        symdiff!
@deprecate  toggle_each   symdiff!
@deprecate  xor!          symdiff!
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
@deprecate  order         sortperm
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

#@deprecate  strcat        string
export strcat
strcat(xs...) = string(xs...)

export randi
function randi(n,x...)
    depwarn("randi(n,...) is deprecated, use rand(1:n,...) instead.")
    rand(1:n,x...)
end

export randival
function randival(lo,hi,x...)
    depwarn("randival(lo,hi,...) is deprecated, use rand(lo:hi,...) instead.")
    rand(lo:hi,x...)
end

# discontinued functions

export randexp, randg, randbeta, randchi

randexp(x...) = error("randexp is no longer supported; use the Distributions package instead:
    using Distributions
    rand(Exponential())")

randg(x...) = error("randg is no longer supported; use the Distributions package instead:
    using Distributions
    rand(Gamma())")

randbeta(x...) = error("randbeta is no longer supported; use the Distributions package instead:
    using Distributions
    rand(Beta)")

randchi2(x...) = error("randchi2 is no longer supported; use the Distributions package instead:
    using Distributions
    rand(Chisq())")
