macro deprecate(oldf,newf)
    oldname = expr(:quote,oldf)
    newname = expr(:quote,newf)
    expr(:toplevel,
        expr(:export,esc(oldf)),
        :(function $(esc(oldf))(args...)
              warn_once(string($oldname," is deprecated, use ",$newname," instead."))
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
@deprecate  strchr        search
@deprecate  memchr        search
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
@deprecate  chars         collect
@deprecate  elements      collect
@deprecate  pairs         collect
@deprecate  strcat        string
@deprecate  iswalnum      isalnum
@deprecate  iswalpha      isalpha
@deprecate  iswascii      isascii
@deprecate  iswblank      isblank
@deprecate  iswcntrl      iscntrl
@deprecate  iswdigit      isdigit
@deprecate  iswgraph      isgraph
@deprecate  iswlower      islower
@deprecate  iswprint      isprint
@deprecate  iswpunct      ispunct
@deprecate  iswspace      isspace
@deprecate  iswupper      isupper
@deprecate  iswxdigit     isxdigit
@deprecate  copy_to       copy!
@deprecate  countp        count
@deprecate  anyp          any
@deprecate  allp          all
@deprecate  resize        sizehint
@deprecate  permute       permutedims
@deprecate  ipermute      ipermutedims
@deprecate  is_hex_digit  isxdigit
@deprecate  read_from     readsfrom
@deprecate  write_to      writesto
@deprecate  download_file download
@deprecate  histc         hist
@deprecate  map_to        map!
@deprecate  rotl          rol
@deprecate  rotr          ror
@deprecate  flipbits      (~)
@deprecate  cor_pearson   cor
@deprecate  cov_pearson   cov
@deprecate  areduce       reducedim
@deprecate  tmpnam        tempname
@deprecate  lud           lufact
@deprecate  chold         cholfact
@deprecate  cholpd        cholpfact
@deprecate  qrd           qrfact
@deprecate  qrpd          qrpfact
@deprecate  key           getkey

export grow!
function grow!(a, d)
    warn_once("grow!(array,delta) is deprecated, use resize!(array,newsize) instead.")
    resize!(a, length(a)+d)
end

export keytype
function keytype(a::Associative)
    warn_once("keytype(assoc) is deprecated, use eltype(assoc)[1] instead.")
    eltype(a)[1]
end

export valtype
function valtype(a::Associative)
    warn_once("valtype(assoc) is deprecated, use eltype(assoc)[2] instead.")
    eltype(a)[2]
end

export randi
function randi(n,x...)
    warn_once("randi(n,...) is deprecated, use rand(1:n,...) instead.")
    rand(1:n,x...)
end

export randival
function randival(lo,hi,x...)
    warn_once("randival(lo,hi,...) is deprecated, use rand(lo:hi,...) instead.")
    rand(lo:hi,x...)
end

function squeeze(A::AbstractArray)
    warn_once("squeeze(A) is deprecated, use squeeze(A, dims) specifying the dimensions to remove.")
    squeeze(A, find([size(A)...].==1))
end

export getenv, hasenv, setenv, unsetenv
function getenv(var::String)
    warn_once("getenv(var) is deprecated, use ENV[var] instead.")
    ENV[var]
end
function hasenv(var::String)
    warn_once("hasenv(var) is deprecated, use has(ENV,var) instead.")
    has(ENV, var)
end
function setenv(var::String, val::String)
    warn_once("setenv(var,val) is deprecated, use ENV[var]=val instead.")
    ENV[var] = val
end
function unsetenv(var::String)
    warn_once("unsetenv(var) is deprecated, use delete!(ENV,var) instead.")
    delete!(ENV, var)
end

# discontinued functions

export randexp, randg, randbeta, randchi
for (fun,typ) in {(:randexp,:Exponential), (:randg,:Gamma), (:randbeta,:Beta), (:randchi,:Chisq)}
@eval $fun(x...) = error("randexp is no longer supported, use the Distributions package instead:

    using Distributions
    rand(",$(expr(:quote,typ)),"())
")
end
