macro deprecate(old,new)
    if isa(old,Symbol)
        oldname = expr(:quote,old)
        newname = expr(:quote,new)
        expr(:toplevel,
            expr(:export,esc(old)),
            :(function $(esc(old))(args...)
                  warn_once(string($oldname," is deprecated, use ",$newname," instead."))
                  $(esc(new))(args...)
              end))
    elseif isa(old,Expr) && old.head == :call
        oldcall = sprint(io->show_unquoted(io,old))
        newcall = sprint(io->show_unquoted(io,new))
        expr(:toplevel,
            expr(:export,esc(old.args[1])),
            :($(esc(old)) = begin
                  warn_once(string($oldcall," is deprecated, use ",$newcall," instead."))
                  $(esc(new))
              end))
    else
        error("invalid usage of @deprecate")
    end
end

@deprecate  push            push!
@deprecate  pop             pop!
@deprecate  grow            grow!
@deprecate  enqueue         unshift!
@deprecate  unshift         unshift!
@deprecate  shift           shift!
@deprecate  insert          insert!
@deprecate  del             delete!
@deprecate  del_all         empty!
@deprecate  add             add!
@deprecate  add_each        add_each!
@deprecate  del_each        del_each!
@deprecate  toggle          symdiff!
@deprecate  toggle_each     symdiff!
@deprecate  xor!            symdiff!
@deprecate  load            require
@deprecate  idump           xdump
@deprecate  cwd             pwd
@deprecate  strlen          length
@deprecate  strchr          search
@deprecate  memchr          search
@deprecate  lc              lowercase
@deprecate  uc              uppercase
@deprecate  nCr             binomial
@deprecate  julia_pkgdir    Pkg.dir
@deprecate  tintersect      typeintersect
@deprecate  searchsorted    searchsortedfirst
@deprecate  choose          first
@deprecate  system          run
@deprecate  order           sortperm
@deprecate  numel           length
@deprecate  islogical       isbool
@deprecate  csvread         readcsv
@deprecate  dlmread         readdlm
@deprecate  csvwrite        writecsv
@deprecate  dlmwrite        writedlm
@deprecate  chi2rnd         randchi2
@deprecate  betarnd         randbeta
@deprecate  exprnd          randexp
@deprecate  rot90           rotl90
@deprecate  chars           collect
@deprecate  elements        collect
@deprecate  pairs           collect
@deprecate  strcat          string
@deprecate  iswalnum        isalnum
@deprecate  iswalpha        isalpha
@deprecate  iswascii        isascii
@deprecate  iswblank        isblank
@deprecate  iswcntrl        iscntrl
@deprecate  iswdigit        isdigit
@deprecate  iswgraph        isgraph
@deprecate  iswlower        islower
@deprecate  iswprint        isprint
@deprecate  iswpunct        ispunct
@deprecate  iswspace        isspace
@deprecate  iswupper        isupper
@deprecate  iswxdigit       isxdigit
@deprecate  copy_to         copy!
@deprecate  countp          count
@deprecate  anyp            any
@deprecate  allp            all
@deprecate  resize          sizehint
@deprecate  permute         permutedims
@deprecate  ipermute        ipermutedims
@deprecate  is_hex_digit    isxdigit
@deprecate  read_from       readsfrom
@deprecate  write_to        writesto
@deprecate  download_file   download
@deprecate  histc           hist
@deprecate  map_to          map!
@deprecate  rotl            rol
@deprecate  rotr            ror
@deprecate  flipbits        (~)
@deprecate  cor_pearson     cor
@deprecate  cov_pearson     cov
@deprecate  areduce         reducedim
@deprecate  tmpnam          tempname
@deprecate  lud             lufact
@deprecate  chold           cholfact
@deprecate  cholpd          cholpfact
@deprecate  qrd             qrfact
@deprecate  qrpd            qrpfact
@deprecate  key             getkey

@deprecate  grow!(a,d)              resize!(a,length(a)+d)
@deprecate  keytype(a)              eltype(a)[1]
@deprecate  valtype(a)              eltype(a)[2]
@deprecate  randi(n,x...)           rand(1:n,x...)
@deprecate  randival(lo,hi,x...)    rand(lo:hi,x...)
@deprecate  squeeze(A)              squeeze(A,find([size(A)...].==1))
@deprecate  getenv(var)             ENV[var]
@deprecate  hasenv(var)             has(ENV,var)
@deprecate  setenv(var,val)         ENV[var] = val
@deprecate  unsetenv(var)           delete!(ENV,var)

function svd(a::StridedMatrix, vecs::Bool, thin::Bool)
    warn_once("The second argument ``vecs`` is no longer supported. Use svd(a, thin) instead.")
    svd(a, thin)
end

function svdt(a::StridedMatrix, vecs::Bool, thin::Bool)
    warn_once("The second argument ``vecs`` is no longer supported. Use svdt(a, thin) instead.")
    svdt(a, thin)
end

# discontinued functions

export randexp, randg, randbeta, randchi
for (fun,typ) in {(:randexp,:Exponential), (:randg,:Gamma), (:randbeta,:Beta), (:randchi,:Chisq)}
@eval $fun(x...) = error($fun," is no longer supported, use the Distributions package instead:

    using Distributions
    rand(",$(expr(:quote,typ)),"())
")
end
