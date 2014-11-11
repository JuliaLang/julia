function Base.parse(stream::IOBuffer; greedy::Bool = true, raise::Bool = true)
  pos = position(stream)
  ex, Δ = Base.parse(readall(stream), 1, greedy = greedy, raise = raise)
  seek(stream, pos + Δ - 1)
  return ex
end

function interpinner(stream::IO, greedy = false)
  startswith(stream, '$') || return
  (eof(stream) || peek(stream) in whitespace) && return
  try
    return Base.parse(stream::IOBuffer, greedy = greedy)
  catch e
    return
  end
end

@trigger '$' ->
function interp(stream::IO)
  withstream(stream) do
    ex = interpinner(stream)
    return ex
  end
end

function blockinterp(stream::IO, md::MD, config::Config)
  withstream(stream) do
    ex = interpinner(stream)
    if ex ≡ nothing
      return false
    else
      push!(md, ex)
      return true
    end
  end
end

toexpr(x) = x

toexpr(xs::Vector{Any}) = Expr(:cell1d, map(toexpr, xs)...)

function deftoexpr(T)
  @eval function toexpr(md::$T)
    Expr(:call, $T, $(map(x->:(toexpr(md.$x)), names(T))...))
  end
end

map(deftoexpr, [MD, Paragraph, Header,
                Link, Bold, Italic])
