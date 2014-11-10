type MD
  content::Vector{Any}
  meta::Dict{Any, Any}

  MD(content::AbstractVector, meta::Dict = Dict()) =
    new(content, meta)
end

MD(xs...) = MD([xs...])

# Forward some array methods

Base.push!(md::MD, x) = push!(md.content, x)
Base.getindex(md::MD, args...) = md.content[args...]
Base.setindex!(md::MD, args...) = setindex!(md.content, args...)
Base.endof(md::MD) = endof(md.content)
Base.length(md::MD) = length(md.content)
Base.isempty(md::MD) = isempty(md.content)

# Parser functions:
#   md – should be modified appropriately
#   return – basically, true if parse was successful
#     false uses the next parser in the queue, true
#     goes back to the beginning
# 
# Inner parsers:
#   return – element to use or nothing

# Inner parsing

function innerparse(stream::IO, parsers::Vector{Function})
  for parser in parsers
    inner = parser(stream)
    inner ≡ nothing || return inner
  end
end

innerparse(stream::IO, config::Config) =
  innerparse(stream, config.inner.parsers)

function parseinline(stream::IO, config::Config)
  content = []
  buffer = IOBuffer()
  while !eof(stream)
    char = peek(stream)
    if haskey(config.inner, char) &&
        (inner = innerparse(stream, config.inner[char])) != nothing
      c = takebuf_string(buffer)
      !isempty(c) && push!(content, c)
      buffer = IOBuffer()
      push!(content, inner)
    else
      write(buffer, read(stream, Char))
    end
  end
  c = takebuf_string(buffer)
  !isempty(c) && push!(content, c)
  return content
end

parseinline(s::String, c::Config) =
  parseinline(IOBuffer(s), c)

parseinline(s) = parseinline(s, _config_)

# Block parsing

function parse(stream::IO, block::MD, config::Config; breaking = false)
  skipblank(stream)
  eof(stream) && return false
  for parser in (breaking ? config.breaking : [config.breaking, config.regular])
    parser(stream, block, config) && return true
  end
  return false
end

function parse(stream::IO; flavor = julia)
  isa(flavor, Symbol) && (flavor = flavors[flavor])
  markdown = MD()
  withconfig(flavor) do
    while parse(stream, markdown, flavor) end
  end
  return markdown
end
