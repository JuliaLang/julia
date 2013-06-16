# pseudo-definitions to show how everything behaves
#
# throw(label, val) = # throw a value to a dynamically enclosing block
#
# function rethrow(val)
#     global current_exception = val
#     throw(current_handler(), current_exception)
# end
#
# rethrow() = rethrow(current_exception)
#
# function throw(val)
#     global catch_backtrace = backtrace()
#     rethrow(val)
# end

## native julia error handling ##

error(e::Exception) = throw(e)
error{E<:Exception}(::Type{E}) = throw(E())
error(s::String) = throw(ErrorException(s))
error(s...)      = throw(ErrorException(string(s...)))

macro unexpected()
    :(error("unexpected branch reached"))
end

rethrow() = ccall(:jl_rethrow, Void, ())::None
rethrow(e) = ccall(:jl_rethrow_other, Void, (Any,), e)::None
backtrace() = ccall(:jl_backtrace_from_here, Array{Ptr{Void},1}, ())
catch_backtrace() = ccall(:jl_get_backtrace, Array{Ptr{Void},1}, ())

## system error handling ##

errno() = ccall(:jl_errno, Int32, ())
strerror(e::Integer) = bytestring(ccall(:strerror, Ptr{Uint8}, (Int32,), e))
strerror() = strerror(errno())
systemerror(p, b::Bool) = b ? throw(SystemError(string(p))) : nothing

## assertion functions and macros that raise an error on assertion violations ##

assert(x) = assert(x,'?')
assert(x,labl) = x ? nothing : error("assertion failed: ", labl)
macro assert(ex)
    :($(esc(ex)) ? nothing : error("assertion failed: ", $(string(ex))))
end

## application-oriented assertion macro that need not raise an error ##

module Assertions # PRELIMINARY VERSION by pkoppstein@gmail.com 2013-06-15

# This version is PRELIMINARY, primarily because the file and line number
# of the assertion are not printed when an assertion violation is detected.

# Usage:
#     @ASSERT <condition>
# or  @ASSERT <condition>, <expr> ....
# or  @ASSERT (<condition>, <expr> ....)  # a space is required after "@ASSERT"
#
# If assertion checking is on, then an attempt to evaluate the tuple
# (<condition>, <expr> ....) will always be made.
#
# For an assertion to be considered valid:
# <condition> should evaluate to a boolean value;
# <expr>      should be an expression that does not raise an exception.
# 
# The @ASSERT macro has been written with the goal of ensuring that:
# (a) invalid assertions will NEVER raise a run-time error;
# (b) if @ASSERT returns a value, that value with be nothing.
#
# Examples:
# julia> x=1; y=2
# julia> @ASSERT x == 1
# julia> @ASSERT (x == y, "x=$x", "y=$y")
# Assertion failed: :((x==y)) where ("x=1","y=2")

# Examples showing that invalid assertions do not raise an error, even
# if Assertions.state.error is true:
# julia> Assertions.error(true)
# julia> @ASSERT 1
# MESSAGE: INVALID ASSERTION: 1
# julia> @ASSERT 1 == 1, error("farewell") 
# MESSAGE: INVALID ASSERTION: :(1 == 1,error("farewell"))

# By default:
# - assertion checking is on
# - assertion violations do NOT raise an error
# - assertion violation messages are sent to OUTPUT_STREAM
#
# To turn assertion-checking off:            Assertions.on(false)
# To raise an error on assertion violations: Assertions.error(true)
# To direct assertions to STDERR:            Assertions.io(STDERR)

type State
  on::Bool
  io::IO
  error::Bool
end

state = State(true, OUTPUT_STREAM, false)  # the default values

function on(b::Bool) 
  state.on = b
end

function io(v::IO)
  state.io = v
end

function error(v::Bool)
  state.error = v
end

# possibly print a message, and possibly raise an error
function message(msg)
  if state.error
    if state.io != STDERR 
       println(state.io, "Assertion failed: ", msg)
    end
    error("Assertion failed: ", msg) 
  else
    println(state.io, "Assertion failed: ", msg)
  end
end

# helper method
function check(values, exp, show)
  if typeof(values[1]) == Bool
    if !values[1] ; message( "$exp where $show" ) ; end
  else
     message( "invalid assertion: $exp where $show" )
  end
end

end # Assertions

# NOTE: The @ASSERT macro is deliberately defined outside the Assertions module.
macro ASSERT(ex)
  if Assertions.state.on
    if typeof(ex) == Expr && ex.head == :tuple && length( ex.args ) > 0
      :(begin
          local values, exp, show
          try
            values = $(esc(ex) )
            exp = $(string(ex.args[1]))
            show = string( values[2:end] )
          catch
            info("INVALID ASSERTION: " *  $(string(ex)))
            values=(true,)
          end
          Assertions.check(values, exp, show)
        end
       )
    else 
      :(begin
          local value
          try
            value = $(esc(ex))
          catch
            info("INVALID ASSERTION: " * $(string(ex)))
            value = true
          end
          value ? nothing : (Assertions.message( $(string(ex)) ))
        end
       )
    end
  end
end # @ASSERT


## printing with color ##

function with_output_color(f::Function, color::Symbol, io::IO, args...)
    have_color || return f(io, args...)
    print(io, get(text_colors, color, color_normal))
    try f(io, args...)
    finally
        print(io, color_normal)
    end
end

print_with_color(color::Symbol, io::IO, msg::String...) =
    with_output_color(print, color, io, msg...)
print_with_color(color::Symbol, msg::String...) =
    print_with_color(color, OUTPUT_STREAM, msg...)

# use colors to print messages and warnings in the REPL

function info(msg::String...; depth=0)
    #stack::Range1{Int} = 3 +
    #    if isa(depth,Int)
    #        depth:depth
    #    else
    #        depth
    #    end
    with_output_color(print, :blue, STDERR, "MESSAGE: ", msg...)
    #with_output_color(show_backtrace, :blue, STDERR, backtrace(), stack)
    println(STDERR)
end
function warn(msg::String...; depth=0)
    stack::Range1{Int} = 2 +
        if isa(depth,Int)
            depth:depth
        else
            depth
        end
    with_output_color(print, :red,  STDERR, "WARNING: ", msg...)
    with_output_color(show_backtrace, :red, STDERR, backtrace(), stack)
    println(STDERR)
end
