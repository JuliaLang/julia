# all enclose styles work the same way

@doc "`⟪ args... ⟫` becomes `enclose_Angle( args... )`" ->
macro enclose_Angle(args...)
    esc( Expr( :call, :enclose_Angle, args... ) )
end

@doc "`{| args... |}` becomes `enclose_Brace( args... )`" ->
macro enclose_Brace(args...)
    esc( Expr( :call, :enclose_Brace, args... ) )
end

@doc "`[| args... |]` becomes `enclose_Brack( args... )`" ->
macro enclose_Brack(args...)
    esc(  Expr( :call, :enclose_Brack, args... ) )
end

@doc """
`foobar⟪ args... ⟫` becomes `call_Angle( foobar, args... )`

`foo.bar⟪ args... ⟫` becomes `call_Angle( foo.bar, args... )`

In practice, the type of foobar usually drives the dispatch.
"""->
macro call_Angle(sym, args...)
    esc( Expr( :call, :call_Angle, sym, args... ) )
end

@doc """
`foobar{| args... |}` becomes `call_Brace( :foobar, args... )`

Note that `foobar` does not need to exist at all! It is
up to the remaining argument type signature to drive dispatch.

This behavior is different from Angle and Brack brackets.
This is to accommodate the situation where we want to pass the
called function as a symbol instead of an existing value.
See `@doc @call_Angle` and `@doc @call_Brack`.

In practice, packages that employ `foobar{| args... |}` are expected to use at least one
custom type in the function signature `args...`.

Also, we can pass an Expr as the 1st argument as well:

`foo.bar{| args... |}` becomes `call_Brace( :(foo.bar), args... )`

""" ->
macro call_Brace(sym, args...)
    esc( Expr( :call, :call_Brace, Expr( :quote, sym ), args... ) )
end

@doc """
`foobar[| args...|]` becomes `call_Brack( foobar, args... )`

`foo.bar[| args...|]` becomes `call_Brack( foo.bar, args... )`

In practice, the type of foobar usually drives the dispatch.
"""->
macro call_Brack(sym, args...)
    esc( Expr( :call, :call_Brack, sym, args... ) )
end

enclose_Angle( args... ) = throw(ArgumentError( "Undefined enclose_Angle for " * string(args) ) )
enclose_Brace( args... ) = throw(ArgumentError( "Undefined enclose_Brace for " * string(args) ) )
enclose_Brack( args... ) = throw(ArgumentError( "Undefined enclose_Brack for " * string(args) ) )
call_Angle( args... ) = throw(ArgumentError( "Undefined call_Angle for " * string(args) ) )
call_Brace( args... ) = throw(ArgumentError( "Undefined call_Brace for " * string(args) ) )
call_Brack( args... ) = throw(ArgumentError( "Undefined call_Brack for " * string(args) ) )
