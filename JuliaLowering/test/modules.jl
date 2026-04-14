test_mod = Module()

A = JuliaLowering.include_string(test_mod, """
module A
    function g()
        return "hi"
    end
end
""", "module_test")
@test A isa Module
@test A.g() == "hi"
@test A.include isa Base.IncludeInto
@test A.eval isa Core.EvalInto
@test A.Base === Base
@test A.eval(:(x = -2)) == -2
@test A.x == -2

B = JuliaLowering.include_string(test_mod, """
baremodule B
end
""", "baremodule_test")
@test B.Core === Core
@test !isdefined(B, :include)
@test !isdefined(B, :eval)
@test !isdefined(B, :Base)

# Module init order
Amod = JuliaLowering.include_string(test_mod, """
module A
    init_order = []
    __init__() = push!(init_order, "A")
    module B
        using ..A
        __init__() = push!(A.init_order, "B")
    end
    module C
        using ..A
        __init__() = push!(A.init_order, "C")
        module D
            using ...A
            __init__() = push!(A.init_order, "D")
        end
        module E
            using ...A
            __init__() = push!(A.init_order, "E")
        end
    end
end
""")
@test Amod.init_order == ["B", "D", "E", "C", "A"]

# Anonymous naming conventions
# These tests verify that JuliaLowering generates the same anonymous function
# and closure type names as flisp (julia-syntax.scm), with identical counter
# values. The module counter starts at 1 for fresh modules.
const _skip = Symbol("#_internal_julia_parse")
filter_names(m) = sort([s for s in names(m; all=true)
                        if startswith(string(s), "#") && s != _skip])

# Arrow function: local name #1, type #2#3
M = JuliaLowering.include_string(test_mod, """
module _AnonArrow; f = x -> x + 1; end
""")
@test filter_names(M) == [Symbol("#2#3")]

# Anonymous function(x) ... end: same pattern as arrow
M = JuliaLowering.include_string(test_mod, """
module _AnonFunc; f = function(x) x + 1 end; end
""")
@test filter_names(M) == [Symbol("#2#3")]

# Two arrows: each statement lowered independently, 3 counter values each
M = JuliaLowering.include_string(test_mod, """
module _TwoArrows; f = x -> x; g = y -> y; end
""")
@test filter_names(M) == [Symbol("#2#3"), Symbol("#5#6")]

# Named closure: type name is #<closure_name>#<outermost>##N
M = JuliaLowering.include_string(test_mod, """
module _NamedClosure
    function outer()
        x = 1
        inner() = x + 1
        inner()
    end
end
""")
@test filter_names(M) == [Symbol("#inner#outer##0"), Symbol("#outer")]

# Nested closures: all share the outermost function's reservation space
M = JuliaLowering.include_string(test_mod, """
module _NestedClosures
    function a()
        x = 1
        b() = begin
            y = 2
            c() = x + y
            c()
        end
        b()
    end
end
""")
@test filter_names(M) == [Symbol("#a"), Symbol("#b#a##0"), Symbol("#c#a##1")]

# Keyword function: body name is #name#<counter>
M = JuliaLowering.include_string(test_mod, """
module _KWFunc; function foo(x; y=1) x + y end; end
""")
@test filter_names(M) == [Symbol("##foo#1"), Symbol("#foo"), Symbol("#foo#1")]

# Anonymous closure inside named function
M = JuliaLowering.include_string(test_mod, """
module _AnonClosure
    function outer()
        x = 1
        f = y -> x + y
        f
    end
end
""")
@test filter_names(M) == [Symbol("#outer"), Symbol("#outer##0#outer##1")]

# Verify that generated names are compatible with jl_demangle_typename.
# The runtime uses this to produce display names for closures and anonymous
# function types (e.g. in stack traces, show methods).
function demangle(s::Symbol)
    ccall(:jl_demangle_typename, Any, (Any,), s)::Symbol
end

# Named closure: #inner#outer##0 → inner
M = JuliaLowering.include_string(test_mod, """
module _Demangle1
    function outer()
        x = 1
        inner() = x + 1
        inner()
    end
end
""")
@test demangle(Symbol("#inner#outer##0")) == :inner

# Anonymous arrow at toplevel: #N#M → #N
M = JuliaLowering.include_string(test_mod, """
module _Demangle2; f = x -> x + 1; end
""")
closure_type_sym = only(filter(s -> s != Symbol("#_internal_julia_parse"), filter_names(M)))
@test startswith(string(demangle(closure_type_sym)), "#")

# Keyword body: #foo#N → foo
M = JuliaLowering.include_string(test_mod, """
module _Demangle3; function foo(x; y=1) x + y end; end
""")
@test demangle(Symbol("#foo#1")) == :foo

# Nested closure: #b#a##0 → b
M = JuliaLowering.include_string(test_mod, """
module _Demangle4
    function a()
        x = 1
        b() = x + 1
        b()
    end
end
""")
@test demangle(Symbol("#b#a##0")) == :b

# Anonymous closure in named function: #outer##N#outer##M → #outer##N
# (canonicalized anonymous function name, preserved with leading #)
M = JuliaLowering.include_string(test_mod, """
module _Demangle5
    function outer()
        x = 1
        f = y -> x + y
        f
    end
end
""")
anon_type = only(filter(s -> occursin("##", string(s)), filter_names(M)))
demangled = demangle(anon_type)
# The demangled name for a canonicalized anonymous function keeps the leading #
@test startswith(string(demangled), "#")
