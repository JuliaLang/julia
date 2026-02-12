@testset "Scopes" begin

test_mod = Module()

#-------------------------------------------------------------------------------
# Scopes
@test JuliaLowering.include_string(test_mod,
"""
let
    y = 0
    x = 1
    let x = x + 1
        y = x
    end
    (x, y)
end
""") == (1, 2)

JuliaLowering.include_string(test_mod, """
x = 101
y = 202
""")
@test test_mod.x == 101
@test test_mod.y == 202
@test JuliaLowering.include_string(test_mod, "x + y") == 303

@test JuliaLowering.include_string(test_mod, """
begin
    local x = 1
    local x = 2
    let (x,y) = (:x,:y)
        (y,x)
    end
end
""") === (:y,:x)

# Types on left hand side of type decls refer to the outer scope
# (In the flisp implementation they refer to the inner scope, but this seems
# like a bug.)
@test JuliaLowering.include_string(test_mod, """
let x::Int = 10.0
    local Int = Float64
    x
end
""") === 10

# Closures in let syntax can only capture values from the outside
# (In the flisp implementation it captures from inner scope, but this is
# inconsistent with let assignment where the rhs refers to the outer scope and
# thus seems like a bug.)
@test JuliaLowering.include_string(test_mod, """
begin
    local y = :outer_y
    let f() = y
        local y = :inner_y
        f()
    end
end
""") === :outer_y

#=
| old\new: || global     | local | arg                | sparam             |
|----------++------------+-------+--------------------+--------------------|
| global   || no-op      | (*)   |                    |                    |
| local    || error (*)  | no-op |                    |                    |
| arg      || shadow(??) | error | error (not unique) |                    |
| sparam   || shadow(??) | error | error (sparam/arg) | error (not unique) |
=#
@testset "Conflicts in the same local scope" begin

    # no-op cases.  It would probably be clearer (but breaking) if these were
    # errors like the conflict cases (two of the same decl should never do
    # anything, and the user might be expecting two variables).
    @testset "global,global" begin
        s = "function (); global g; global g; 1; end"
        @test JuliaLowering.include_string(test_mod, s) isa Function
    end
    @testset "local,local" begin
        s = "function (); local l; local l; end"
        @test JuliaLowering.include_string(test_mod, s) isa Function
    end

    # locals may not overlap args/sparams/globals
    @testset "global,local/local,global" begin
        s = "function (); global g; local g; end"
        @test_throws LoweringError JuliaLowering.include_string(test_mod, s)
    end
    @testset "arg,local" begin
        s = "function (x); local x; end"
        @test_throws LoweringError JuliaLowering.include_string(test_mod, s)
    end
    @testset "sparam,local" begin
        s = "function (a::s) where {s}; local s; end"
        @test_throws LoweringError JuliaLowering.include_string(test_mod, s)
    end

    # globals may overlap args or sparams (buggy?)  TODO: decide whether it's
    # worth replicating this behaviour.  We would likely need to copy the way
    # flisp nests an extra scope block in every lambda.
    @testset "arg,global" begin
        s = "function (a); global a = 1; a; end"
        @test_broken f = JuliaLowering.include_string(test_mod, s)
        @test_broken f isa Function
        @test_broken f(999) === 1
        @test_broken isdefinedglobal(test_mod, :a)
    end
    @testset "sparam,global" begin
        s = "function (a::s) where {s}; global s = 1; s; end"
        @test_broken f = JuliaLowering.include_string(test_mod, s)
        @test_broken f isa Function
        @test_broken f(999) === 1
        @test_broken isdefinedglobal(test_mod, :s)
    end

    # sp/arg conflict
    @testset "arg,sparam" begin
        s = "function (a) where {a}; end"
        @test_throws LoweringError JuliaLowering.include_string(test_mod, s)
    end
    @testset "arg,arg" begin
        s = "function (a,a); end"
        @test_throws LoweringError JuliaLowering.include_string(test_mod, s)
    end
    @testset "sparam,sparam" begin
        s = "function () where {s,s}; end"
        @test_throws LoweringError JuliaLowering.include_string(test_mod, s)
    end

    # (not in table) destructured args are handled internally like locals, but
    # should have similar conflict rules to arguments
    @testset "destructured-arg,destructured-arg/arg/local/sp/global" begin
        s = "function ((x,x)); end"
        @test_throws LoweringError JuliaLowering.include_string(test_mod, s)
        s = "function ((x,y),x); end"
        @test_throws LoweringError JuliaLowering.include_string(test_mod, s)
        s = "function ((x,y)) where {x}; end"
        @test_throws LoweringError JuliaLowering.include_string(test_mod, s)
        s = "function ((x,y)); global x; x; end"
        @test_throws LoweringError JuliaLowering.include_string(test_mod, s)
        # quirk: flisp is OK with this
        s = "function ((x,y)); local x; end"
        @test JuliaLowering.include_string(test_mod, s) isa Function
    end

end

# Switch to Core.eval for sanity-checking
expr_eval(mod, ex) = JuliaLowering.eval(mod, ex)

enable_softscope(e...) = Expr(:block, Expr(:softscope, true), e...)
wrap_none(e...) = Expr(:block, e...)
wrap_neutral(e...) = Expr(:try, # use try so that a value is returned
                          Expr(:block, e...),
                          :catchvar, Expr(:block,
                                          Expr(:call, rethrow, :catchvar)))
wrap_func(e...) = Expr(:call,
                       Expr(:function, Expr(:tuple),
                            Expr(:block, e...)))
wrap_hard(e...) = Expr(:let, Expr(:block), Expr(:block, e...))

decls(e...) = Expr(:block,
                   :(local lname = false),
                   :(global gname = false),
                   e...)

decls_none(e...) = decls(e...)
decls_neutral(e...) = wrap_neutral(decls(), e...)
decls_hard(e...) = :(let lname = false # takes a different code path in flisp
                      global gname = false
                      $(e...)
                  end)
decls_func(e...) = :((function (argname::spname = false) where spname
                          local lname = false
                          global gname = false
                          $(e...)
                      end)(#=called=#))

lhs_names = (:lname, :gname, :argname, :spname)

# For each distinct outer scope, declaration scope, and assignment scope, and
# each kind of variable (lhs_names) in the declaration scope, set the same name
# to true from the inner scope
@testset "Behaviour of `=` in local scope (shadow or assign-existing)" begin
    expected_outer_vals = Dict{Tuple{Bool, Function, Function, Function}, Tuple}(
        (false, decls_func,    wrap_hard,    wrap_func   ) => (true,true,true),
        (false, decls_func,    wrap_hard,    wrap_hard   ) => (true,true,true),
        (false, decls_func,    wrap_hard,    wrap_neutral) => (true,true,true),
        (false, decls_func,    wrap_neutral, wrap_func   ) => (true,true,true),
        (false, decls_func,    wrap_neutral, wrap_hard   ) => (true,true,true),
        (false, decls_func,    wrap_neutral, wrap_neutral) => (true,true,true),
        (false, decls_func,    wrap_none,    wrap_func   ) => (true,true,true),
        (false, decls_func,    wrap_none,    wrap_hard   ) => (true,true,true),
        (false, decls_func,    wrap_none,    wrap_neutral) => (true,true,true),
        (false, decls_hard,    wrap_hard,    wrap_func   ) => (true,true),
        (false, decls_hard,    wrap_hard,    wrap_hard   ) => (true,true),
        (false, decls_hard,    wrap_hard,    wrap_neutral) => (true,true),
        (false, decls_hard,    wrap_neutral, wrap_func   ) => (true,true),
        (false, decls_hard,    wrap_neutral, wrap_hard   ) => (true,true),
        (false, decls_hard,    wrap_neutral, wrap_neutral) => (true,true),
        (false, decls_hard,    wrap_none,    wrap_func   ) => (true,true),
        (false, decls_hard,    wrap_none,    wrap_hard   ) => (true,true),
        (false, decls_hard,    wrap_none,    wrap_neutral) => (true,true),
        (false, decls_neutral, wrap_hard,    wrap_func   ) => (true,true),
        (false, decls_neutral, wrap_hard,    wrap_hard   ) => (true,true),
        (false, decls_neutral, wrap_hard,    wrap_neutral) => (true,true),
        (false, decls_neutral, wrap_neutral, wrap_func   ) => (true,true),
        (false, decls_neutral, wrap_neutral, wrap_hard   ) => (true,true),
        (false, decls_neutral, wrap_neutral, wrap_neutral) => (true,true),
        (false, decls_neutral, wrap_none,    wrap_func   ) => (true,true),
        (false, decls_neutral, wrap_none,    wrap_hard   ) => (true,true),
        (false, decls_neutral, wrap_none,    wrap_neutral) => (true,true),
        (false, decls_none,    wrap_hard,    wrap_func   ) => (true,false),
        (false, decls_none,    wrap_hard,    wrap_hard   ) => (true,false),
        (false, decls_none,    wrap_hard,    wrap_neutral) => (true,false),
        (false, decls_none,    wrap_neutral, wrap_func   ) => (true,false),
        (false, decls_none,    wrap_neutral, wrap_hard   ) => (true,false),
        (false, decls_none,    wrap_neutral, wrap_neutral) => (true,false),
        (false, decls_none,    wrap_none,    wrap_func   ) => (true,false),
        (false, decls_none,    wrap_none,    wrap_hard   ) => (true,false),
        (false, decls_none,    wrap_none,    wrap_neutral) => (true,false),
        (true,  decls_func,    wrap_hard,    wrap_func   ) => (true,true,true),
        (true,  decls_func,    wrap_hard,    wrap_hard   ) => (true,true,true),
        (true,  decls_func,    wrap_hard,    wrap_neutral) => (true,true,true),
        (true,  decls_func,    wrap_neutral, wrap_func   ) => (true,true,true),
        (true,  decls_func,    wrap_neutral, wrap_hard   ) => (true,true,true),
        (true,  decls_func,    wrap_neutral, wrap_neutral) => (true,true,true),
        (true,  decls_func,    wrap_none,    wrap_func   ) => (true,true,true),
        (true,  decls_func,    wrap_none,    wrap_hard   ) => (true,true,true),
        (true,  decls_func,    wrap_none,    wrap_neutral) => (true,true,true),
        (true,  decls_hard,    wrap_hard,    wrap_func   ) => (true,true),
        (true,  decls_hard,    wrap_hard,    wrap_hard   ) => (true,true),
        (true,  decls_hard,    wrap_hard,    wrap_neutral) => (true,true),
        (true,  decls_hard,    wrap_neutral, wrap_func   ) => (true,true),
        (true,  decls_hard,    wrap_neutral, wrap_hard   ) => (true,true),
        (true,  decls_hard,    wrap_neutral, wrap_neutral) => (true,true),
        (true,  decls_hard,    wrap_none,    wrap_func   ) => (true,true),
        (true,  decls_hard,    wrap_none,    wrap_hard   ) => (true,true),
        (true,  decls_hard,    wrap_none,    wrap_neutral) => (true,true),
        (true,  decls_neutral, wrap_hard,    wrap_func   ) => (true,true),
        (true,  decls_neutral, wrap_hard,    wrap_hard   ) => (true,true),
        (true,  decls_neutral, wrap_hard,    wrap_neutral) => (true,true),
        (true,  decls_neutral, wrap_neutral, wrap_func   ) => (true,true),
        (true,  decls_neutral, wrap_neutral, wrap_hard   ) => (true,true),
        (true,  decls_neutral, wrap_neutral, wrap_neutral) => (true,true),
        (true,  decls_neutral, wrap_none,    wrap_func   ) => (true,true),
        (true,  decls_neutral, wrap_none,    wrap_hard   ) => (true,true),
        (true,  decls_neutral, wrap_none,    wrap_neutral) => (true,true),
        (true,  decls_none,    wrap_hard,    wrap_func   ) => (true,false),
        (true,  decls_none,    wrap_hard,    wrap_hard   ) => (true,false),
        (true,  decls_none,    wrap_hard,    wrap_neutral) => (true,false),
        (true,  decls_none,    wrap_neutral, wrap_func   ) => (true,false),
        (true,  decls_none,    wrap_neutral, wrap_hard   ) => (true,false),
        (true,  decls_none,    wrap_neutral, wrap_neutral) => (true,true),
        (true,  decls_none,    wrap_none,    wrap_func   ) => (true,false),
        (true,  decls_none,    wrap_none,    wrap_hard   ) => (true,false),
        (true,  decls_none,    wrap_none,    wrap_neutral) => (true,true),
    )
    expected_s(b::Bool) = b ? "assignment to outer var" : "brand-new var"

    tmp_test_mod = Module()
    tmp_test_mod_2 = Module()

    for ((soft_mode, decls_s, middle_s, assign_s), results) in expected_outer_vals,
            (lhs_i, lhs) in enumerate(lhs_names)

        ex = decls_s(middle_s(assign_s(:($lhs = true))), lhs)
        soft_mode && (ex = enable_softscope(ex))

        if lhs in (:argname, :spname) && decls_s !== decls_func
            continue
        elseif lhs === :spname
            @test_throws LoweringError expr_eval(tmp_test_mod, ex)
        else
            expected = results[lhs_i]
            reference_ok = reference_eval(tmp_test_mod_2, ex) === expected
            !reference_ok && @error("flisp produced unexpected result; fix that or JL scope tests:\n",
                                   "expected $(expected_s(expected)), got $(expected_s(!expected))\n", ex)
            ok = expr_eval(tmp_test_mod, ex) === expected
            !ok && @error("expected $(expected_s(expected)), got $(expected_s(!expected))\n", ex)
            @test ok
        end

        if lhs === :gname
            Base.delete_binding(tmp_test_mod, :gname)
            Base.delete_binding(tmp_test_mod_2, :gname)
        end
    end
end

@testset "global declarations at top level are ignored in assignment resolution" begin
    suggest_global(e) = :(begin; global declared_unassigned_global; $e; end)
    for soft_mode in (true, false), scope in (wrap_func, wrap_hard, wrap_neutral)
        ex = scope(:(declared_unassigned_global = true))
        soft_mode && (ex = enable_softscope(ex))
        expr_eval(test_mod, ex)
        global_assigned = @invokelatest isdefined(test_mod, :declared_unassigned_global)
        global_assigned && error("global should not be assigned. settings: $soft_mode $scope\n")
        @test !global_assigned
    end

    @testset "soft scope isn't top level" begin
        ex = quote
            begin
                for i in 1:1; global soft_assigned_explicit_global = 1; end
                for i in 1:1; soft_assigned_explicit_global = 2; end
            end
        end
        expr_eval(test_mod, enable_softscope(ex))
        @test test_mod.soft_assigned_explicit_global === 1
    end
end

# Distinct from the stateful "existing global" check (probably to get around the
# case where the global only becomes existing within the expression being
# lowered)
@testset "assignments at top level can influence assignment resolution in soft scopes" begin
    for soft_mode in (true, false),
        s1 in (wrap_neutral, (e)->wrap_neutral(wrap_neutral(e))),
        g_assign in (:(assigned_global = false), :(global assigned_global = false))

        inner_assign_islocal = s1(Expr(
            :block,
            :(assigned_global = true),
            Expr(:(=), :out, Expr(:islocal, :assigned_global))))

        for ex in (Expr(:block, :(local out), inner_assign_islocal, g_assign, :out),
                   Expr(:block, :(local out), g_assign, inner_assign_islocal, :out))

            if soft_mode
                ex = enable_softscope(ex)
                ok = expr_eval(test_mod, ex) === false
                !ok && error("expected assignment to global\n", ex)
                @test ok
            else
                # some of these produce warning in flisp
                ok = expr_eval(test_mod, ex) === true
                !ok && error("expected assignment to local\n", ex)
                @test ok
            end
            Base.delete_binding(test_mod, :assigned_global)
        end
    end
end

# Note: Certain flisp (un)hygiene behaviour is yet to be implemented.
# In flisp, with no escaping:
# - Top-level functions are unhygienic and declared in the macro's module
# - Top level globals are unhygienic and declared in the calling module
#   - this includes abstract, primitive, and struct types
# - Top-level `x=y` implicitly declares hygienic globals (but it is not breaking
#   to make them local)
#
# See https://github.com/JuliaLang/julia/issues/53667 for more quirks
@testset "unescaped macro expansions introduce a hygienic scope" begin
    @eval test_mod module macro_mod
        macro m(x); x; end
        macro mesc(x); esc(x); end
    end

    JuliaLowering.include_string(test_mod, "macro_mod.@m function f_local_1(); 1; end")
    @test !isdefined(test_mod.macro_mod, :f_local_1)
    JuliaLowering.include_string(test_mod, "macro_mod.@mesc function f_nonlocal_2(); 1; end")
    @test isdefined(test_mod, :f_nonlocal_2)
    # An unescaped const is local to a macro expansion
    @test_throws LoweringError JuliaLowering.include_string(test_mod, "macro_mod.@m const c_local_1 = 1")
    # The const may be escaped into test_mod
    JuliaLowering.include_string(test_mod, "macro_mod.@mesc const c_nonlocal_2 = 1")
    @test isdefined(test_mod, :c_nonlocal_2)
    JuliaLowering.include_string(test_mod, "macro_mod.@mesc const c_nonlocal_3 = 1"; expr_compat_mode=true)
    @test isdefined(test_mod, :c_nonlocal_3)
end

end
