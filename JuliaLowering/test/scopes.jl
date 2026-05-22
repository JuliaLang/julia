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
        local f
        s = "function (a); global a = 1; a; end"
        @test_broken f = JuliaLowering.include_string(test_mod, s)
        @test_broken f isa Function
        @test_broken f(999) === 1
        @test_broken isdefinedglobal(test_mod, :a)
    end
    @testset "sparam,global" begin
        local f
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
        # this works in flisp; should it?
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
                   :(name = false),
                   :(local lname = false),
                   :(global gname = false),
                   e...)

decls_none(e...) = decls(e...)
decls_neutral(e...) = wrap_neutral(decls(), e...)
decls_hard(e...) = :(let lname = false # takes a different code path in flisp
                         name = false
                         global gname = false
                         $(e...)
                     end)
decls_func(e...) = :((function (argname::spname = false) where spname
                          name = false
                          local lname = false
                          global gname = false
                          $(e...)
                      end)(#=called=#))

lhs_names = (:name, :lname, :gname, :argname, :spname)

#=
simple test that
```
distraction_scope_begin
    local_scope_begin
        lhs = "resolve me"
        lhs *= '!'
        lhs
    local_scope_end
distraction_scope_end === "resolve me!"
```
=#
@testset "explicit locals and globals in local scope shadowing outer vars" begin
    local jl_mod = Module()
    local fl_mod = Module()

    @testset for soft_mode in (false, true),
        decls_s in (decls_func, decls_hard, decls_neutral, decls_none),
        local_s in (wrap_func, wrap_hard, wrap_neutral),
        lhs in lhs_names,
        assign_ex in (:(local $lhs = "resolve me"; $lhs *= '!'; $lhs),
                      :(global $lhs = "resolve me"; $lhs *= '!'; $lhs))

        ex = decls_s(local_s(assign_ex))
        soft_mode && (ex = enable_softscope(ex))

        if lhs == :spname && decls_s == decls_func && assign_ex.args[1].head === :local
            # flisp specifically disallows locals shadowing sparams; why?
            @test_broken fl_eval(fl_mod, ex) === "resolve me!"
        elseif lhs in (:spname, :argname) && decls_s != decls_func
            continue
        else
            reference_ok = fl_eval(fl_mod, ex) === "resolve me!"
            !reference_ok &&
                @error("shadow test failed: flisp produced unexpected result; fix that or JL scope tests:\n", ex)
            @test reference_ok
        end

        ok = expr_eval(jl_mod, ex) === "resolve me!"
        !ok && @error("shadow test failed:\n", ex)
        @test ok
    end

end

# For each distinct outer scope, declaration scope, and assignment scope, and
# each kind of variable (lhs_names) in the declaration scope, set the same name
# to true from the inner scope
@testset "Behaviour of `=` in local scope (shadow or assign-existing)" begin
    expected_outer_vals = Dict{Tuple{Bool, Function, Function, Function}, Tuple}(
        (false, decls_func,    wrap_hard,    wrap_func   ) => (true,true,true,true),
        (false, decls_func,    wrap_hard,    wrap_hard   ) => (true,true,true,true),
        (false, decls_func,    wrap_hard,    wrap_neutral) => (true,true,true,true),
        (false, decls_func,    wrap_neutral, wrap_func   ) => (true,true,true,true),
        (false, decls_func,    wrap_neutral, wrap_hard   ) => (true,true,true,true),
        (false, decls_func,    wrap_neutral, wrap_neutral) => (true,true,true,true),
        (false, decls_func,    wrap_none,    wrap_func   ) => (true,true,true,true),
        (false, decls_func,    wrap_none,    wrap_hard   ) => (true,true,true,true),
        (false, decls_func,    wrap_none,    wrap_neutral) => (true,true,true,true),
        (false, decls_hard,    wrap_hard,    wrap_func   ) => (true,true,true),
        (false, decls_hard,    wrap_hard,    wrap_hard   ) => (true,true,true),
        (false, decls_hard,    wrap_hard,    wrap_neutral) => (true,true,true),
        (false, decls_hard,    wrap_neutral, wrap_func   ) => (true,true,true),
        (false, decls_hard,    wrap_neutral, wrap_hard   ) => (true,true,true),
        (false, decls_hard,    wrap_neutral, wrap_neutral) => (true,true,true),
        (false, decls_hard,    wrap_none,    wrap_func   ) => (true,true,true),
        (false, decls_hard,    wrap_none,    wrap_hard   ) => (true,true,true),
        (false, decls_hard,    wrap_none,    wrap_neutral) => (true,true,true),
        (false, decls_neutral, wrap_hard,    wrap_func   ) => (true,true,true),
        (false, decls_neutral, wrap_hard,    wrap_hard   ) => (true,true,true),
        (false, decls_neutral, wrap_hard,    wrap_neutral) => (true,true,true),
        (false, decls_neutral, wrap_neutral, wrap_func   ) => (true,true,true),
        (false, decls_neutral, wrap_neutral, wrap_hard   ) => (true,true,true),
        (false, decls_neutral, wrap_neutral, wrap_neutral) => (true,true,true),
        (false, decls_neutral, wrap_none,    wrap_func   ) => (true,true,true),
        (false, decls_neutral, wrap_none,    wrap_hard   ) => (true,true,true),
        (false, decls_neutral, wrap_none,    wrap_neutral) => (true,true,true),
        (false, decls_none,    wrap_hard,    wrap_func   ) => (false,true,false),
        (false, decls_none,    wrap_hard,    wrap_hard   ) => (false,true,false),
        (false, decls_none,    wrap_hard,    wrap_neutral) => (false,true,false),
        (false, decls_none,    wrap_neutral, wrap_func   ) => (false,true,false),
        (false, decls_none,    wrap_neutral, wrap_hard   ) => (false,true,false),
        (false, decls_none,    wrap_neutral, wrap_neutral) => (false,true,false),
        (false, decls_none,    wrap_none,    wrap_func   ) => (false,true,false),
        (false, decls_none,    wrap_none,    wrap_hard   ) => (false,true,false),
        (false, decls_none,    wrap_none,    wrap_neutral) => (false,true,false),
        (true,  decls_func,    wrap_hard,    wrap_func   ) => (true,true,true,true),
        (true,  decls_func,    wrap_hard,    wrap_hard   ) => (true,true,true,true),
        (true,  decls_func,    wrap_hard,    wrap_neutral) => (true,true,true,true),
        (true,  decls_func,    wrap_neutral, wrap_func   ) => (true,true,true,true),
        (true,  decls_func,    wrap_neutral, wrap_hard   ) => (true,true,true,true),
        (true,  decls_func,    wrap_neutral, wrap_neutral) => (true,true,true,true),
        (true,  decls_func,    wrap_none,    wrap_func   ) => (true,true,true,true),
        (true,  decls_func,    wrap_none,    wrap_hard   ) => (true,true,true,true),
        (true,  decls_func,    wrap_none,    wrap_neutral) => (true,true,true,true),
        (true,  decls_hard,    wrap_hard,    wrap_func   ) => (true,true,true),
        (true,  decls_hard,    wrap_hard,    wrap_hard   ) => (true,true,true),
        (true,  decls_hard,    wrap_hard,    wrap_neutral) => (true,true,true),
        (true,  decls_hard,    wrap_neutral, wrap_func   ) => (true,true,true),
        (true,  decls_hard,    wrap_neutral, wrap_hard   ) => (true,true,true),
        (true,  decls_hard,    wrap_neutral, wrap_neutral) => (true,true,true),
        (true,  decls_hard,    wrap_none,    wrap_func   ) => (true,true,true),
        (true,  decls_hard,    wrap_none,    wrap_hard   ) => (true,true,true),
        (true,  decls_hard,    wrap_none,    wrap_neutral) => (true,true,true),
        (true,  decls_neutral, wrap_hard,    wrap_func   ) => (true,true,true),
        (true,  decls_neutral, wrap_hard,    wrap_hard   ) => (true,true,true),
        (true,  decls_neutral, wrap_hard,    wrap_neutral) => (true,true,true),
        (true,  decls_neutral, wrap_neutral, wrap_func   ) => (true,true,true),
        (true,  decls_neutral, wrap_neutral, wrap_hard   ) => (true,true,true),
        (true,  decls_neutral, wrap_neutral, wrap_neutral) => (true,true,true),
        (true,  decls_neutral, wrap_none,    wrap_func   ) => (true,true,true),
        (true,  decls_neutral, wrap_none,    wrap_hard   ) => (true,true,true),
        (true,  decls_neutral, wrap_none,    wrap_neutral) => (true,true,true),
        (true,  decls_none,    wrap_hard,    wrap_func   ) => (false,true,false),
        (true,  decls_none,    wrap_hard,    wrap_hard   ) => (false,true,false),
        (true,  decls_none,    wrap_hard,    wrap_neutral) => (false,true,false),
        (true,  decls_none,    wrap_neutral, wrap_func   ) => (false,true,false),
        (true,  decls_none,    wrap_neutral, wrap_hard   ) => (false,true,false),
        (true,  decls_none,    wrap_neutral, wrap_neutral) => (true,true,true),
        (true,  decls_none,    wrap_none,    wrap_func   ) => (false,true,false),
        (true,  decls_none,    wrap_none,    wrap_hard   ) => (false,true,false),
        (true,  decls_none,    wrap_none,    wrap_neutral) => (true,true,true),
    )
    expected_s(b::Bool) = b ? "assignment to outer var" : "brand-new var"

    local jl_mod = Module()
    local fl_mod = Module()

    @testset for ((soft_mode, decls_s, middle_s, assign_s), results) in expected_outer_vals,
            (lhs_i, lhs) in enumerate(lhs_names)

        ex = decls_s(middle_s(assign_s(:($lhs = true))), lhs)
        soft_mode && (ex = enable_softscope(ex))

        if lhs in (:argname, :spname) && decls_s !== decls_func
            continue
        elseif lhs === :spname
            @test_throws LoweringError expr_eval(jl_mod, ex)
        else
            @assert !isdefined(jl_mod, lhs) && !isdefined(fl_mod, lhs)
            expected = results[lhs_i]
            reference_ok = fl_eval(fl_mod, ex) === expected
            !reference_ok && @error("flisp produced unexpected result; fix that or JL scope tests:\n",
                                   "expected $(expected_s(expected)), got $(expected_s(!expected))\n", ex)
            @test reference_ok
            ok = expr_eval(jl_mod, ex) === expected
            !ok && @error("expected $(expected_s(expected)), got $(expected_s(!expected))\n", ex)
            @test ok
        end

        Core.@latestworld
        for mod in (jl_mod, fl_mod), n in (:gname, :name)
            isdefined(mod, n) && Base.delete_binding(mod, n)
        end
        Core.@latestworld
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

module ambiguous_local
    global x::Int = 0
end

function resolve_and_get_bindings(
        mod::Module, ex;
        world::UInt = Base.get_world_counter(),
        soft_scope::Union{Nothing,Bool} = nothing,
    )
    est = JuliaLowering.expr_to_est(ex)
    ctx1, ex1 = JuliaLowering.expand_forms_1(mod, est, false, world)
    ctx2, ex2 = JuliaLowering.expand_forms_2(ctx1, ex1)
    ctx3, _ = JuliaLowering.resolve_scopes(ctx2, ex2; soft_scope)
    return ctx3.bindings.info
end

@testset "is_ambiguous_local" begin
    # Assignment in for loop within begin block after toplevel assignment
    let bindings = resolve_and_get_bindings(ambiguous_local, :(for _ = 1:10; x = 1; end))
        binfo = only(filter(b->b.name=="x", bindings))
        @test binfo.kind === :local
        @test binfo.is_ambiguous_local
    end

    # while loop
    let bindings = resolve_and_get_bindings(ambiguous_local, :(while x < 5; x += 1; break; end))
        binfos = filter(b->b.name=="x", bindings)
        @test length(binfos) == 2
        binfo = only(filter(b->b.kind==:local, binfos))
        @test binfo.is_ambiguous_local
        @test count(b->b.kind==:global, binfos) == 1
    end

    # No ambiguity inside a function (hard scope)
    let bindings = resolve_and_get_bindings(ambiguous_local, :(function f()
            for _ = 1:10
                x = 1
            end
        end))
        binfo = only(filter(b->b.name=="x", bindings))
        @test binfo.kind === :local
        @test !binfo.is_ambiguous_local
    end

    # No ambiguity when shadowing global variable does not exist
    let bindings = resolve_and_get_bindings(ambiguous_local, :(for _ = 1:10; y = 1; end))
        binfo = only(filter(b->b.name=="y", bindings))
        @test binfo.kind === :local
        @test !binfo.is_ambiguous_local
    end

    # Explicit `global` should not produce ambiguous local
    let bindings = resolve_and_get_bindings(ambiguous_local, :(for _ = 1:10; global x = 1; end))
        binfo = only(filter(b->b.name=="x", bindings))
        @test binfo.kind === :global
    end

    # Block containing a toplevel assignment preceding a permeable scope
    let bindings = resolve_and_get_bindings(Module(), quote
            x = 0
            for _ = 1:10
                x = 1
            end
        end)
        binfos = filter(b->b.name=="x", bindings)
        @test length(binfos) == 2
        binfo = only(filter(b->b.kind==:local, binfos))
        @test binfo.is_ambiguous_local
        @test count(b->b.kind==:global, binfos) == 1
    end
    # Block containing a permeable scope followed by a toplevel assignment
    let bindings = resolve_and_get_bindings(Module(), quote
            for _ = 1:10
                x = 1
            end
            x = 0
        end)
        binfos = filter(b->b.name=="x", bindings)
        @test length(binfos) == 2
        binfo = only(filter(b->b.kind==:local, binfos))
        @test binfo.is_ambiguous_local
        @test count(b->b.kind==:global, binfos) == 1
    end

    # For some reason, flisp can avoid ambiguity when there is an additional `global` annotation.
    # JuliaLowering may want to follow suit, but it would be better to first decide on the details of this behaviour.
    let bindings = resolve_and_get_bindings(Module(), quote
            global x = 0
            for _ = 1:10
                x = 1
            end
        end)
        binfos = filter(b->b.name=="x", bindings)
        @test length(binfos) == 2
        binfo = only(filter(b->b.kind==:local, binfos))
        @test_broken !binfo.is_ambiguous_local
        @test count(b->b.kind==:global, binfos) == 1
    end
    let bindings = resolve_and_get_bindings(Module(), quote
            for _ = 1:10
                x = 1
            end
            global x = 0
        end)
        binfos = filter(b->b.name=="x", bindings)
        @test length(binfos) == 2
        binfo = only(filter(b->b.kind==:local, binfos))
        @test_broken !binfo.is_ambiguous_local
        @test count(b->b.kind==:global, binfos) == 1
    end

    @testset "soft_scope kwarg override" begin
        # Without soft_scope, x becomes an ambiguous local
        let bindings = resolve_and_get_bindings(ambiguous_local, :(for _ = 1:10; x = 1; end))
            binfo = only(filter(b->b.name=="x", bindings))
            @test binfo.kind === :local
            @test binfo.is_ambiguous_local
        end
        # With soft_scope=true, x stays global (no local created)
        let bindings = resolve_and_get_bindings(ambiguous_local, :(for _ = 1:10; x = 1; end); soft_scope=true)
            binfo = only(filter(b->b.name=="x", bindings))
            @test binfo.kind === :global
        end
    end

    @testset "world-age propagation" begin
        let m = Module()
            Core.eval(m, :(global x = 0))
            bindings = resolve_and_get_bindings(m, :(for _ = 1:10; x = 1; end); world=Base.get_world_counter())
            binfo = only(filter(b->b.name=="x", bindings))
            @test binfo.kind === :local
            @test binfo.is_ambiguous_local
        end
        let m = Module()
            Core.eval(m, :(global x = 0))
            bindings = resolve_and_get_bindings(m, :(for _ = 1:10; x = 1; end); world=Base.get_world_counter(), soft_scope=true)
            binfo = only(filter(b->b.name=="x", bindings))
            @test binfo.kind === :global
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
