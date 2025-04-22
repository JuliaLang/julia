# This file is a part of Julia. License is MIT: https://julialang.org/license

# Full-featured versions of _eval_import and _eval_using

for m in methods(Core._eval_import)
    delete_method(m)
end
for m in methods(Core._eval_using)
    delete_method(m)
end

function call_require(into::Module, name::Symbol)
    # TODO: JL_TIMING(LOAD_IMAGE, LOAD_Require)
    build_mode = Bool(Base.JLOptions().incremental) && Bool(ccall(:jl_generating_output, Cint, ()))
    m = nothing
    req_w = unsafe_load(cglobal(:jl_require_world, UInt))
    cur_w, tls_w = get_world_counter(), tls_world_age()
    if isdefined(Base, :require)
        w = build_mode && req_w < tls_w ? req_w : cur_w
        m = invoke_in_world(w, getglobal(Base, :require), into, name)
    end
    m isa Module || error("failed to load module $v")
    m
end

function eval_import_path(at::Module, from::Union{Module, Nothing}, path::Expr, keyword::String)
    isempty(path.args) && error("malformed import statement")

    i::Int = 1
    function next!()
        i <= length(path.args) || error("invalid module path")
        v = path.args[i]
        i += 1
        v isa Symbol || throw(TypeError(Symbol(keyword), "", Symbol, v))
        v
    end
    v = next!()
    m = nothing

    if from !== nothing
        m = from
    elseif v !== :.
        # `A.B`: call the loader to obtain the root A in the current environment.
        if v === :Core
            m = Core
        elseif v === :Base
            m = Base
        else
            m = call_require(at, v)
        end
        i > lastindex(path.args) && return m, nothing
        v = next!()
    else
        # `.A.B.C`: strip off leading dots by following parent links
        m = at
        while (v = next!()) === :.
            m = parentmodule(m)
        end
    end

    while true
        v === :. && error("invalid $keyword path: \".\" in identifier path")
        i > lastindex(path.args) && break
        m = getglobal(m, v)
        m isa Module || error("invalid $keyword path: \"$v\" does not name a module")
        v = next!()
    end
    m, v
end

function eval_import_path_all(at::Module, path::Expr, keyword::String)
    m, v = eval_import_path(at, nothing, path, keyword)
    if v !== nothing
        m = getglobal(m, v)
        m isa Module || error("invalid $keyword path: \"$v\" does not name a module")
    end
    m
end

function check_macro_rename(from::Symbol, to::Symbol, keyword::String)
    c1(sym) = bitcast(Char, UInt32(unsafe_load(unsafe_convert(Ptr{UInt8}, sym))) << 24)
    from_c, to_c = c1(from), c1(to)
    if from_c == '@' && to_c != '@'
        error("cannot rename macro \"$from\" to non-macro \"$to\" in \"$keyword\"")
    end
    if from_c != '@' && to_c == '@'
        error("cannot rename non-macro \"$from\" to macro \"$to\" in \"$keyword\"")
    end
end

"""
    _eval_import(imported::Bool, to::Module, from::Union{Expr, Nothing}, paths::Expr...)

Evaluate the import paths, calling `Core._import` for each name to be imported.
`imported` imports are created with `import`, `using A: x` sets this to false.
The `from` is the part of the import path before the `:`.  This is the lowered
form of `import`, `import ...:`, and `using ...:`.

```
import A             => _eval_import(true,  Main, nothing,          Expr(:., :A))
import A.b           => _eval_import(true,  Main, nothing,          Expr(:., :A, :b))
import A.b as c      => _eval_import(true,  Main, nothing,          Expr(:as, Expr(:., :A, :b), :c))
import A.B: C.d, e   => _eval_import(true,  Main, Expr(:., :A, :B), Expr(:., :C, :d), Expr(:., :e))
import A.B: C.d as e => _eval_import(true,  Main, Expr(:., :A, :B), Expr(:as, Expr(:., :C, :d), :e))
using  A.B: C.d, e   => _eval_import(false, Main, Expr(:., :A, :B), Expr(:., :C, :d), Expr(:., :e))

See also [`_import`](@ref Core._import).
```
"""
function Core._eval_import(imported::Bool, to::Module, from::Union{Expr, Nothing}, paths::Expr...)
    keyword = imported ? "import" : "using"
    fail() = error("malformed \"$keyword\" statement")
    from = from !== nothing ? eval_import_path_all(to, from, keyword) : nothing

    for path in paths
        path isa Expr || fail()
        asname = nothing
        if path.head === :as && length(path.args) == 2
            path, asname = path.args
        elseif path.head !== :.
            fail()
        end
        old_from = from
        from, name = eval_import_path(to, from, path, keyword)
        asname = asname === nothing ? name : asname

        if name !== nothing
            check_macro_rename(name, asname, keyword)
            Core._import(to, from, asname, name, imported)
        else
            Core._import(to, from, nameof(from))
        end
    end
end

"""
    _eval_using(to::Module, path::Expr)

Evaluate the import path to a module and call [`Core._using`](@ref) on it,
making its exports available to the `to` module; this is the lowered form of
`using A`.

```
using A.B            => _module_using(Main, Expr(:., :A, :B))
```

See also [`_using`](@ref Core._using).
"""
function Core._eval_using(to::Module, path::Expr)
    from = eval_import_path_all(to, path, "using")
    Core._using(to, from)
    is_package = length(path.args) == 1 && path.args[1] !== :.
    if to == Main && is_package
        Core._import(to, from, nameof(from))
    end
end
