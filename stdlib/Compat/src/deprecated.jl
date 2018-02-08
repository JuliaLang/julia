function depwarn_ex(msg, name)
    return quote
        if VERSION >= v"0.6.0"
            Base.depwarn($msg, Symbol($name))
        end
    end
end

function primarytype(@nospecialize(t))
    tn = t.name
    if isdefined(tn, :primary)
        return tn.primary
    else
        return tn.wrapper
    end
end

export @functorize
macro functorize(f)
    code = f === :scalarmax          ? :(Base.scalarmax) :
           f === :scalarmin          ? :(Base.scalarmin) :
           f === :centralizedabs2fun ? :(primarytype(typeof(Base.centralizedabs2fun(0)))) :
           f
    warning = depwarn_ex("@functorize is deprecated as functor objects are no longer supported in julia", "@functorize")
    return quote
        $warning
        $code
    end
end

@static if VERSION >= v"0.6.0"
    Base.@deprecate_binding KERNEL Sys.KERNEL
    Base.@deprecate_binding UTF8String Core.String
    Base.@deprecate_binding ASCIIString Core.String
    Base.@deprecate_binding unsafe_convert Base.unsafe_convert
    Base.@deprecate_binding remote_do Distributed.remote_do
    Base.@deprecate_binding Filesystem Base.Filesystem
    Base.@deprecate_binding AsyncCondition Base.AsyncCondition
    Base.@deprecate_binding promote_eltype_op Base.promote_eltype_op
    @eval Base.@deprecate_binding $(Symbol("@irrational")) Base.$(Symbol("@irrational"))
    @eval Base.@deprecate_binding $(Symbol("@blasfunc")) Compat.LinearAlgebra.BLAS.$(Symbol("@blasfunc"))
else
    const KERNEL = Sys.KERNEL
    const UTF8String = Core.String
    const ASCIIString = Core.String
    import Base.unsafe_convert
    import Base.remote_do
    import Base.Filesystem
    import Base.AsyncCondition
    import Base.promote_eltype_op
    import Base.@irrational
    import Base.LinAlg.BLAS.@blasfunc
end

if VERSION < v"0.7.0-DEV.2915"
    const textwidth = Compat.Unicode.textwidth
    export textwidth
end
