## interface definition and implementation ##

macro interface(decl, body)
    interface_body = Expr[]
    if body.head != :block error("Invalid interface body") end
    for expr in body.args
        if typeof(expr) != Expr error("Unexpected non-expression $expr") end
        if expr.head == :line
            # Dict literal expressions don't like line annotations in the body
            continue
        end
        if expr.head == :call
            function_name = string(expr.args[1])
            function_sig = Expr(:tuple, expr.args[2:end]...)
            push!(interface_body, :(symbol($function_name) => $function_sig))
            continue
        end
        error("Unexpected expression $expr")
    end

    interface_dict = Expr(:dict, interface_body...)
    esc(Expr(:function, decl, Expr(:block, interface_dict)))
end

function used_typevars(vars, exprs)
    base_var(expr::Symbol) = expr
    function base_var(expr::Expr)
        if expr.head != :<: error("Unexpected expr $expr ($(expr.head))") end
        expr.args[1]
    end

    used = {[var, base_var(var), false] for var in vars}

    function check_expr(expr::Symbol)
        for ix = 1:length(used)
            if used[ix][2] == expr
                used[ix][3] = true
                return
            end
        end
    end
    function check_expr(expr::Expr)
        if expr.head != :curly error("Unexpected expr $expr") end
        for var in expr.args
            check_expr(var)
        end
    end

    for expr in exprs
        check_expr(expr)
    end
    result = Any[]
    for p in used
        if p[3] push!(result, p[1]) end
    end
    result
end

macro implement(decl, body)
    if decl.head != :call error("Invalid declaration") end
    if typeof(decl.args[1]) == Symbol
        interface_name = decl.args[1]
        interface_typevars = []
    elseif decl.args[1].head == :curly
        interface_name = decl.args[1].args[1]
        interface_typevars = decl.args[1].args[2:end]
    else
        error("Invalid declaration")
    end
    interface_params = decl.args[2:end]
    if length(interface_params) == 0 error("At least one parameter is needed") end
    
    interface = @eval $interface_name($interface_params...)
    
    implemented = Symbol[]
    
    if body.head != :block error("Invalid body") end
    final_block = quote end
    for expr in body.args
        if typeof(expr) != Expr error("Unexpected non-expression $expr") end
        if expr.head == :line
            push!(final_block.args, expr)
            continue
        end
        if expr.head == :(=)
            final_expr = deepcopy(expr)
            fn_decl = expr.args[1]
            fn_body = expr.args[2]

            if fn_decl.head != :call error("Invalid interface function declaration") end
            fn_decl_name = fn_decl.args[1]
            if typeof(fn_decl_name) != Symbol error("Invalid interface function declaration") end
            if !haskey(interface, fn_decl_name) error("Interface has no function $fn_decl_name") end

            interface_param_types = interface[fn_decl_name]
            fn_decl_params = fn_decl.args[2:end]
            if length(interface_param_types) != length(fn_decl_params) error("Mismatch between expected and actual parameter count") end

            if length(interface_typevars) > 0
                method_typevars = used_typevars(interface_typevars, interface_param_types)
                if length(method_typevars) > 0
                    final_expr.args[1].args[1] = Expr(:curly, fn_decl_name, method_typevars...)
                end
            end

            for ix = 1:length(interface_param_types)
                if typeof(fn_decl_params[ix]) != Symbol error("Interface function parameters must be unqualified") end
                param_name = fn_decl_params[ix]
                param_type = interface_param_types[ix]
                final_expr.args[1].args[1+ix] = :($param_name :: $param_type)
            end
            push!(final_block.args, final_expr)
            push!(implemented, fn_decl_name)
            continue
        end
        error("Unexpected expression $expr")
    end
    if length(implemented) != length(interface)
        missing = setdiff(keys(interface), implemented)
        error("Missing interface functions $missing")
    end
    esc(final_block)
end
