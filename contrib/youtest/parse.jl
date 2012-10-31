function canonicalize_input(exp_tuple)
    # On the inner levels, sequences of expressions will appear as
    # arrays, but the macro receives the top-level expressions as an
    # argument list, in a tuple. Avoid feeding this special case to
    # the low-level machinery.

    # In the case of a single line macro call, will get a tuple of
    # arbitrary many expressions. Wrap these in a block.

    # Alternatively, the macro gets a single block: pass it on.
    if (length(exp_tuple) == 1  &&
        isa(exp_tuple[1], Expr) &&
        exp_tuple[1].head == :block)
        exp_tuple[1]
    else
        Expr(:block, [ item for item in exp_tuple ], nothing)
    end
end

Test() = Dict{Any, Any}()

###################################
####### Test Parser ###############
###################################

# There are only four special symbols hard-wired into the parser
# (well, true and false are hard-wired into the language, so the
# parser has to treat them as special cases too):

# 0. Booleans are hardwired in the language: ':true' and ':false'
#    evaluate to booleans, rather than symbols, so they need to be
#    treated specially by the framework. (There may be other stuff in
#    this category which I haven't discovered yet.)

# 1. :=        Defines metadata

# 2. blockmode Swiches on block mode for the current block.

# 3. to-be-chosen escape symbol: Prevents lone symbols from being
#    treated as metadata shorthand (see next point).

# 4. test_code Identifies the non-metadata at the core of any test.

# Expressions consisting of a single symbol are shorthand for
#
#     <symbol> := true

# Everything else is treated as test body code.

# TODO: discuss blockmode vs. linemode.

function parse(block, inherited_scope, all_tests)
    single = contains(block.args, :single)
    implicit_name(block) # Can/should this be done with a plugin?
    code_lines_to_be_joined = {}
    current_scope = Scope(inherited_scope)
    for exp in block.args
        kind = classify(exp)
        if kind == :metadata
            store_metadatum(exp, current_scope)
        elseif kind == :code
            if single
                push(code_lines_to_be_joined, exp)
            else
                push(all_tests, Test(exp, current_scope))
            end
        elseif kind == :block
            parse(exp, Scope(current_scope), all_tests)
        elseif kind == :ignore
        else
            throw(FrameworkError(
              "Unexpected object when parsing test. Type: $(typeof(exp)), Value:\n$exp"))
        end
    end
    if single && length(code_lines_to_be_joined) > 0
        code_lines_wrapped_in_a_block = Expr(:block, code_lines_to_be_joined, nothing)
        enqueue(all_tests, Test(code_lines_wrapped_in_a_block, current_scope))
    end
end

Test(code, scope) = { :code => code, :scope => scope }

function classify(it)
    if isa(it, Expr)
        if     it.head == :(:=);      :metadata
        elseif it.head == :block;     :block
        elseif it.head == :line;      :ignore
        else                          :code
        end
    elseif isa(it, Symbol)
        if has(default, it);          :metadata
        elseif it != :single;         :code
        else                          :ignore
        end
    elseif isa(it, Bool);             :code
    elseif isa(it, LineNumberNode);   :ignore
    else
        throw(FrameworkError("Couldn't classify expression when parsing test. Type: $(typeof(it)), Value:\n$it"))
    end
end

is_on_single_line(block) = !any(e -> isa(e, LineNumberNode) ||
                                isa(e, Expr) && e.head == :line, block.args)

function store_metadatum(it, scope)
    if isa(it, Symbol)
        scope[it] = default[it]
    else
        scope[it.args[1]] = it.args[2]
    end
end

function implicit_name(block)
    for (n,it) in enumerate(block.args)
        if isa(it, String)
            block.args[n] = :(name := $it)
        end
    end
end
