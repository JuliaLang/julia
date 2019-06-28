# Lowering pass 1: Syntax desugaring
#
# In this pass, we simplify the AST by transforming much of the rich surface
# syntax into a smaller core syntax containing fewer expression heads.
#
# Some of this core syntax is also part of the surface syntax, but some is
# unique to the lowered code. For example, `Expr(:scope_block, ...)` all
# scoping in the core syntax is
# handled by the scope_block

using Core: SSAValue

# :(f(arg; par)).arg s =>  [:f, Expr(:parameters, :par), :arg]
has_parameters(args) = length(args) >= 2 && args[2] isa Expr && args[2].head === :parameters


isquoted(ex) = ex isa QuoteNode || (ex isa Expr &&
               ex.head in (:quote, :top, :core, :globalref, :outerref, :break, :inert, :meta))

# TODO: Shouldn't be global
const _ssa_index = Ref(0)
make_ssa() = SSAValue(_ssa_index += 1)

top(ex) = Expr(:top, ex)
core(ex) = Expr(:core, ex)
mapargs(f, ex) = ex isa Expr ? Expr(ex.head, map(f, ex.args)...) : ex

# replace `end` for the closest ref expression; don't go inside nested refs
function replace_end(ex, a, n, tuples, last)
    if ex === :end
        # the appropriate computation for an `end` symbol for indexing
        # the array `a` in the `n`th index.
        # `tuples` are a list of the splatted arguments that precede index `n`
        # `last` = is this last index?
        # returns a call to lastindex(a) or lastindex(a,n)
        if isempty(tuples)
            last && n == 1 ? Expr(:call, top(:lastindex), a) :
                             Expr(:call, top(:lastindex), a, n)
            #  @top(lastindex($a)) : @top(lastindex($a, $n)) ??
        else
            dimno = Expr(:call, top(:+), n - length(tuples),
                         map(t->:(Expr(:call, top(:length), t)), tuples)...)
            Expr(:call, top(:lastindex), a, dimno)
        end
    elseif !(ex isa Expr) || isquoted(ex)
        ex
    elseif ex.head == :ref
        # Only recurse into first argument of ref, not into index list.
        Expr(:ref, replace_end(ex.args[1], a, n, tuples, last), ex.args[2:end]...)
    else
        mapargs(x->replace_end(x, a, n, tuples, last), ex)
    end
end

# go through indices and replace the `end` symbol
# a = array being indexed, i = list of indices
# returns (values, index_list, stmts) where stmts are statements that need to
# execute first.
function process_indices(a, inds)
    
end

function partially_expand_ref(ex)
    a = ex.args[1]
    idxs = ex.args[2]
end

function expand_forms(ex)
    if ex isa Symbol
        ex
    elseif ex isa Expr
        head = ex.head
        if head == :ref
            !has_parameters(ex.args) || error("unexpected semicolon in array expression")
            expand_forms(partially_expand_ref(ex))
        end
    end
end
