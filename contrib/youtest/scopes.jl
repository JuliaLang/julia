#######################################################
#################### Nested Scopes ####################
#######################################################

include("helpers.jl")

type Scope
    here :: Dict{Symbol, Any}
    next :: Union(Nothing, Scope)
end

# Scopes may be created in two ways:
# A bottom level scope
Scope()          = Scope(Dict{Symbol, Any}(), nothing)
# A scope layered on top of another
Scope(s::Scope)  = Scope(Dict{Symbol, Any}(), s)
Scope(::Nothing) = Scope()

function get(scope::Scope, symbol::Symbol, notfound)
    if has(scope.here, symbol)
        scope.here[symbol]
    elseif !is(scope.next, nothing)
        get(scope.next, symbol, notfound)
    else
        notfound
    end
end

# For use as safe third argument to get
const KEYNOTFOUND = gensym("Key Not Found")

function ref(scope::Scope, symbol::Symbol)
    found = get(scope, symbol, KEYNOTFOUND)
    found == KEYNOTFOUND ? throw(KeyError(symbol)) : found
end

function has(scope::Scope, symbol::Symbol)
    found = get(scope, symbol, KEYNOTFOUND)
    found == KEYNOTFOUND ? false : true
end

function has(scope::Scope, symbol::Symbol, style::Symbol)
    if is(style, :here)
        has(scope.here, symbol)
    else # :shadow and :combine give the same result
        has(scope, symbol)
    end
end

function ref(scope::Scope, symbol::Symbol, style::Symbol)
    if is(style, :here)
        scope.here[symbol]

    elseif is(style, :combine)
        binding = scope.next == nothing ? {} : scope.next[symbol, style]
        if has(scope.here, symbol)
            push(binding, scope.here[symbol])
        else
            
        end
        binding
    else
        error("YouTest: Unknown scope lookup style: $style")
    end
end

multiple_binding_strategy = Dict{Symbol, Any}()

type RebindingNotAllowed <: FrameworkError
    symbol :: Symbol
end

show(io, e::RebindingNotAllowed) = print(io, "YouTest: Rebinding in the same scope is not allowed for '$(e.symbol)'.")


function assign(scope::Scope, value, symbol::Symbol)
    # What should happen if a name that was already bound at THIS
    # LEVEL is being bound again? (Note that this is a separate issue
    # from that of dealing with bindings of the same name in an
    # enclosing scope. The latter is resolved by giving :shadow,
    # :combine or :here as extra arguments to has or ref, in scope
    # lookups.) The framework offers the plugin writer several options
    #
    # 1. It should not be allowed: it is an error. (Default)
    #
    # 2. More recent bindings should displace earlier ones: A warning
    #    is printed.
    #
    # 3. All the bindings should be combined into a single one, using
    #    a given strategy.
    #
    # This choice can be made on a per-symbol basis, and is recorded
    # in `multiple_binding_strategy`.
    if has(scope.here, symbol)
        if has(multiple_binding_strategy, symbol)
            if multiple_binding_strategy[symbol] == :displace
                # Option 2 above
                println("YouTest: Warning, '$(symbol)' is being re-assigned in the same scope.")
                scope.here[symbol] = value
            else
                # Option 3 above
                combine = multiple_binding_strategy[symbol]
                scope.here[symbol] = combine(scope.here[symbol], value)
            end
        else
            # Option 3 above
            throw(RebindingNotAllowed(symbol))
        end
    else
        scope.here[symbol] = value
    end
end



         flatten(scope::Scope) = flatten(scope, :shadow)
function flatten(scope::Scope, style::Symbol)
    if style == :here
        return copy(scope.here)
    end
    so_far = scope.next == nothing ? Dict{Symbol, Any}() : flatten(scope.next, style)
    for (k,v) in scope.here
        if style == :shadow
            so_far[k] = v
        else # :combine
            if !has(so_far, k)
                so_far[k] = {}
            end
            push(so_far[k], v)
        end
    end
    so_far
end

############################## Tests ########################################

bottom = Scope()
# New scopes should be empty
@throws KeyError bottom[:a]
# Setting should work
bottom[:a] = 1
@assert bottom[:a] == 1
# Layered scopes should inherit from their base
layer1 = Scope(bottom)
@assert layer1[:a] == 1
# Layered scopes should shadow their base
layer1[:a] = 2
@assert bottom[:a] == 1
@assert layer1[:a] == 2
# Rebinding shadowed name in lower scope should leave higher level
# unchanged (but first we need to enable rebinding for the symbol)
multiple_binding_strategy[:a] = :displace
bottom[:a] = 4
@assert layer1[:a] == 2
# Clean up
del(multiple_binding_strategy, :a)
# New names introduced in lower scope after higher level was created,
# should appear in higher level.
bottom[:b] = 5
@assert layer1[:b] == 5

### There are three different lookup styles:
#
# 1. Shadowing     (the default)
#
#              Needs no introduction.
#
# 2. Accumulating  (:combine)
#
#              All the bindings of the symbol, all the way up from the
#              bottom, are combined into an ordered collection. This
#              will be used for things like `name`.
#
# 3. Single level  (:here)
#
#              Bingings from lower levels are not inherited by higher
#              levels. This will be used for things like `linemode`.

bottom = Scope()       ; bottom[:b] = 0
layer1 = Scope(bottom) ; layer1[:b] = 1
layer2 = Scope(layer1)
layer3 = Scope(layer2) ; layer3[:b] = 3

@assert bottom[:b]           ==  0
@assert bottom[:b, :here]    ==  0
@assert bottom[:b, :combine] == [0]

@assert layer1[:b]           ==     1
@assert layer1[:b, :here]    ==     1
@assert layer1[:b, :combine] == [0, 1]

@assert layer2[:b]           ==     1
@throws layer2[:b, :here] KeyError
@assert layer2[:b, :combine] == [0, 1]

@assert layer3[:b]           ==        3
@assert layer3[:b, :here]    ==        3
@assert layer3[:b, :combine] == [0, 1, 3]



# There are three different strategies for dealing with multiple
# assignments to the same name in the same scope.  See the comments in
# assign(scope::Scope, value, symbol::Symbol)
the_symbol = gensym()
variety = Scope()
# By default, it is an error to reset
variety[the_symbol] = 1
@throws RebindingNotAllowed variety[the_symbol] = 2
@assert variety[the_symbol] == 1
# But it is perfectly OK to shadow
higher = Scope(variety)
@assert higher[the_symbol] == 1
higher[the_symbol] = 2
@assert higher[the_symbol] == 2
@assert variety[the_symbol] == 1
# Overwriting may be enabled on a per-symbol basis
multiple_binding_strategy[the_symbol] = :displace
variety[the_symbol] = 3
@assert variety[the_symbol] == 3
# Or a stategy may be provided for combining the different bindings
multiple_binding_strategy[the_symbol] = (+)
variety[the_symbol] = 4
@assert variety[the_symbol] == 7
variety[the_symbol] = 5
@assert variety[the_symbol] == 12

# Helper for the next tests
function isequal(l::Dict, r::Dict)
    if length(l) != length(r) return false end
    for (k,v) in l
        if !(has(r,k) && isequal(r[k], v))
            return false
        end
    end
    true
end

# For use in tests of the test parser.
@assert isequal(flatten(bottom),           {:b =>  0})
@assert isequal(flatten(bottom, :here),    {:b =>  0})
@assert isequal(flatten(bottom, :combine), {:b => [0]})

@assert isequal(flatten(layer1),           {:b =>     1})
@assert isequal(flatten(layer1, :here),    {:b =>     1})
@assert isequal(flatten(layer1, :combine), {:b => [0, 1]})

@assert isequal(flatten(layer2),           {:b =>     1})
@assert isequal(flatten(layer2, :here),    Dict{Symbol, Any}())
@assert isequal(flatten(layer2, :combine), {:b => [0, 1]})

@assert isequal(flatten(layer3),           {:b =>     3})
@assert isequal(flatten(layer3, :here),    {:b =>     3})
@assert isequal(flatten(layer3, :combine), {:b => [0, 1, 3]})
