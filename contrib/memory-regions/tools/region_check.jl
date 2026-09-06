# The discipline checker.
#
# Region identity lives in a side table, not in the runtime. The hooked
# compiler hands every optimized IRCode to `instrument`, which wraps every
# allocation with a registration call and every reference store with a check.
# A store whose child is younger than its parent is a violation; violations
# are counted by (parent type, child type) with one recorded site each.

module RegionCheck

export with_region, region_report, region_names!, region_name, enable!, disable!

const ROOT = Int8(0)
# The chain's names belong to the model under test, not to the checker. A
# run calls region_names! once, with one name per region from 0 upward;
# region_report prints an unnamed region as "region N".
const REGION_NAMES = Dict{Int8,String}()
function region_names!(names::AbstractString...)
    empty!(REGION_NAMES)
    for (i, name) in enumerate(names)
        REGION_NAMES[Int8(i - 1)] = String(name)
    end
    return nothing
end
region_name(r::Integer) = get(REGION_NAMES, Int8(r), "region $(Int(r))")

const CURRENT = Ref(ROOT)
const ENABLED = Ref(false)

const REGION_OF = IdDict{Any,Int8}()

struct ViolationKey
    site::Int32
    parent::Type
    child::Type
    parent_region::Int8
    child_region::Int8
end

# One entry per instrumented store site: "Module.function:line".
const SITES = String[]
const VIOLATIONS = Dict{ViolationKey,Int}()
const PASSES = Dict{ViolationKey,Int}()
const CHECKS_RUN = Ref(0)
const REGISTERS_RUN = Ref(0)

@noinline function register!(x)
    ENABLED[] || return nothing
    (isbits(x) || x isa Symbol || x isa Type || x isa Module) && return nothing
    REGISTERS_RUN[] += 1
    REGION_OF[x] = CURRENT[]
    # An Array's element storage is a separate Memory object that element
    # stores name through a MemoryRef; give it the array's region.
    x isa Array && (REGION_OF[x.ref.mem] = CURRENT[])
    return nothing
end

# A `:new` both registers the object and checks every reference it embeds:
# for an immutable, construction IS the store.
@noinline function newobj!(site::Int, x, children...)
    ENABLED[] || return nothing
    register!(x)
    for child in children
        check!(site, x, child)
    end
    return nothing
end

@noinline function check!(site::Int, parent, child)
    ENABLED[] || return nothing
    CHECKS_RUN[] += 1
    (child === nothing || isbits(child)) && return nothing
    parent isa MemoryRef && (parent = parent.mem)
    child isa MemoryRef && (child = child.mem)
    parent_region = get(REGION_OF, parent, ROOT)
    child_region = get(REGION_OF, child, ROOT)
    key = ViolationKey(Int32(site), typeof(parent), typeof(child), parent_region, child_region)
    if child_region <= parent_region
        PASSES[key] = get(PASSES, key, 0) + 1
        return nothing
    end
    VIOLATIONS[key] = get(VIOLATIONS, key, 0) + 1
    return nothing
end

function with_region(f, r::Int8)
    old = CURRENT[]
    CURRENT[] = r
    try
        return f()
    finally
        CURRENT[] = old
    end
end

# Non-const on purpose; see the pass comment.
global register_dyn = register!
global newobj_dyn = newobj!
global check_dyn = check!

enable!() = (ENABLED[] = true; nothing)

"""Run `body` in region `r` without a closure — a `do` block boxes captured
locals, and the checker flags the box."""
macro in_region(r, body)
    quote
        local old = CURRENT[]
        CURRENT[] = $(esc(r))
        local result = $(esc(body))
        CURRENT[] = old
        result
    end
end
export @in_region
disable!() = (ENABLED[] = false; nothing)

function region_report()
    println("violations: ", sum(values(VIOLATIONS); init = 0), " stores at ",
            length(VIOLATIONS), " (parent type, child type) sites")
    rows = sort!(collect(VIOLATIONS); by = last, rev = true)
    for (key, count) in rows
        println(lpad(count, 9), "  ", region_name(key.parent_region), " ← ",
                region_name(key.child_region), "   ", key.parent, " ← ", key.child,
                "   at ", SITES[key.site])
    end
    println("passing stores, for coverage reading:")
    for (key, count) in sort!(collect(PASSES); by = last, rev = true)
        count < 100 && continue
        println(lpad(count, 9), "  ", region_name(key.parent_region), " ← ",
                region_name(key.child_region), "   ", key.parent, " ← ", key.child,
                "   at ", SITES[key.site])
    end
    return nothing
end

end # module RegionCheck

# --- the instrumentation pass, installed into the hooked compiler -------------

using Compiler: Compiler

const TOUCHED = Dict{String,Tuple{Int,Int}}()   # label -> (registers, checks)
const SKIPPED = Set{String}()
# The inserted calls dispatch through NON-CONST globals, so the callee is
# opaque to inference. Three failure modes forced this exact shape:
# a direct GlobalRef call is concrete-evaluated by inference inside the
# compiler and raises a world-age MethodError there; `invokelatest` with the
# function as a value gets const-propagated, and inference then descends the
# checker -> IdDict -> instrumented IdDict -> checker cycle until the stack
# overflows. An `Any`-typed binding read makes the call ineligible for both.
const REG_REF = GlobalRef(RegionCheck, :register_dyn)
const NEW_REF = GlobalRef(RegionCheck, :newobj_dyn)
const CHK_REF = GlobalRef(RegionCheck, :check_dyn)
const IN_PASS = Ref(false)
const STATS = Ref((methods = 0, registers = 0, checks = 0))

function _excluded(mod::Module)
    root = mod
    while parentmodule(root) !== root
        root = parentmodule(root)
    end
    return root === Compiler || mod === RegionCheck || root === Core
end

function _resolve(f)
    f isa GlobalRef || return f
    isdefined(f.mod, f.name) || return nothing
    return getglobal(f.mod, f.name)
end
_is(f, value) = _resolve(f) === value

function instrument(ir, opt)
    IN_PASS[] && return ir
    # No ENABLED gate here: everything compiled after installation carries the
    # calls, and the calls themselves return at once while disabled. Gating
    # compilation instead leaves every method that the static call graph
    # reaches uninstrumented, because those compile before `enable!` runs.
    def = opt.linfo.def
    mod = def isa Method ? def.module : def
    # The inserted calls travel through `invokelatest`; instrumenting it (or
    # its kernel) recurses at compile time.
    def isa Method && def.name in (:invokelatest, :invokelatest_trimmed, :_call_latest) && return ir
    if _excluded(mod::Module)
        push!(SKIPPED, string(mod))
        return ir
    end
    IN_PASS[] = true
    registers = 0
    checks = 0
    label = def isa Method ? string(def.module, ".", def.name, ":", def.line) : string(mod)
    site() = (push!(RegionCheck.SITES, label); length(RegionCheck.SITES))
    try
        for i in 1:length(ir.stmts)
            stmt = ir.stmts[i][:stmt]
            stmt isa Expr || continue
            if stmt.head === :new || stmt.head === :splatnew
                Compiler.insert_node!(ir, Compiler.SSAValue(i),
                    Compiler.NewInstruction(
                        Expr(:call, NEW_REF, site(), Compiler.SSAValue(i), stmt.args[2:end]...),
                        Nothing), #= attach after =# true)
                registers += 1
            elseif stmt.head === :call && length(stmt.args) >= 3
                f = stmt.args[1]
                if _is(f, Core.memorynew)
                    Compiler.insert_node!(ir, Compiler.SSAValue(i),
                        Compiler.NewInstruction(Expr(:call, REG_REF, Compiler.SSAValue(i)),
                                                Nothing), true)
                    registers += 1
                elseif _is(f, Core.setfield!) && length(stmt.args) >= 4
                    Compiler.insert_node!(ir, Compiler.SSAValue(i),
                        Compiler.NewInstruction(
                            Expr(:call, CHK_REF, site(), stmt.args[2], stmt.args[4]), Nothing))
                    checks += 1
                elseif _is(f, Core.memoryrefset!)
                    Compiler.insert_node!(ir, Compiler.SSAValue(i),
                        Compiler.NewInstruction(
                            Expr(:call, CHK_REF, site(), stmt.args[2], stmt.args[3]), Nothing))
                    checks += 1
                end
            end
        end
        old = STATS[]
        STATS[] = (methods = old.methods + 1, registers = old.registers + registers,
                   checks = old.checks + checks)
        (registers > 0 || checks > 0) && (TOUCHED[label] = (registers, checks))
        return Compiler.compact!(ir)
    finally
        IN_PASS[] = false
    end
end

function install_checker!()
    # Compile the checker's own path once BEFORE instrumentation turns on, so
    # the pass never has to instrument itself.
    push!(RegionCheck.SITES, "warmup")
    RegionCheck.register!(Ref(0))
    RegionCheck.check!(1, Ref(0), Ref(0))
    RegionCheck.newobj!(1, Ref(0), Ref(0))
    instrument === nothing && error("unreachable")
    Compiler.IR_HOOK[] = instrument
    return nothing
end
