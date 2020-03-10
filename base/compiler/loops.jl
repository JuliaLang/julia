# This file is a part of Julia. License is MIT: https://julialang.org/license

# This file implements loop optimizations, such as splitting
# `iterate(::CartesianIndices{N})` into `N` one-dimensional loops.

enterif(@nospecialize(ex), head::Symbol, argidx::Int) =
    isexpr(ex, head) && length(ex.args) >= argidx ? ex.args[argidx] : ex

isglobalref(g, mod::Module, name::Symbol) =
    isa(g, GlobalRef) && g.mod === mod && g.name === name

getlhs(@nospecialize(src), id::Int) =
    isexpr(src.code[id], :(=)) ? src.code[id].args[1] : SSAValue(id)
getrhs(@nospecialize(src), id::Int) = enterif(src.code[id], :(=), 2)

# Extract loop information from a call to `iterate`
#    iterator, iteratortype, betwixt, itervar, bodystart, bodystop, loopexit = extract_iterate(src, id)
# where
#   - iterator::Union{Slot,SSAValue} is the iterator
#   - iteratortype is the type of the iterator
#   - betwixt::UnitRange{Int} is a range containing statements between
#     the :gotoifnot and the tuple destructuring
#   - itervar::Union{Slot,SSAValue} is the `i` in `for i = ...`
#   - bodystart::Int is the id of the first statement of the loop body
#   - bodystop::Int is the id of the last statement of the loop body
#   - loopexit::Int is the id of the first post-loop statement
# Note: it may also return `nothing`, meaning that it doesn't match the expected pattern.
function extract_iterate(src::CodeInfo, id::Int)
    # lhs = getlhs(src, id)
    rhs = getrhs(src, id)
    @assert isexpr(rhs, :call) && length(rhs.args) == 2
    @assert isglobalref(rhs.args[1], Main.Base, :iterate)
    iterator = rhs.args[2]
    iteratortype = if isa(iterator, SSAValue)
        src.ssavaluetypes[iterator.id]
    elseif isa(iterator, SlotNumber) || isa(iterator, TypedSlot)
        src.slottypes[iterator.id]
    else
        Any
    end
    # Identify the loop terminus by looking for the :gotoifnot. Because some code
    # might manually call `iterate` and insert other statements in a form
    # we're not prepared to handle, use strict pattern-matching and bail
    # if it doesn't match.
    if length(src.code) <= id + 3
        return nothing
    end
    idgoto = id
    stmt = src.code[idgoto+=1]   # should be `ret === nothing`
    if !(isexpr(stmt, :call) && length(stmt.args) == 3 &&
         isglobalref(stmt.args[1], Core, :(===)) &&
         stmt.args[3] === nothing)
        return nothing
    end
    stmt = src.code[idgoto+=1]   # should be `not_int(%)`
    if !(isexpr(stmt, :call) && length(stmt.args) == 2 &&
         isglobalref(stmt.args[1], Main.Base, :not_int) &&
         stmt.args[2] === SSAValue(idgoto-1))
        return nothing
    end
    stmt = src.code[idgoto+=1]   # should be `goto # if not %`
    if !(isexpr(stmt, :gotoifnot) && stmt.args[1] === SSAValue(idgoto-1))
        return nothing
    end
    loopexit = src.code[idgoto].args[2]
    # Extract the iteration variable and identify the loop body start
    bodystart = idgoto + 1
    betwixt_start = bodystart
    rhs = getrhs(src, bodystart)
    while !(isexpr(rhs, :call) && length(rhs.args) == 3 &&
            isglobalref(rhs.args[1], Core, :getfield) &&
            rhs.args[3] == 1)  # tuple-destructuring call (get item)
        bodystart += 1
        if bodystart > length(src.code)
            return nothing
        end
        rhs = getrhs(src, bodystart)
    end
    betwixt = betwixt_start:bodystart-1
    itervar = getlhs(src, bodystart)
    bodystart += 1
    rhs = getrhs(src, bodystart)
    @assert isexpr(rhs, :call) && length(rhs.args) == 3 &&
        isglobalref(rhs.args[1], Core, :getfield) &&
        rhs.args[3] == 2  # tuple-destructuring (get iterator state)
    bodystart += 1
    # Identify the inner call to iterate
    bodystop = loopexit - 1
    rhs = getrhs(src, bodystop)
    while bodystop > 1 && !(isexpr(rhs, :call) && length(rhs.args) == 3 && isglobalref(rhs.args[1], Main.Base, :iterate))
        bodystop -= 1
        rhs = getrhs(src, bodystop)
    end
    return iterator, iteratortype, betwixt, itervar, bodystart, bodystop-1, loopexit
end

# For manipulating loops (and especially nested loops), it makes sense
# to transiently go back to a nested block structure to avoid lots of
# `insert!` and `deleteat!` shuffling.

# Hold the parts of a CodeInfo that we may rewrite
struct StmtBlock
    stmts::Vector{Any}
    codelocs::Vector{Int32}
    codelocdefault::Int32       # so empty blocks can be expanded correctly
    ssavaluetypes::Vector{Any}
    # Note: no ssaflags
    ssavalueids::Vector{Int32}  # the ids that `stmts` were created with
end
StmtBlock(codelocdefault::Integer) =
    StmtBlock([], Int32[], Int32(codelocdefault), [], Int32[])
StmtBlock(src::CodeInfo, rng::UnitRange) =
    StmtBlock(Any[copy_exprs(src.code[i]) for i in rng],
              src.codelocs[rng],
              src.codelocs[first(rng)],
              src.ssavaluetypes[rng],
              Int32[i for i in rng])

function addstmt!(sb::StmtBlock, @nospecialize(stmt), @nospecialize(typ), id::Integer)
    push!(sb.stmts, stmt)
    push!(sb.codelocs, isempty(sb.codelocs) ? sb.codelocdefault : sb.codelocs[end])
    push!(sb.ssavaluetypes, typ)
    push!(sb.ssavalueids, id)
    return id+1
end

function append!(dest::StmtBlock, src::StmtBlock)
    append!(dest.stmts, src.stmts)
    append!(dest.codelocs, src.codelocs)
    append!(dest.ssavaluetypes, src.ssavaluetypes)
    append!(dest.ssavalueids, src.ssavalueids)
    return dest
end


mutable struct PrePostBlock
    pre::StmtBlock
    body::Union{Nothing,StmtBlock,PrePostBlock}
    post::StmtBlock
end

function append!(dest::StmtBlock, src::PrePostBlock)
    append!(dest, src.pre)
    append!(dest, src.body)
    append!(dest, src.post)
    return dest
end


#     block, enterid, item, itemtype, state, statetype, enterid, nextid = iterate_block(...)
# Create a block of statements implementing an `iterate` call.
# Call it once for the loop entrance and once for the exit; for the
#   exit you need to pass `state` and `statetype`.
# Arguments:
#   - iter is the reference to the iterator
#   - itertype is typeof(dereferenced iter)
#   - slotid is the slot number to which to assign the return value of
#     `iterate`, or an Int=>Symbol pair also specifying the name
#   - startloc is the starting codeloc
#   - nextid is the starting statement #/SSAValue
#   - state is a reference to the iterator state, or `nothing` on entrance
#   - statetype is typeof(dereferenced state), or `nothing` on entrance
# and
#  - block is the created StmtBlock
#  - enterid is the SSAValue of the entrance to the loop body, or `nothing` on exit
#  - item::SSAValue is a reference to the item returned from `iterate`
#  - itemtype::Type typeof(dererenced item)
#  - state::SSAValue is a reference to the state returned from `iterate`
#  - statetype::Type typeof(dereferenced state)
#  - nextid::Int is the next free statement #/SSAValue
function iterate_block!(src::CodeInfo, iter::Union{Slot,SSAValue}, itertype::Type, slotinfo::Union{Int,Pair{Int,Symbol}}, startloc::Int, enterid::Int, exitid::Int, nextid::Int, world, state::Union{Nothing,SSAValue}=nothing, statetype::Union{Nothing,Type}=nothing)
    @nospecialize
    if isa(slotinfo, Int)
        slotid, slotname = slotinfo, Symbol("")
    else
        slotid, slotname = slotinfo.first, slotinfo.second
    end
    block = StmtBlock(startloc)
    # Add the `iterate(iter)` or `iterate(iter, state)` call
    iterret = SlotNumber(slotid)
    if state === nothing
        iterrettype = _return_type(Main.Base.iterate, Tuple{itertype}, world)
        nextid = addstmt!(block,
                          Expr(:(=), iterret, Expr(:call, GlobalRef(Main.Base, :iterate), iter)),
                          iterrettype,
                          nextid)
    else
        iterrettype = _return_type(Main.Base.iterate, Tuple{itertype,statetype}, world)
        nextid = addstmt!(block,
                          Expr(:(=), iterret, Expr(:call, GlobalRef(Main.Base, :iterate), iter, state)),
                          iterrettype,
                          nextid)
    end
    if !isassigned(src.slotnames, slotid)
        @assert length(src.slotnames) == slotid - 1
        # push!(src.slotnames, Symbol(Main.Base.string(slotname, "[", i, "]")))
        push!(src.slotnames, slotname)
        push!(src.slotflags, 0x02)  # 0x02 = assigned
        push!(src.slottypes, iterrettype)
    end
    # %d === nothing
    addstmt!(block,  # deliberately don't increment nextid
             Expr(:call, GlobalRef(Core, :(===)), iterret, nothing),
             Bool,
             nextid)
    # Base.not_int(%d)
    addstmt!(block,
             Expr(:call, GlobalRef(Main.Base, :not_int), SSAValue(nextid)),
             Bool,
             nextid+1)
    # goto #x if not %y
    nextid = addstmt!(block,
                      Expr(:gotoifnot, SSAValue(nextid+1), exitid),
                      Any,
                      nextid+2)
    if isa(iterrettype, Union)
        itemstatetype = iterrettype.a === Nothing ? iterrettype.b : iterrettype.a
        itemtype  = itemstatetype.parameters[1]
        statetype = itemstatetype.parameters[2]
    else
        itemstatetype = itemtype = statetype = Any
    end
    item = nothing
    if state === nothing
        # item, state = iterret
        enterid = nextid
        iterretssa = SSAValue(nextid)
        # Union-splitting via insertion of a TypedSlot
        nextid = addstmt!(block,
                          TypedSlot(iterret.id, itemstatetype),
                          itemstatetype,
                          nextid)
        item = SSAValue(nextid)
        nextid = addstmt!(block,
                          Expr(:call, GlobalRef(Core, :getfield), iterretssa, 1),
                          itemtype,
                          nextid)
        state = SSAValue(nextid)
        nextid = addstmt!(block,
                          Expr(:call, GlobalRef(Core, :getfield), iterretssa, 2),
                          statetype,
                          nextid)
    else
        # Jump back to top of loop
        nextid = addstmt!(block,
                          GotoNode(enterid),
                          Any,
                          nextid)
        enterid = nothing
    end
    return block, enterid, item, itemtype, state, statetype, nextid
end

# Convert an iteration over a CartesianIndices into a set of nested loops
# `src.code[id]` must contain the call to `iterate(::CartesianIndices)`
function expand_cartesian_loop!(src::CodeInfo, id::Int, world)
    @assert isempty(src.ssaflags)
    @assert !isempty(src.ssavaluetypes)  # puzzlingly, src.inferred might still be false
    iterate_info = extract_iterate(src, id)
    if iterate_info === nothing
        return false   # abort due to pattern-matching failure
    end
    iterator, iteratortype, betwixt, itervar, bodystart, bodystop, loopexit = iterate_info
    @assert iteratortype <: Main.Base.CartesianIndices
    # slotname = isa(iterator, Slot) ? Main.Base.string("#", Main.Base.String(src.slotnames[iterator.id])) : Main.Base.String(gensym())
    if isa(iteratortype, UnionAll)
        return false   # abort if `N` has not (yet) been inferred
    end
    N = iteratortype.parameters[1]           # number of loops (dimensionality)
    tt = iteratortype.parameters[2]          # typeof(iter.indices)
    # Check to see if the "betwixt" segment matches known patterns.
    # Nontrivial betwixt segments arise in multi-iterator for loops, e.g.,
    #    for Ipre in Rpre, i in rng, Ipost in Rpost
    iterret = getlhs(src, id)
    for i in betwixt
        stmt = src.code[i]
        if i == last(betwixt)
            # the final statement of the block should be a typeassert
            # on the return value of `iterate`. We'll end up dropping this because
            # it's an assert on the return type of `iterate(::CartesianIndices)` and
            # we're replacing that statement.
            if !(isa(stmt, TypedSlot) && stmt.id == iterret.id)
                return false
            end
        else
            if isa(stmt, SlotNumber) || (isexpr(stmt, :(=)) && isa(stmt.args[1], SlotNumber))
                # allow assignments to slots
            elseif isa(stmt, NewvarNode)
                # allow NewvarNode
            else
                return false  # abort due to unknown "betwixt" statement
            end
        end
    end
    # Extract the code chunks
    wholepre  = StmtBlock(src, 1:id-1)
    body = StmtBlock(src, bodystart:bodystop)
    if length(betwixt) > 1   # don't include the TypedSlot statement
        body = append!(StmtBlock(src, first(betwixt):last(betwixt)-1), body)
    end
    wholepost = StmtBlock(src, loopexit:length(src.code))
    nextid = length(src.code) + 1  # the next-to-be-created SSAValue.id
    # Extract the indices into separate SSAValues
    destruct, indicesid = StmtBlock(src.codelocs[id]), nextid
    ## Get the .indices field of the CartesianIterator
    nextid = addstmt!(destruct,
                      Expr(:call, GlobalRef(Main.Base, :getproperty), iterator, QuoteNode(:indices)),
                      tt,
                      nextid)
    ## Get the elements of the indices tuple
    iteratorids = Array{SSAValue}(undef, N)  # id of each 1d iterator
    for i = 1:N
        iteratorids[i] = SSAValue(nextid)
        nextid = addstmt!(destruct,
                          Expr(:call, GlobalRef(Main.Base, :getindex), SSAValue(indicesid), i),
                          tt.parameters[i],
                          nextid)
    end
    # Build the loops
    startloc, stoploc = src.codelocs[id], src.codelocs[bodystop+1]
    loopbody = PrePostBlock(StmtBlock(startloc),
                            body,
                            StmtBlock(stoploc))
    iteritem_ids  = Array{SSAValue}(undef, N)
    exitid = loopexit
    # From the standpoint of marking entrances and exits, it's best to
    # generate the loops from outer-to-inner. But nesting should go
    # the other direction, so we temporarily store a list of
    # independent loops.
    loops = Vector{PrePostBlock}(undef, N)
    outerid = innerid = nextid
    for i = N:-1:1
        # Add the `iterate(iter)` call
        pre, enterid, item, itemtype, state, statetype, nextid =
            iterate_block!(src, iteratorids[i],
                           tt.parameters[i],
                           length(src.slotnames)+1,
                           Int(startloc), -1, Int(exitid), Int(nextid), world)
        iteritem_ids[i]  = item
        # Add the `iterate(iter, state)` call
        if i == N
            outerid = nextid
        end
        if i == 1
            innerid = nextid
        end
        post, _, _, _, _, _, nextid =
            iterate_block!(src, iteratorids[i],
                           tt.parameters[i],
                           length(src.slotnames),
                           Int(stoploc), Int(enterid), Int(exitid),
                           Int(nextid), world, state, statetype)
        loops[i] = PrePostBlock(pre, nothing, post)
        exitid = post.ssavalueids[1]
    end
    # In the loop body, create the `CartesianIndex` that would have
    # resulted from iterating the CartesianIndices
    tupleid = SSAValue(nextid)
    nextid = addstmt!(loopbody.pre,
                      Expr(:call, GlobalRef(Core, :tuple), iteritem_ids...),
                      NTuple{N,Int},
                      nextid)
    nextid = addstmt!(loopbody.pre,
                      Expr(:(=), itervar, Expr(:new, Main.Base.CartesianIndex{N}, tupleid)),
                      Main.Base.CartesianIndex{N},
                      nextid)
    # Nest the loops
    if N > 0
        outerloop = loop = loops[N]
        for i = N-1:-1:1
            loop.body = loops[i]
            loop = loops[i]
        end
        loop.body = loopbody
    else
        outerloop = StmtBlock(startloc)
    end
    # Concatenate back to a linear representation
    whole = StmtBlock(wholepre.codelocdefault)
    append!(whole, wholepre)
    append!(whole, destruct)
    append!(whole, outerloop)
    append!(whole, wholepost)
    # Fix up the numbering
    changemap = fill(typemin(Int), nextid)
    for i = 1:length(whole.ssavalueids)
        id = whole.ssavalueids[i]
        changemap[id] = i - id
    end
    changemap[bodystop+1] = changemap[innerid] + innerid - (bodystop+1) # inner `iterate(iter, state)`
    remap_ir_elements!(whole.stmts, changemap)
    src.code = whole.stmts
    src.codelocs = whole.codelocs
    src.ssavaluetypes = whole.ssavaluetypes
    return true
end

function expand_cartesian_loops!(src::CodeInfo, world)
    modified = false
    if isdefined(Main, :Base) && isdefined(Main.Base, :CartesianIndices)
        idxs = Int[]
        for i = 1:length(src.code)
            rhs = getrhs(src, i)
            if isexpr(rhs, :call) && length(rhs.args) == 2
                if isglobalref(rhs.args[1], Main.Base, :iterate)
                    iterator = rhs.args[2]
                    iteratortype = if isa(iterator, SSAValue)
                        src.ssavaluetypes[iterator.id]
                    elseif isa(iterator, SlotNumber) || isa(iterator, TypedSlot)
                        src.slottypes[iterator.id]
                    else
                        Any
                    end
                    if isa(iteratortype, Type) && iteratortype <: Main.Base.CartesianIndices
                        push!(idxs, i)
                    end
                end
            end
        end
        if !isempty(idxs)
            osrc = copy(src)
            for i = length(idxs):-1:1
                modified |= expand_cartesian_loop!(src, idxs[i], world)
            end
        end
    end
    return modified
end
