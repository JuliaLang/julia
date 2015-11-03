# This file is a part of Julia. License is MIT: http://julialang.org/license

abstract AbstractChannel{T}

type Channel{T} <: AbstractChannel{T}
    cond_take::Condition    # waiting for data to become available
    cond_put::Condition     # waiting for a writeable slot
    state::Symbol

    data::Array{T,1}
    szp1::Int               # current channel size plus one
    sz_max::Int             # maximum size of channel
    take_pos::Int           # read position
    put_pos::Int            # write position

    function Channel(sz)
        sz_max = sz == typemax(Int) ? typemax(Int) - 1 : sz
        szp1 = sz > 32 ? 33 : sz+1
        new(Condition(), Condition(), :open,
            Array(T, szp1), szp1, sz_max, 1, 1)
    end
end

const DEF_CHANNEL_SZ=32

Channel(sz::Int = DEF_CHANNEL_SZ) = Channel{Any}(sz)

closed_exception() = InvalidStateException("Channel is closed.", :closed)
function close(c::Channel)
    c.state = :closed
    notify_error(c::Channel, closed_exception())
    c
end
isopen(c::Channel) = (c.state == :open)

type InvalidStateException <: Exception
    msg::AbstractString
    state::Symbol
end

function put!(c::Channel, v)
    !isopen(c) && throw(closed_exception())
    d = c.take_pos - c.put_pos
    if (d == 1) || (d == -(c.szp1-1))
        # grow the channel if possible
        if (c.szp1 - 1) < c.sz_max
            if ((c.szp1-1) * 2) > c.sz_max
                c.szp1 = c.sz_max + 1
            else
                c.szp1 = ((c.szp1-1) * 2) + 1
            end
            newdata = Array(eltype(c), c.szp1)
            if c.put_pos > c.take_pos
                copy!(newdata, 1, c.data, c.take_pos, (c.put_pos - c.take_pos))
                c.put_pos = c.put_pos - c.take_pos + 1
            else
                len_first_part = length(c.data) - c.take_pos + 1
                copy!(newdata, 1, c.data, c.take_pos, len_first_part)
                copy!(newdata, len_first_part+1, c.data, 1, c.put_pos-1)
                c.put_pos = len_first_part + c.put_pos
            end
            c.take_pos = 1
            c.data = newdata
        else
            wait(c.cond_put)
        end
    end

    c.data[c.put_pos] = v
    c.put_pos = (c.put_pos == c.szp1 ? 1 : c.put_pos + 1)
    notify(c.cond_take, nothing, true, false)  # notify all, since some of the waiters may be on a "fetch" call.
    v
end

push!(c::Channel, v) = put!(c, v)

function fetch(c::Channel)
    wait(c)
    c.data[c.take_pos]
end

function take!(c::Channel)
    !isopen(c) && !isready(c) && throw(closed_exception())
    wait(c)
    v = c.data[c.take_pos]
    c.take_pos = (c.take_pos == c.szp1 ? 1 : c.take_pos + 1)
    notify(c.cond_put, nothing, false, false) # notify only one, since only one slot has become available for a put!.
    v
end

shift!(c::Channel) = take!(c)

isready(c::Channel) = (c.take_pos == c.put_pos ? false : true)

function isready_put(c::Channel)
    d = c.take_pos - c.put_pos
    if (d == 1) || (d == -(c.szp1-1))
        if (c.szp1 - 1) ≥ c.sz_max
            return false
        end
    end
    return true
end

function wait(c::Channel)
    while !isready(c)
        wait(c.cond_take)
    end
    nothing
end

function wait_put(c::Channel)
    while !isready_put(c)
        wait(c.cond_put)
    end
end

function notify_error(c::Channel, err)
    notify_error(c.cond_take, err)
    notify_error(c.cond_put, err)
end

eltype{T}(::Type{Channel{T}}) = T

function n_avail(c::Channel)
    if c.put_pos >= c.take_pos
        return c.put_pos - c.take_pos
    else
        return c.szp1 - c.take_pos + c.put_pos
    end
end

show(io::IO, c::Channel) = print(io, "$(typeof(c))(sz_max:$(c.sz_max),sz_curr:$(n_avail(c)))")

start{T}(c::Channel{T}) = Ref{Nullable{T}}()
function done(c::Channel, state::Ref)
    try
        # we are waiting either for more data or channel to be closed
        state[] = take!(c)
        return false
    catch e
        if isa(e, InvalidStateException) && e.state==:closed
            return true
        else
            rethrow(e)
        end
    end
end
next{T}(c::Channel{T}, state) = (v=get(state[]); state[]=nothing; (v, state))

## Implementation of 'select' mechanism to block on the disjunction of
## of 'waitable' objects.

@enum SelectClauseKind SelectPut SelectTake SelectDefault

# Represents a single parsed select "clause" of a @select macro call.
# eg, the (channel |> value) part of
# @select if channel |> value
#    println(value)
# ...
# end
immutable SelectClause{ChannelT, ValueT}
    kind::SelectClauseKind
    channel::Nullable{ChannelT}
    value::Nullable{ValueT}
end

const select_take_symbol = :|>
const select_put_symbol = :<|

#  A 'structured' select clause is one of the form "channel|>val" or
#  "channel<|val". All other clauses are considered "non-structured", meaning
#  the entire clause is assumed to be an expression that evaluates to a
#  conditional to which "_take!" will be applied.
is_structured_select_clause(clause::Expr) =
    clause.head == :call &&
    length(clause.args) == 3 &&
    clause.args[1] ∈ (select_take_symbol, select_put_symbol)

is_structured_select_clause(clause) = false

function parse_select_clause(clause)
    if is_structured_select_clause(clause)
        if clause.args[1] == select_take_symbol
            SelectClause(SelectTake, Nullable(clause.args[2]), Nullable(clause.args[3]))
        elseif clause.args[1] == select_put_symbol
            SelectClause(SelectPut, Nullable(clause.args[2]), Nullable(clause.args[3]))
        end
    else
        # Assume this is a 'take' clause whose return value isn't wanted.
        # To simplify the rest of the code to not have to deal with this special case,
        # the return value is assigned to a throw-away gensym.
        SelectClause(SelectTake, Nullable(clause), Nullable(gensym()))
    end
end

"""
`@select`

A select expression of the form:

```julia
@select begin
     clause1 => body1
     clause2 => body2
     _       => default_body
    end
```

Wait for multiple clauses simultaneously using a pattern matching syntax, taking a different action depending on which clause is available first.

A clause has three possible forms:

1) `event |> value`

If `event` is an `AbstractChannel`, wait for a value to become available in the channel and assign `take!(event)` to `value`.
if `event` is a `Task`, wait for the task to complete and assign `value` the return value of the task.

2) `event |< value`

Only suppored for `AbstractChannel`s. Wait for the channel to capabity to store an element, and then call `put!(event, value)`.

3) `event`

Calls `wait` on `event`, discarding the return value. Usable on any "waitable" events", which include channels, tasks, `Condition` objects, and processes.

If a default branch is provided, `@select` will check arbitrary choose any event which is ready and execute its body, or will execute `default_body` if none of them are.

Otherise, `@select` blocks until at least one event is ready.

For example,

```julia
channel1 = Channel()
channel2 = Channel()
task = @task ...

result = @select begin
    channel1 |> value => begin
            info("Took from channel1")
            value
        end
    channel2 <| :test => info("Put :test into channel2")
    task              => info("task finished")
end
```
"""
macro select(expr)
    clauses = Tuple{SelectClause, Any}[]
    # @select can operate in blocking or nonblocking mode, determined by whether
    # an 'else' clause is present in the @select body (in which case it will be
    # nonblocking).
    mode = :blocking
    for se in expr.args
        # skip line nodes
        isa(se, Expr) || continue
        # grab all the pairs
        if se.head == :(=>)
            if se.args[1] != :_
                push!(clauses, (parse_select_clause(se.args[1]), se.args[2]))
            else
                # The defaule case (_). If present, the select
                # statement is considered non-blocking and will return this
                # section if none of the other conditions are immediately available.
                push!(clauses, (SelectClause(SelectDefault, Nullable(), Nullable()), se.args[2]))
                mode = :nonblocking
            end
        elseif se.head != :block && se.head != :line
            # if we run into an expression that is not a block. line or pair throw an error
            throw(ErrorException("Selection expressions must be Pairs. Found: $(se.head)"))
        end
    end
    if mode == :nonblocking
        _select_nonblock_macro(clauses)
    else
        _select_block_macro(clauses)
    end
end

# These defintions allow for any condition-like object to be used
# with select.

# @select if x |> value  ... will ultimately insert an expression value=_take!(x).
_take!(c::AbstractChannel) = take!(c)
_take!(x) = wait(x)

# @select if x <| value .... will ultimately inset value=put!(x), which currently
# is only meanginful for channels and so no underscore varirant is used here.

# These are used with the non-blocking variant of select, which will
# only work with channels and tasks. Arbitrary conditionals can't be supported
# since "wait" is level-triggered.
_isready(c::AbstractChannel) = isready(c)
_isready(t::Task) = istaskdone(t)

# helper function to place the default case in the proper position
function set_default_first!(clauses)
    default_pos = find(clauses) do x
        clause, body = x
        clause.kind == SelectDefault
    end
    l = length(default_pos)
    l == 0 && return # bail out if there is no default case
    if l  > 1
        d = [clauses[i] for i in default_pos]
        err_str = reduce(* , ["\n _ => $(string(b))" for (c, b) in d])
        throw(ErrorException("Select takes at most one default case. Found: $l $err_str"))
    end
    # swap elements to sure make SelectDefault comes first
    clauses[1], clauses[default_pos[1]] = clauses[default_pos[1]], clauses[1]
    clauses
end

function _select_nonblock_macro(clauses)
    set_default_first!(clauses)
    branches = Expr(:block)
    for (clause, body) in clauses
        branch =
        if clause.kind == SelectPut
            :(if isready_put($(clause.channel|>get|>esc))
                put!($(clause.channel|>get|>esc), $(clause.value|>get|>esc))
                $(esc(body))
            end)
        elseif clause.kind == SelectTake
            :(if _isready($(clause.channel|>get|>esc))
                $(clause.value|>get|>esc) = _take!($(clause.channel|>get|>esc))
                $(esc(body))
            end)
        elseif clause.kind == SelectDefault
            :($(esc(body)))
        end

        # the next two lines build an if / elseif chain from the bottom up
        push!(branch.args, branches)
        branches = branch
    end
    :($branches)
end

# The strategy for blocking select statements is to create a set of "rival"
# tasks, one per condition. When a rival "wins" by having its conditional be
# the first available, it sends a special interrupt to its rivals to kill them.
# The interrupt includes the task where control should be resumed
# once the rival has shut itself down.
immutable SelectInterrupt <: Exception
    parent::Task
end

# Kill all tasks in "tasks" besides  a given task. Used for killing the rivals
# of the winning waiting task.
function select_kill_rivals(tasks, myidx)
    for (taskidx, task) in enumerate(tasks)
        taskidx == myidx && continue
        if task.state==:waiting
            # Rival is blocked waiting for its channel; send it a message that it's
            # lost the race.
            Base.throwto(task, SelectInterrupt(current_task()))
        elseif task.state==:queued
            # Rival hasn't starting running yet and so hasn't blocked or set up
            # a try-catch block to listen for SelectInterrupt.
            # Just delete it from the workqueue.
            queueidx = findfirst(Base.Workqueue.==task)
            deleteat!(Base.Workqueue, queueidx)
        end
    end
end

function _select_block_macro(clauses)
    branches = Expr(:block)
    body_branches = Expr(:block)
    for (i, (clause, body)) in enumerate(clauses)
        if clause.kind == SelectPut
            wait_for_channel = :(wait_put($(clause.channel|>get|>esc)))
            mutate_channel =  :(put!($(clause.channel|>get|>esc), $(clause.value|>get|>esc)))
            bind_variable = :(nothing)
        elseif clause.kind == SelectTake
            wait_for_channel =  :(wait($(clause.channel|>get|>esc)))
            mutate_channel =  :(_take!($(clause.channel|>get|>esc)))
            bind_variable = :($(clause.value|>get|>esc) = branch_val)
        end
        branch = quote
            tasks[$i] = @schedule begin
                try  # Listen for genuine errors to throw to the main task
                    try
                        # Listen for SelectInterrupt messages so we can shutdown
                        # if a rival's channel unblocks first.
                        $wait_for_channel
                    catch err
                        if isa(err, SelectInterrupt)
                            yieldto(err.parent)
                            return
                        else
                            rethrow()
                        end
                    end
                    select_kill_rivals(tasks, $i)
                    event_val = $mutate_channel
                    put!(winner_ch, ($i, event_val))
                catch err
                    Base.throwto(maintask, err)
                end
            end # if
        end # for
        push!(branches.args, branch)

        body_branch = :(if branch_id == $i; $bind_variable; $(esc(body)); end)
        # the next two lines build an if / elseif chain from the bottom up
        push!(body_branch.args, body_branches)
        body_branches = body_branch
    end
    quote
        winner_ch = Channel(1)
        tasks = Array(Task, $(length(clauses)))
        maintask = current_task()
        $branches # set up competing tasks
        (branch_id, branch_val) = take!(winner_ch) # get the id of the winning task
        $body_branches # execute the winning block in the original lexical context
    end
end

# The following methods are the functional (as opposed to macro) forms of
# the select statement.
function _select_nonblock(clauses)
    for (i, clause) in enumerate(clauses)
        if clause[1] == :put
            if isready_put(clause[2])
                return (i, put!(clause[2], clause[3]))
            end
        elseif clause[1] == :take
            if _isready(clause[2])
                return (i, _take!(clause[2]))
            end
        else
            error("Invalid select clause: $clause")
        end
    end
    return (0, nothing)
end

function _select_block(clauses)
    winner_ch = Channel{Tuple{Int, Any}}(1)
    tasks = Array(Task, length(clauses))
    maintask = current_task()
    for (i, clause) in enumerate(clauses)
        tasks[i] = @async begin
            try
                try
                    if clause[1] == :put
                        wait_put(clause[2])
                    elseif clause[1] ==  :take
                        wait(clause[2])
                    end
                catch err
                    if isa(err, SelectInterrupt)
                        yieldto(err.parent)
                        return
                    else
                        rethrow()
                    end
                end
                select_kill_rivals(tasks, i)
                if clause[1] == :put
                    ret = put!(clause[2], clause[3])
                elseif clause[1] == :take
                    ret = _take!(clause[2])
                end
                put!(winner_ch, (i, ret))
            catch err
                throwto(maintask, err)
            end
        end
    end
    take!(winner_ch)
end

"""
`select(clauses[, block=true]) -> (clause_index, clause_value)`

Functional form of the `@select` macro, intended to be used when the set of clauses is dynamic. In general, this method will be less performant than the macro variant.

Clauses are specified as an array of tuples. Each tuple is expected to have 2 or 3 elements, as follows:

1) The clause type (`:take` or `:put`)
2) The waitable object
3) If the clause type is `:put`, the value to insert into the object.

If `block` is `true` (the default), wait for at least one clause to be satisfied and return a tuple whose first elmement is the index of the clause which unblocked first and whose whose second element is the value of the clause (see the manual on `select` for the meaning of clause value).

Otherwise, an arbitrary available clause will be executed, or a return value of `(0, nothing)` will be returned  immediately if no clause is available.
"""
function select(clauses, block=true)
    if block
        _select_block(clauses)
    else
        _select_nonblock(clauses)
    end
end
