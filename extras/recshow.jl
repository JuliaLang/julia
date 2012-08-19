# recshow.jl: show for self-referential and DAG structured objects

# ---- ObjNode: capture of an object's output to show() -----------------------

type ObjNode
    # actual capture
    obj
    reused::Bool
    items::Vector   # ObjNode:s and strings/chars

    # scratch space for recshow etc
    strlen::Integer
    name::String

    ObjNode(obj) = new(obj, false, {}, -1, "")
end
emit(dest::ObjNode, arg) = (push(dest.items, arg); nothing)

function print(io::IO, node::ObjNode)
    if node.reused print(io, node.name)
    else           print(io, node.items...)
    end
end

get_strlen(node::ObjNode) = node.strlen
get_strlen(c::Char) = 1
get_strlen(s::String) = strlen(s)
get_strlen(x) = -1

function finish!(node::ObjNode)
    lengths = [get_strlen(item) for item in node.items]
    if !any(lengths .== -1) node.strlen = sum(lengths) end
end


# ---- RecordIO: IO that recursively captures the output of show() ------------

type RecordIO <: IO
    shows::ObjectIdDict  # Shows that have started capture so far
    dest::ObjNode        # Currently capturing
end
emit(io::RecordIO, arg) = emit(io.dest, arg)

## Stuff needed to make an IO subtype: ##

# Redirect character output to one place: emit
print(io::RecordIO, s::ASCIIString)   = emit(io, s)
print(io::RecordIO, s::ASCIIString)   = emit(io, s)
print(io::RecordIO, s::UTF8String)    = emit(io, s)
print(io::RecordIO, s::RopeString)    = emit(io, s)
print(io::RecordIO, s::String)        = emit(io, s)
print(io::RecordIO, c::Char)          = emit(io, c)

write(io::RecordIO, c::Char)          = emit(io, c)
write(io::RecordIO, s::ASCIIString)   = emit(io, s)

# Work around some types that do funky stuff in show
show(io::RecordIO, x::Float32) = print(io, repr(x))
show(io::RecordIO, x::Float64) = print(io, repr(x))
show(io::RecordIO, x::Symbol)  = print(io, string(x))

## Recording of show() ##

type RecordShowError <: Exception
    cause::Exception
end
function show(io::IO, e::RecordShowError)
    println(io, "Exception in recshow:"); show(io, e.cause)
end

function record_show!(shows::ObjectIdDict, dest::ObjNode)
    @assert !has(shows, dest.obj)
    @assert isempty(dest.items)
  
    shows[dest.obj] = dest
    try
        show(RecordIO(shows, dest), dest.obj)
    catch e
        if !isa(e, RecordShowError)
            emit(dest, "#encountered exception!")
            e = RecordShowError(e)
        end
        throw(e)
    end
    finish!(dest)
    nothing
end

function rshow(io::RecordIO, arg)
    if has(io.shows, arg)  # reuse old node
        node = io.shows[arg]
        node.reused = true
        emit(io, node)
    else                   # record new node
        node = ObjNode(arg)
        emit(io, node)
        record_show!(io.shows, node)
    end
end

record_show!(dest::ObjNode) = record_show!(ObjectIdDict(), dest)
record_show(arg) = (dest=ObjNode(arg); record_show!(dest); dest)


# ---- list_trees!: Prepare recshow print list from ObjNode:s -----------------

# is x immutable up to where show() calls rshow()?
is_immutable_to_rshow(x::Union(Number,Function,Type,TypeName,Symbol)) = true
is_immutable_to_rshow(x) = false

function treeify!(trees::Vector{ObjNode}, node::ObjNode)
    if !node.reused ||
      (is_immutable_to_rshow(node.obj) && (0 <= node.strlen <= 11))
        # node will be printed inline
        node.reused = false
        treeify_node!(trees, node)
    else
        if (node.name != "") return end
        # First encounter: name the node, add it to the print list
        push(trees, node)
        k = length(trees)
        node.name = "<x$k>"
    end
end
treeify!(trees::Vector{ObjNode}, x) = nothing
function treeify_node!(trees::Vector{ObjNode}, node::ObjNode)
    for item in node.items; treeify!(trees, item); end
end

function list_trees!(args...)
    trees = ObjNode[]
    for arg in args; treeify!(trees, arg); end
    k = 1
    while k <= length(trees); treeify_node!(trees, trees[k]); k += 1; end 
    trees
end


# ---- recshow: Show a possibly self-referential object -----------------------

recshow(io::RecordIO, arg) = rshow(io, arg)
function recshow(io::IO, arg)
    node = ObjNode(arg)
    try
        record_show!(node)    
    catch e
        print_recshow(io, node)
        throw(e)
    end
    print_recshow(io, node)
end
recshow(arg) = recshow(OUTPUT_STREAM, arg)

function print_recshow(io::IO, node::ObjNode)
    trees = list_trees!(node)
    if isempty(trees); print(io, node); return; end

    node.name = "<obj>"
    if !is(trees[1], node); enqueue(trees, node); end
    for node in trees; println(io, node.name, "\t= ", node.items...); end
end
