#########
# GRAPH #
#########
# Instantiate an undirected graph by calling ILGraph(edges),
# where edges is an array of tuples each with two or three elements:
# the first two elements are names of the endpoints;
# the third element is an optional one for the value of that edge
# Example:
# julia> ILGraph({(1,2,3),(2,3,5),(3,4)})
# Edge 1,2: 3
# Edge 2,3: 5
# Edge 3,4
# 
# ILGraph has two fields: edges and vertices, which are arrays containing
# all ILEdge and ILVertex objects in the graph.
# ILEdge has three fields: v1, v2, and value. By default, value is set to zero.
# v1 and v2 store pointers to ILVertex objects
# ILVertex has 3 fields: name, value, neighbors. By default, value is set to zero.
# neighbors is an array of pointers to all other ILVertex objects that this one is adjacent to.
# Currently, ILVertex's name and ILEdge/ILVertex's value fields aren't bound to
# any type. This may be later changed to only allow Int32 and Real fields, respectively.
#
# FUNCTIONS (that should be supported by all implementations of Graph):
# Here, G is a Graph argument, x,y are Vertex arguments, and e is an Edge argument.
# It might be best to eliminate the functions that take the Edge and Vertex objects
# as arguments, and the ones that give access to these objects, but I don't see
# the harm in keeping them in here for now.
#
# add_vertex(G, name[, value]): adds a vertex to graph G with given name(/value)
# add(G, name1, name2[, value]): adds an edge to graph G with endpoints of given
#                                names, which need not already exist in G
# add(G, x, y[, value]): adds an edge to graph G with given endpoints, which
#                        must already exist in G
# delete(G, name1, name2), delete(G, x, y): attempts to delete the edge with the
# two given endpoints; returns true if successful, false if the edge didn't exist,
# and an error if either of the vertices doesn't exist in the graph
# has_vertex(G, name), has_vertex(G, x): test if the given vertex is a member of the graph
# get_vertex(G, name): get the Vertex object with the given name
# neighbors(G, name), neighbors(G, x): return an array of Vertex objects that are
# adjacent to the given vertex
# adjacent(G, x, y), adjacent(G, name1, name2): returns boolean for if the given vertices
# are adjacent or not in the graph (returns an error if either vertex isn't in the graph)
# get_edge(G, name1, name2), get_edge(G, x, y): get the Edge objects with the given vertices
# get_node_value(G, name), get_node_value(G, x): return the value of given vertex
# set_node_value(G, name, v), set_node_value(G, x, v): set the value of given vertex to v
# get_edge_value(G, name1, name2), get_edge_value(G, x, y), get_edge_value(G, e):
# return value of given edge
# set_edge_value(G, name1, name2, v), set_edge_value(G, x, y, v), set_edge_value(G, e, v):
# set value of given edge
#
# There is also a BGraph class for bipartite graphs. This class was written by copy/pasting
# ILGraph and quickly changing things, so I can't actually guarantee that it works.
# For functions where it asks for two names/vertices, it implicitly assumes that the first
# argument belongs to side A and the second belongs to side B.
# For functions where it asks for one name, you must supply the side it belongs on
# with a boolean (true = A, false = B). This is done with either an extra argument immediately
# following name, or by replacing name with nameside, a tuple (name, side).
# Example: set_node_value(G, name, side, v) and set_node_value(G, nameside, v) are the
# corresponding functions for set_node_value(G, name, v)
#
# Currently, the only other thing to do with these Graphs is to run searches on them.
# search(G::Graph, start_test, end_test[[, move_test], bfs::Bool])
# start_test and end_test are functions that take a vertex and return a boolean
# representing if that vertex is a legal starting/ending spot -- they can also
# be replaced by a vertex object/name to represent only one legal spot
# move_test is a optional argument function taking two vertices and returning
# a boolean representing if the move from the first to the second is legal 
# bfs is a optional argument boolean defaulting to true; if true, the search
# is bfs, and otherwise it's dfs
#
# POSSIBLE TO-DOS:
#  * Add checks to BGraph functions (currently, it attempts to access nameside[1], nameside[2])
#    without checking for length.
#  * Use a better data structure than an array that regrows/shrinks with each addition/deletion.
#  * Implement a Graph type that uses incidence or adjacency matrices.
#  * Implement a type for directed graphs.
#  * Implement more algorithms, including Dijkstra's, max flow, (and maximal matchings?).


function index_of(A::Array, b)
    i = 1
    for a = A
        if isequal(a,b); return i; end
        i += 1
    end
    return -1
end

abstract Vertex
abstract Edge
abstract Graph

##IL = IncidenceList

type ILVertex <: Vertex
    name
    value
    neighbors::Array
    ILVertex(name, value) = new(name, value, {})
    ILVertex(name) = ILVertex(name, 0)
end

#print(v::ILVertex) = print("Vertex $(v.name): $(v.value)")
show(io,v::ILVertex) = (v.value == 0 ?
                        print(io, "Vertex $(v.name)") : 
                        print(io, "Vertex $(v.name): $(v.value)"))

type ILEdge <: Edge
    v1::ILVertex
    v2::ILVertex
    value
end

#print(e::ILEdge) = print("Edge: $(e.v1), $(e.v2): $(e.value)")
show(io, e::ILEdge) = (e.value == 0 ?
                       print(io,"Edge $(e.v1.name),$(e.v2.name)") :
                       print(io,"Edge $(e.v1.name),$(e.v2.name): $(e.value)"))

ILEdge(v1, v2) = ILEdge(v1, v2, 0)

function isequal(e1::ILEdge, e2::ILEdge)
    if !isequal(e1.value, e2.value); return false; end
    return (isequal(e1.v1,e2.v1) && isequal(e1.v2,e2.v2)) ||
           (isequal(e1.v2,e2.v1) && isequal(e1.v1,e2.v2))
end

type ILGraph <: Graph
    vertices::Array
    edges::Array
    function ILGraph()
       new({},{}) 
    end
end


#print(G::ILGraph) = (for edge = G.edges; println(edge); end) 
show(io, G::ILGraph) = (for edge = G.edges; println(io, edge); end) 

function ILGraph(edges)
    this = ILGraph()
    for edge = edges
        add_edge(this, edge)
    end
    this
end

function add_vertex(G::ILGraph, name, value)
    if has_vertex(G, name); error("add_vertex: graph already has a vertex with name=$name"); end
    v = ILVertex(name, value)
    push(G.vertices, v)
end
add_vertex(G::ILGraph, name) = add_vertex(G, name, 0)

#add the edge with vertices with names x, y, which don't need to be part of the graph yet
function add(G::ILGraph, name1, name2, value)
    if has_vertex(G, name1)
        x = get_vertex(G, name1)
    else
        x = ILVertex(name1)
        push(G.vertices, x)
    end

    if has_vertex(G, name2)
        y = get_vertex(G, name2)
    else
        y = ILVertex(name2)
        push(G.vertices, y)
    end

    e = ILEdge(x, y, value)
    push(G.edges, e)
    push(x.neighbors, y)
    push(y.neighbors, x)
end
add(G::ILGraph, name1, name2) = add(G, name1, name2, 0)

#add the edge with vertices x,y; x and y must already be vertices of the graph
function add(G::ILGraph, x::ILVertex, y::ILVertex, value)
    if !has_vertex(G, x); error("add: graph does not have $x"); end
    if !has_vertex(G, y); error("add: graph does not have $y"); end
    e = ILEdge(x, y, value)
    push(G.edges, e)
    push(x.neighbors, y)
    push(y.neighbors, x)
end
add(G::ILGraph, x::ILVertex, y::ILVertex) = add(G, x, y, 0)

#assumes edge is array/tuple of length 2 or 3
#first two arguments are names of vertices
#optional third argument is value
function add_edge(G::ILGraph, edge)
    if length(edge) ==  3
        add(G, edge[1], edge[2], edge[3])
    elseif length(edge) == 2
        add(G, edge[1], edge[2])
    else
        error("add_edge: $edge does not have 2 or 3 elements")
    end
end

#attempts to delete the edge with vertices given
#returns true if successful, returns false if no such edge exists
#returns an error if either name1 or name2 aren't vertices in the graph
function delete(G::ILGraph, name1, name2)
    x = get_vertex(G, name1)
    y = get_vertex(G, name2)
    delete(G, x, y)
end

#attempts to delete the edge with vertices given
#returns true if successful, returns false if no such edge exists
#returns error if either of the vertices aren't in the graph
function delete(G::ILGraph, x::ILVertex, y::ILVertex)
    if !has_vertex(G, x); error("delete: graph does not have $x"); end
    if !has_vertex(G, y); error("delete: graph does not have $y"); end
    
    if !adjacent(G, x, y); return false; end
    #TODO: delete x from y.neighbors, delete y from x.neighbors, delete edge x,y
    i = index_of(x.neighbors, y)
    del(x.neighbors, i)
    i = index_of(y.neighbors, x)
    del(y.neighbors, i)
    i = index_of(G.edges, get_edge(G, x, y))
    del(G.edges, i)
end

function has_vertex(G::ILGraph, name)
    for v = G.vertices
        if isequal(v.name, name); return true; end
    end
    return false
end

function has_vertex(G::ILGraph, v::ILVertex)
    contains(G.vertices, v)
end

function get_vertex(G::ILGraph, name)
    for v = G.vertices
        if isequal(v.name, name); return v; end
    end
    error("get_vertex: no vertex with name=$name")
end

function adjacent(G::ILGraph, name1, name2)
    x = get_vertex(G, name1)
    y = get_vertex(G, name2)
    return contains(x.neighbors, y)
end

function adjacent(G::ILGraph, x::ILVertex, y::ILVertex)
    if !has_vertex(G, x); error("adjacent: graph does not have $x"); end
    if !has_vertex(G, y); error("adjacent: graph does not have $y"); end
    return contains(x.neighbors, y)
end

function neighbors(G::ILGraph, name)
    x = get_vertex(G, name)
    x.neighbors
end

function neighbors(G::ILGraph, x::ILVertex)
    if has_vertex(G, x)
        x.neighbors
    else
        error("neighbors: graph does not have $x")
    end
end

function get_edge(G::ILGraph, name1, name2)
    if !adjacent(G, name1, name2)
        error("get_edge: graph doesn't have that edge")
    else
        x = get_vertex(G, name1)
        y = get_vertex(G, name2)
        e1 = ILEdge(x,y)
        for e2 =  G.edges
            if (isequal(e1.v1,e2.v1) && isequal(e1.v2,e2.v2)) || (isequal(e1.v2,e2.v1) && isequal(e1.v1,e2.v2))
                return e2
            end
        end
    end
    error("get_edge: couldn't find the edge")
end

function get_edge(G::ILGraph, x::ILVertex, y::ILVertex)
    if !adjacent(G, x, y)
        error("get_edge: graph doesn't have that edge")
    else
        e1 = ILEdge(x,y)
        for e2 =  G.edges
            if (isequal(e1.v1,e2.v1) && isequal(e1.v2,e2.v2)) || (isequal(e1.v2,e2.v1) && isequal(e1.v1,e2.v2))
                return e2
            end
        end
    end
    error("get_edge: couldn't find the edge")
end

function get_node_value(G::ILGraph, name)
    x = get_vertex(G, name)
    x.value
end

function get_node_value(G::ILGraph, x::ILVertex)
    if has_vertex(G, x)
        x.value
    else
        error("get_node_value: graph does not have $x")
    end
end

function set_node_value(G::ILGraph, name, value)
    x = get_vertex(G, name)
    x.value = value
end

function set_node_value(G::ILGraph, x::ILVertex, value)
    if has_vertex(G, x)
        x.value = value
    else
        error("set_node_value: graph does not have $x")
    end
end

function get_edge_value(G::ILGraph, name1, name2)
    x = get_vertex(G, name1)
    y = get_vertex(G, name2)
    e = get_edge(G, x, y)
    return e.value
end

function get_edge_value(G::ILGraph, e::ILEdge)
    x = e.v1
    y = e.v2
    if !has_vertex(G, x); error("get_edge_value: graph does not have $x"); end
    if !has_vertex(G, y); error("get_edge_value: graph does not have $y"); end
    e = get_edge(G, x, y)
    return e.value
end

function set_edge_value(G::ILGraph, name1, name2, value)
    x = get_vertex(G, name1)
    y = get_vertex(G, name2)
    e = get_edge(G, x, y)
    e.value = value
end

function set_edge_value(G::ILGraph, e::ILEdge, value)
    x = e.v1
    y = e.v2
    if !has_vertex(G, x); error("get_edge_value: graph does not have $x"); end
    if !has_vertex(G, y); error("get_edge_value: graph does not have $y"); end
    e = get_edge(G, x, y)
    e.value = value
end

##B = Bipartite
type BVertex <: Vertex
    name
    side::Bool #true if on left side, false if on right
    value
    neighbors::Array
    BVertex(name, side::Bool, value) = new(name, side, value, {})
    BVertex(name, side::Bool) = BVertex(name, side, 0)
end

sides(side::Bool) = (side ? "A" : "B")

#print(v::BVertex) = print("Vertex $(v.name): $(v.value)")
function show(io, v::BVertex)
    side = sides(v.side)
    if v.value == 0
        print(io, "Vertex $side/$(v.name)") 
    else
        print(io, "Vertex $side/$(v.name): $(v.value)")
    end
end

type BEdge <: Edge
    v1::BVertex
    v2::BVertex
    value
end

#print(e::BEdge) = print("Edge: $(e.v1), $(e.v2): $(e.value)")
show(io, e::BEdge) = (e.value == 0 ?
                      print(io,"Edge $(e.v1.name),$(e.v2.name)") :
                      print(io,"Edge $(e.v1.name),$(e.v2.name): $(e.value)"))

BEdge(v1, v2) = BEdge(v1, v2, 0)

#function isequal(e1::BEdge, e2::BEdge)
#    return isequal(e1.value, e2.value) && isequal(e1.v1,e2.v1)
#           && isequal(e1.v2,e2.v2)
#end

type BGraph <: Graph
    vertices::Array
    edges::Array
    function BGraph()
        new({},{}) 
    end
end

#print(G::BGraph) = (for edge = G.edges; println(edge); end) 
show(io, G::BGraph) = (for edge = G.edges; println(io,edge); end) 

function BGraph(edges)
    this = BGraph()
    for edge = edges
        add_edge(this, edge)
    end
    this
end

function add_vertex(G::BGraph, name, side::Bool, value)
    if has_vertex(G, name, side); error("add_vertex: graph already has a vertex with name=$name,side=$(sides(side))"); end
    v = BVertex(name, value, side)
    push(G.vertices, v)
end
add_vertex(G::BGraph, name, side::Bool) = add_vertex(G, name, side, 0)
add_vertex(G::BGraph, nameside) = add_vertex(G::BGraph, nameside[1], nameside[2])

#add the edge with vertices with names x, y, which don't need to be part of the graph yet
function add(G::BGraph, name1, name2, value)
    if has_vertex(G, name1, true)
        x = get_vertex(G, name1, true)
    else
        x = BVertex(name1, true)
        push(G.vertices, x)
    end

    if has_vertex(G, name2, false)
        y = get_vertex(G, name2, false)
    else
        y = BVertex(name2, false)
        push(G.vertices, y)
    end

    e = BEdge(x, y, value)
    push(G.edges, e)
    push(x.neighbors, y)
    push(y.neighbors, x)
end
add(G::BGraph, name1, name2) = add(G, name1, name2, 0)

#add the edge with vertices x,y; x and y must already be vertices of the graph
function add(G::BGraph, x::BVertex, y::BVertex, value)
    if !has_vertex(G, x); error("add: graph does not have vertex $x"); end
    if !has_vertex(G, y); error("add: graph does not have vertex $y"); end
    if x.side == y.side; error("add: vertices are on same side"); end
    e = BEdge(x, y, value)
    push(G.edges, e)
    push(x.neighbors, y)
    push(y.neighbors, x)
end
add(G::BGraph, x::BVertex, y::BVertex) = add(G, x, y, 0)

#assumes edge is array of length 2 or 3
#first two arguments are names of vertices
#optional third argument is value
function add_edge(G::BGraph, edge)
    if length(edge) ==  3
        add(G, edge[1], edge[2], edge[3])
    elseif length(edge) == 2
        add(G, edge[1], edge[2])
    else
        error("add_edge: $edge is not an array of 2 or 3 elements")
    end
end

#attempts to delete the edge with vertices given
#returns true if successful, returns false if no such edge exists
#returns an error if either name1 or name2 aren't vertices in the graph
function delete(G::BGraph, name1, name2)
    x = get_vertex(G, name1, true)
    y = get_vertex(G, name2, false)
    delete(G, x, y)
end

#attempts to delete the edge with vertices given
#returns true if successful, returns false if no such edge exists
#returns error if either of the vertices aren't in the graph
function delete(G::BGraph, x::BVertex, y::BVertex)
    if !has_vertex(G, x); error("delete: graph does not have $x"); end
    if !has_vertex(G, y); error("delete: graph does not have $y"); end
    
    if !adjacent(G, x, y); return false; end
    #TODO: delete x from y.neighbors, delete y from x.neighbors, delete edge x,y
    i = index_of(x.neighbors, y)
    del(x.neighbors, i)
    i = index_of(y.neighbors, x)
    del(y.neighbors, i)
    i = index_of(G.edges, get_edge(G, x, y))
    del(G.edges, i)
end

function has_vertex(G::BGraph, v::BVertex)
    contains(G.vertices, v)
end

function has_vertex(G::BGraph, name, side::Bool)
    for v = G.vertices
        if isequal(v.name, name) && v.side == side; return true; end
    end
    return false
end
has_vertex(G::BGraph, nameside) = has_vertex(G, nameside[1], nameside[2])

function get_vertex(G::BGraph, name, side)
    for v = G.vertices
        if isequal(v.name, name) && v.side == side; return v; end
    end
    error("get_vertex: no vertex with name=$name,side=$(sides(side))")
end
get_vertex(G::BGraph, nameside) = get_vertex(G, nameside[1], nameside[2])

function adjacent(G::BGraph, name1, name2)
    x = get_vertex(G, name1, true)
    y = get_vertex(G, name2, false)
    return contains(x.neighbors, y)
end

function adjacent(G::BGraph, x::BVertex, y::BVertex)
    if !has_vertex(G, x); error("adjacent: graph does not have $x"); end
    if !has_vertex(G, y); error("adjacent: graph does not have $y"); end
    return contains(x.neighbors, y)
end

function neighbors(G::BGraph, name, side::Bool)
    x = get_vertex(G, name, side)
    x.neighbors
end
neighbors(G::BGraph, nameside) = neighbors(G, nameside[1], nameside[2])

function neighbors(G::BGraph, x::BVertex)
    if has_vertex(G, x)
        x.neighbors
    else
        error("neighbors: graph does not have vertex $x")
    end
end

function get_edge(G::BGraph, name1, name2)
    if !adjacent(G, name1, name2)
        error("get_edge: graph doesn't have that edge")
    else
        x = get_vertex(G, name1)
        y = get_vertex(G, name2)
        e1 = BEdge(x,y)
        for e2 =  G.edges
            if (isequal(e1.v1,e2.v1) && isequal(e1.v2,e2.v2))
                return e2
            end
        end
    end
    error("get_edge: couldn't find the edge")
end

function get_edge(G::BGraph, x::BVertex, y::BVertex)
    if !adjacent(G, x, y)
        error("get_edge: graph doesn't have that edge")
    else
        e1 = BEdge(x,y)
        for e2 =  G.edges
            if (isequal(e1.v1,e2.v1) && isequal(e1.v2,e2.v2))
                return e2
            end
        end
    end
    error("get_edge: couldn't find the edge")
end

get_node_value(G::BGraph, nameside) =  get_node_value(G, nameside[1], nameside[2])
function get_node_value(G::BGraph, name, side::Bool)
    x = get_vertex(G, name, side)
    x.value
end

function get_node_value(G::BGraph, x::BVertex)
    if has_vertex(G, x)
        x.value
    else
        error("get_node_value: graph does not have $x")
    end
end

function set_node_value(G::BGraph, name, side::Bool, value)
    x = get_vertex(G, name, side)
    x.value = value
end
set_node_value(G::BGraph, nameside, value) = set_node_value(G, nameside[1], nameside[2], value)

function set_node_value(G::BGraph, x::BVertex, value)
    if has_vertex(G, x)
        x.value = value
    else
        error("set_node_value: graph does not have $x")
    end
end

function get_edge_value(G::BGraph, name1, name2)
    x = get_vertex(G, name1, true)
    y = get_vertex(G, name2, false)
    e = get_edge(G, x, y)
    return e.value
end

function get_edge_value(G::BGraph, e::BEdge)
    x = e.v1
    y = e.v2
    if !has_vertex(G, x); error("get_edge_value: graph does not have $x"); end
    if !has_vertex(G, y); error("get_edge_value: graph does not have $y"); end
    e = get_edge(G, x, y)
    return e.value
end

function set_edge_value(G::BGraph, name1, name2, value)
    x = get_vertex(G, name1, true)
    y = get_vertex(G, name2, false)
    e = get_edge(G, x, y)
    e.value = value
end

function set_edge_value(G::BGraph, e::BEdge, value)
    x = e.v1
    y = e.v2
    if !has_vertex(G, x); error("get_edge_value: graph does not have vertex $x"); end
    if !has_vertex(G, y); error("get_edge_value: graph does not have vertex $y"); end
    e = get_edge(G, x, y)
    e.value = value
end

## Various functions

#search
#start_test and end_test are functions that take in a vertex and return
#boolean representing if the search can start or end there
#or they can be Vertex objects/names of vertices
#(for BGraph, represent vertex as (name,side))
#move_test is a function that takes two vertices and returns boolean
#representing if that move is legal
#if bfs flag is false, does dfs instead
function search(G::Graph, start_test, end_test, move_test, bfs::Bool)
    #convert start_test and end_test to functions, if necessary
    if !isa(start_test, Function)
        if !isa(start_test, Vertex); start_test = get_vertex(G, start_test); end
        temp1 = start_test
        start_test = (v) -> (isequal(v, temp1))
    end
    if !isa(end_test, Function)
        if !isa(end_test, Vertex); end_test = get_vertex(G, end_test); end
        temp2 = end_test
        end_test = (v) -> (isequal(v, temp2))
    end
    paths = {}
    for v = G.vertices
        if start_test(v); push(paths,{v}); end
    end

    finalPath = {}
    while numel(paths) > 0
        flag = false
        path = pop(paths)
        lastPlace = path[numel(path)]
        for v = lastPlace.neighbors
            if contains(path, v); continue; end
            if move_test(lastPlace, v)
                path2 = copy(path)
                    newPath = push(path2, v)
                if end_test(v)
                    finalPath = newPath
                    flag = true
                    break;
                end
                if bfs
                    enqueue(paths, newPath)
                else
                    push(paths, newPath)
                end
            end
        end
        if flag; break; end
    end

    return finalPath
end
search(G::Graph, start_test, end_test, bfs::Bool) = search(G, start_test, end_test, (x,y) -> true, bfs)
search(G::Graph, start_test, end_test) = search(G, start_test, end_test, (x,y) -> true, true)

