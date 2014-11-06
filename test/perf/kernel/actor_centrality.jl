type Node
    name::UTF8String
    n::Set{Node}

    Node(name) = new(name, Set{Node}())
end

typealias Graph Dict{UTF8String, Node}

function get(G::Graph, name)
    if haskey(G, name)
        return G[name]
    end
    G[name] = Node(name)
end

function centrality_mean(G::Graph, start_node)
    dists = Dict{Node,UInt64}()
    next = Set([G[start_node]])

    cdist = 0
    while !isempty(next)
        nnext = Set{Node}()
        for n in next
            if !haskey(dists, n)
                dists[n] = cdist
                for neigh in n.n
                    push!(nnext, neigh)
                end
            end
        end
        cdist += 1
        next = nnext
    end
    mean([ v for (k,v) in dists ])
end

function read_graph()
    G = Graph()
    actors = Set()

    open(joinpath(JULIA_HOME,"..","..","test","perf","kernel","imdb-1.tsv"), "r") do io
        while !eof(io)
            k = split(strip(readline(io)), "\t")
            actor, movie = k[1], join(k[2:3], "_")
            ac, mn = get(G, actor), get(G, movie)
            push!(actors, actor)
            push!(ac.n, mn)
            push!(mn.n, ac)
        end
    end
    G, sort!([ a for a in actors])
end

function actor_centrality()
    G, actors = read_graph()
    d = Dict{UTF8String, Float64}()

    for a in actors[1:50]
        d[a] = centrality_mean(G, a)
        #print("$a: ", d[a], "\n")
    end

    vals = sort!([(v,k) for (k,v) in d])
    #for i=1:20
    #    print("$i: ", vals[i], "\n")
    #end

    # print(centrality_mean(G, "Hoffman, Dustin"), "\n")
end
