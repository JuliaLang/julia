# This file is a part of Julia. License is MIT: https://julialang.org/license

"""
    TestSetNode

A node in a tree representing the structure of `@testset` calls found in test files.
Each node represents a `@testset` macro call and contains information about its
location and nested structure.
"""
mutable struct TestSetNode
    id::Int               # Unique identifier for this testset
    name::String           # Description of the testset
    file::String          # File where it's defined
    line::Int             # Line number
    children::Vector{TestSetNode}  # Nested testsets
    enabled::Bool          # Whether this testset is enabled for execution
end

TestSetNode(id::Int, name::String, file::String, line::Int) = TestSetNode(id, name, file, line, TestSetNode[], false)
TestSetNode(id::Int, name::String, file::String, line::Int, children::Vector{TestSetNode}) = TestSetNode(id, name, file, line, children, false)

# Constructor that generates deterministic ID from source location and name
TestSetNode(name::String, file::String, line::Int) = TestSetNode(generate_deterministic_testset_id(file, line, name), name, file, line, TestSetNode[], false)

function Base.show(io::IO, node::TestSetNode)
    enabled_str = node.enabled ? " [enabled]" : ""
    print(io, "TestSetNode(", node.id, ", \"", node.name, "\", \"", node.file, "\", ", node.line, enabled_str, ")")
end

"""
    show_tree(io::IO, testsets::Vector{TestSetNode}; enabled_only::Bool=false, show_ids::Bool=false)
    show_tree(testsets::Vector{TestSetNode}; enabled_only::Bool=false, show_ids::Bool=false)

Display a hierarchical tree view of testsets.

# Arguments
- `io::IO`: Output stream (defaults to stdout)
- `testsets::Vector{TestSetNode}`: The testsets to display
- `enabled_only::Bool=false`: If true, only show enabled testsets
- `show_ids::Bool=false`: If true, show testset IDs in brackets

# Examples
```julia
# Show all testsets
show_tree(testsets)

# Show only enabled testsets
show_tree(testsets, enabled_only=true)

# Show all testsets with IDs
show_tree(testsets, show_ids=true)
```
"""
function show_tree(io::IO, testsets::Vector{TestSetNode}, indent::String=""; enabled_only::Bool=false, show_ids::Bool=false)
    # Filter testsets if enabled_only is true
    display_testsets = enabled_only ? filter(ts -> ts.enabled, testsets) : testsets

    for (i, testset) in enumerate(display_testsets)
        is_last = (i == length(display_testsets))

        # Add enabled indicator (only show if not filtering to enabled_only)
        enabled_indicator = (!enabled_only && testset.enabled) ? " ✓" : ""

        # Print current node - no tree symbols for top level (indent == "")
        if indent == ""
            # Top level - no tree symbols
            if show_ids
                print(io, "[", testset.id, "] ")
            end
            if testset.enabled
                print(io, "\e[32m\"", testset.name, "\"\e[0m")  # Green for enabled testsets
            else
                print(io, "\"", testset.name, "\"")
            end
            print(io, "\e[90m @ ", basename(testset.file), ":", testset.line, "\e[0m")  # File location in light black
            println(io, enabled_indicator)
        else
            # Nested levels - show tree symbols in light black
            tree_symbol = is_last ? "└─ " : "├─ "
            print(io, indent, "\e[90m", tree_symbol, "\e[0m")  # \e[90m = light black, \e[0m = reset
            if show_ids
                print(io, "[", testset.id, "] ")
            end
            if testset.enabled
                print(io, "\e[32m\"", testset.name, "\"\e[0m")  # Green for enabled testsets
            else
                print(io, "\"", testset.name, "\"")
            end
            print(io, "\e[90m @ ", basename(testset.file), ":", testset.line, "\e[0m")  # File location in light black
            println(io, enabled_indicator)
        end

        # Print children with appropriate indentation
        children_to_show = enabled_only ? filter(child -> child.enabled, testset.children) : testset.children
        if !isempty(children_to_show)
            if indent == ""
                # First level of nesting - start with double space
                child_indent = "  "
            else
                # Deeper nesting continues with tree symbols and proper spacing
                child_indent = indent * (is_last ? "  " : "\e[90m│\e[0m ")
            end
            show_tree(io, testset.children, child_indent; enabled_only=enabled_only, show_ids=show_ids)
        end
    end
end

show_tree(testsets::Vector{TestSetNode}; enabled_only::Bool=false, show_ids::Bool=false) = show_tree(stdout, testsets, ""; enabled_only=enabled_only, show_ids=show_ids)

"""
    TestsetFilter

A structure containing information about which testsets should be enabled during test execution.
This is used to communicate testset filtering information from the discovery phase to the execution phase.
"""
struct TestsetFilter
    enabled_patterns::Vector{Union{String,Regex}} # Original patterns used for filtering
    match_type::Symbol                           # Match type (always :simplified now)
    enabled_testset_ids::Set{Int}                # Specific testset IDs that should run
    enabled_testset_names::Set{String}           # Specific testset names for backwards compatibility
    id_to_name_map::Dict{Int, String}           # Map from ID to name for debugging
end

# Global counter for assigning unique testset IDs (used only for discovery)
const TESTSET_ID_COUNTER = Ref{Int}(0)

"""
    reset_testset_id_counter()

Reset the testset ID counter to 0. Useful for testing or when starting fresh discovery.
"""
function reset_testset_id_counter()
    TESTSET_ID_COUNTER[] = 0
end

"""
    generate_deterministic_testset_id(file::String, line::Int, name::String)

Generate a deterministic testset ID based on source location and name.
This ensures the same testset always gets the same ID in both discovery and execution phases.

# Arguments
- `file::String`: The file where the testset is defined
- `line::Int`: The line number where the testset is defined
- `name::String`: The name/description of the testset

# Returns
A deterministic integer ID for the testset.
"""
function generate_deterministic_testset_id(file::String, line::Int, name::String)
    # Use basename for consistency across different path representations
    base_file = basename(file)

    # Create a stable hash from location and name
    # Using a simple but effective combination
    hash_input = string(base_file, ":", line, ":", name)

    # Convert to a positive integer ID in a safe range
    # Use modulo to keep it within a reasonable positive range
    raw_hash = hash(hash_input)
    return Int(abs(raw_hash % 0x7FFFFFFF)) + 1  # +1 to avoid ID 0, keep in positive Int range
end

"""
    create_testset_filter(testsets::Vector{TestSetNode}, patterns::Vector{Union{String,Regex}})

Create a TestsetFilter by applying the given patterns to the discovered testsets.

# Arguments
- `testsets::Vector{TestSetNode}`: The discovered testset tree
- `patterns::Vector{Union{String,Regex}}`: Patterns to match against testset names

# Returns
A `TestsetFilter` containing information about which testsets should be enabled.

Matching behavior:
- String patterns: Exact match (case-sensitive)
- Regex patterns: Use `occursin` for substring/pattern matching

# Examples
```julia
testsets = discover_testsets("test/runtests.jl")
filter = create_testset_filter(testsets, ["network", "http"])  # Exact matches
filter = create_testset_filter(testsets, [r"network.*test"])   # Regex matching
```
"""
function create_testset_filter(testsets::Vector{TestSetNode}, patterns::AbstractVector)
    # Reset all testsets to disabled
    disable_all_testsets!(testsets)

    # Apply each pattern
    total_enabled = 0
    for pattern in patterns
        enabled_count = enable_matching_testsets!(testsets, pattern)
        total_enabled += enabled_count
    end

    # Collect enabled testset IDs and names
    enabled_testset_ids = Set{Int}()
    enabled_testset_names = Set{String}()
    id_to_name_map = Dict{Int, String}()

    function collect_enabled(nodes::Vector{TestSetNode})
        for node in nodes
            if node.enabled
                push!(enabled_testset_ids, node.id)
                push!(enabled_testset_names, node.name)
                id_to_name_map[node.id] = node.name
            end
            collect_enabled(node.children)
        end
    end

    collect_enabled(testsets)

    return TestsetFilter(patterns, :simplified, enabled_testset_ids, enabled_testset_names, id_to_name_map)
end

# Global testset filter - loaded at module initialization
const TESTSET_FILTER = Ref{Union{TestsetFilter, Nothing}}(nothing)

"""
    should_run_testset(name::String)
    should_run_testset(id::Int)

Check if a testset with the given name or ID should be executed based on the current filter.
Returns `true` if no filter is active or if the testset is enabled by the filter.

# Arguments
- `name::String`: The name of the testset to check (for backward compatibility)
- `id::Int`: The unique ID of the testset to check (preferred method)

# Returns
`true` if the testset should run, `false` if it should be skipped.
"""
function should_run_testset(name::String)
    filter = TESTSET_FILTER[]
    filter === nothing && return true  # No filtering active
    return name in filter.enabled_testset_names
end

function should_run_testset(id::Int)
    filter = TESTSET_FILTER[]
    filter === nothing && return true  # No filtering active
    return id in filter.enabled_testset_ids
end

"""
    should_run_testset_by_id(id::Int)

Check if a testset should run by its unique ID.
This is the most efficient method when the ID is known.

# Arguments
- `id::Int`: The unique ID of the testset

# Returns
`true` if the testset should run, `false` if it should be skipped.
"""
function should_run_testset_by_id(id::Int)
    filter = TESTSET_FILTER[]
    filter === nothing && return true  # No filtering active
    return id in filter.enabled_testset_ids
end

"""
    enable_matching_testsets!(testsets::Vector{TestSetNode}, pattern::Union{String,Regex})

Enable testsets whose names match the given pattern, along with all their parent testsets
and all their descendant testsets.

# Arguments
- `testsets::Vector{TestSetNode}`: The testset tree to search through
- `pattern::Union{String,Regex}`: The pattern to match against testset names

# Returns
The number of testsets that were enabled.

Matching behavior:
- String patterns: Exact match (case-sensitive)
- Regex patterns: Use `occursin` for substring/pattern matching

When a testset matches the pattern:
- The testset itself is enabled
- All its parent testsets are enabled (to maintain execution context)
- All its descendant testsets are enabled (complete subtree execution)

# Examples
```julia
# Enable testsets with exact name "network" and all their children
enable_matching_testsets!(testsets, "network")

# Enable testsets matching a regex pattern and all their children
enable_matching_testsets!(testsets, r"test_.*_api")
```
"""
function enable_matching_testsets!(testsets::Vector{TestSetNode}, pattern)
    enabled_count = 0

    function enable_if_matches(node::TestSetNode, parent_path::Vector{TestSetNode})
        matches = false

        # Check if this node matches the pattern
        if pattern isa String
            # String: exact match (case-sensitive)
            matches = node.name == pattern
        elseif pattern isa Regex
            # Regex: use occursin for pattern matching
            matches = occursin(pattern, node.name)
        else
            error("Pattern must be a String or Regex, got $(typeof(pattern))")
        end

        # Recursively check children first
        child_matches = false
        for child in node.children
            if enable_if_matches(child, [parent_path; node])
                child_matches = true
            end
        end

        # If this node matches or any child matches, enable this node and all parents
        if matches || child_matches
            if !node.enabled
                # Enable this node
                node.enabled = true
                enabled_count += 1

                # Enable all parent nodes
                for parent in parent_path
                    if !parent.enabled
                        parent.enabled = true
                        enabled_count += 1
                    end
                end
            end

            # If this node directly matches (not just because of children), enable all its descendants
            if matches
                enabled_count += _enable_all_descendants(node)
            end

            return true
        end

        return false
    end

    # Process all top-level testsets
    for testset in testsets
        enable_if_matches(testset, TestSetNode[])
    end

    return enabled_count
end

"""
    _enable_all_descendants(node::TestSetNode)

Enable all descendant testsets of the given node recursively.

# Returns
The number of testsets that were enabled.
"""
function _enable_all_descendants(node::TestSetNode)
    enabled_count = 0

    function enable_recursive(testset::TestSetNode)
        if !testset.enabled
            testset.enabled = true
            enabled_count += 1
        end
        for child in testset.children
            enable_recursive(child)
        end
    end

    # Enable all children (but not the node itself, which should already be enabled)
    for child in node.children
        enable_recursive(child)
    end

    return enabled_count
end

"""
    disable_all_testsets!(testsets::Vector{TestSetNode})

Disable all testsets in the tree.

# Returns
The number of testsets that were disabled.
"""
function disable_all_testsets!(testsets::Vector{TestSetNode})
    disabled_count = 0

    function disable_recursive(node::TestSetNode)
        if node.enabled
            node.enabled = false
            disabled_count += 1
        end
        for child in node.children
            disable_recursive(child)
        end
    end

    for testset in testsets
        disable_recursive(testset)
    end

    return disabled_count
end

function Base.show(io::IO, ::MIME"text/plain", node::TestSetNode; indent::Int=0)
    prefix = "  "^indent
    println(io, "[", node.id, "] \"", node.name, "\" at ", node.file, ":", node.line)
    for child in node.children
        show(io, MIME("text/plain"), child; indent=indent+1)
    end
end

"""
    discover_testsets(file::AbstractString; follow_includes::Bool=true)

Discover and return the tree structure of `@testset` macro calls in a test file.
This function parses the Julia source code without executing it and builds a tree
representation of all `@testset` declarations found.

# Arguments
- `file::AbstractString`: Path to the test file to analyze
- `follow_includes::Bool=true`: Whether to recursively analyze files loaded via `include()` calls

# Returns
A `Vector{TestSetNode}` representing the top-level testsets found in the file.
Each `TestSetNode` contains information about nested testsets in its `children` field.

The discovery uses JuliaSyntax to parse the file without executing any code,
making it safe to analyze test files without running them.
"""
function discover_testsets(file::AbstractString; follow_includes::Bool=true)
    # Reset the ID counter for each new discovery
    reset_testset_id_counter()
    visited_files = Set{String}()
    return _parse_file_for_testsets(file, follow_includes, visited_files)
end

function _parse_file_for_testsets(file::AbstractString, follow_includes::Bool, visited_files::Set{String})
    abs_file = abspath(file)

    # Avoid infinite recursion from circular includes
    abs_file in visited_files && return TestSetNode[]
    push!(visited_files, abs_file)

    # Check if file exists
    if !isfile(abs_file)
        @warn "File not found: $abs_file"
        return TestSetNode[]
    end

    testsets = TestSetNode[]
    variable_tracker = Dict{String, Any}()  # Track variable assignments

    try
        # Read and parse the file using JuliaSyntax
        content = read(abs_file, String)
        tree = Base.JuliaSyntax.parseall(Base.JuliaSyntax.SyntaxNode, content; filename=abs_file)

        # Walk the syntax tree to find @testset and include calls
        _walk_syntax_tree(tree, abs_file, testsets, follow_includes, visited_files, variable_tracker)

    catch e
        @warn "Failed to parse $abs_file: $e"
    end

    return testsets
end

function _walk_syntax_tree(node, file::String, testsets::Vector{TestSetNode},
                          follow_includes::Bool, visited_files::Set{String}, variable_tracker::Dict{String, Any})
    # Track variable assignments and const declarations
    if Base.JuliaSyntax.kind(node) == Base.JuliaSyntax.K"="
        _track_assignment(node, variable_tracker, file)
    elseif Base.JuliaSyntax.kind(node) == Base.JuliaSyntax.K"const"
        _track_const_declaration(node, variable_tracker, file)
    elseif Base.JuliaSyntax.kind(node) == Base.JuliaSyntax.K"macrocall"
        node_children = Base.JuliaSyntax.children(node)
        if node_children !== nothing
            children = collect(node_children)
            if length(children) >= 2
                macro_name = children[1]
                # Check if this is a @testset macro call
                if Base.JuliaSyntax.kind(macro_name) == Base.JuliaSyntax.K"MacroName" &&
                   string(macro_name) == "@testset"
                    _handle_testset_macro(node, file, testsets, follow_includes, visited_files, variable_tracker)
                    return  # Don't recurse into testset children here, handle them in _handle_testset_macro
                end
            end
        end
    elseif Base.JuliaSyntax.kind(node) == Base.JuliaSyntax.K"call"
        # Check for include() calls
        if follow_includes
            _handle_include_call(node, file, testsets, follow_includes, visited_files, variable_tracker)
        end
    elseif Base.JuliaSyntax.kind(node) == Base.JuliaSyntax.K"for"
        # Handle for loops that might contain include statements
        if follow_includes
            _handle_for_loop(node, file, testsets, follow_includes, visited_files, variable_tracker)
        end
    end

    # Recursively walk all children
    node_children = Base.JuliaSyntax.children(node)
    if node_children !== nothing
        for child in node_children
            _walk_syntax_tree(child, file, testsets, follow_includes, visited_files, variable_tracker)
        end
    end
end

function _handle_testset_macro(node, file::String, testsets::Vector{TestSetNode},
                              follow_includes::Bool, visited_files::Set{String}, variable_tracker::Dict{String, Any})
    node_children = Base.JuliaSyntax.children(node)
    if node_children === nothing
        return
    end
    children = collect(node_children)

    # Get the line number from the macro call
    line = _get_source_line(node)

    # Parse the testset description and body
    name, body, is_loop = _parse_testset_args(children[2:end], variable_tracker)

    if is_loop
        # Handle loop-based testsets by expanding them
        loop_testsets = _expand_loop_testsets(node, file, line, children[2:end], variable_tracker, follow_includes, visited_files)
        append!(testsets, loop_testsets)
    else
        # Create the testset node
        testset_node = TestSetNode(name, file, line)

        # If there's a body, recursively search for nested testsets
        if body !== nothing
            nested_testsets = TestSetNode[]
            _walk_syntax_tree(body, file, nested_testsets, follow_includes, visited_files, variable_tracker)
            append!(testset_node.children, nested_testsets)
        end

        push!(testsets, testset_node)
    end
end

function _handle_include_call(node, file::String, testsets::Vector{TestSetNode},
                             follow_includes::Bool, visited_files::Set{String}, variable_tracker::Dict{String, Any})
    node_children = Base.JuliaSyntax.children(node)
    if node_children === nothing
        return
    end
    children = collect(node_children)

    # Check if this is an include call
    if length(children) >= 2
        func_node = children[1]
        if Base.JuliaSyntax.kind(func_node) == Base.JuliaSyntax.K"Identifier" &&
           string(func_node) == "include"
            # Try to extract the filename argument
            if length(children) >= 2
                arg_node = children[2]

                # Handle both static and dynamic includes
                if Base.JuliaSyntax.kind(arg_node) == Base.JuliaSyntax.K"string"
                    # Static include: include("filename.jl")
                    filename = _extract_string_literal(arg_node)
                    if filename !== nothing && !startswith(filename, "Dynamic:")
                        _process_include_file(filename, file, testsets, follow_includes, visited_files)
                    end
                elseif Base.JuliaSyntax.kind(arg_node) == Base.JuliaSyntax.K"Identifier"
                    # Dynamic include: include(variable)
                    var_name = string(arg_node)
                    if haskey(variable_tracker, var_name)
                        value = variable_tracker[var_name]
                        if isa(value, Vector{String})
                            # Multiple files to include
                            for filename in value
                                _process_include_file(filename, file, testsets, follow_includes, visited_files)
                            end
                        elseif isa(value, String)
                            # Single file to include
                            _process_include_file(value, file, testsets, follow_includes, visited_files)
                        end
                    end
                elseif Base.JuliaSyntax.kind(arg_node) == Base.JuliaSyntax.K"call"
                    # Handle complex expressions like: include(file * ".jl")
                    call_result = _resolve_include_expression(arg_node, file, variable_tracker)
                    if call_result !== nothing
                        if isa(call_result, Vector{String})
                            for filename in call_result
                                _process_include_file(filename, file, testsets, follow_includes, visited_files)
                            end
                        elseif isa(call_result, String)
                            _process_include_file(call_result, file, testsets, follow_includes, visited_files)
                        end
                    end
                end
            end
        end
    end
end

function _handle_for_loop(node, file::String, testsets::Vector{TestSetNode},
                         follow_includes::Bool, visited_files::Set{String}, variable_tracker::Dict{String, Any})
    # Handle for loops that might contain include statements with loop variables

    node_children = Base.JuliaSyntax.children(node)
    if node_children === nothing
        return
    end
    children = collect(node_children)

    # Extract loop information
    if length(children) >= 1
        iteration_node = children[1]
        if Base.JuliaSyntax.kind(iteration_node) == Base.JuliaSyntax.K"iteration"
            iter_children = Base.JuliaSyntax.children(iteration_node)
            if iter_children !== nothing
                iter_parts = collect(iter_children)
                if length(iter_parts) >= 1
                    in_node = iter_parts[1]
                    if Base.JuliaSyntax.kind(in_node) == Base.JuliaSyntax.K"in"
                        in_children = Base.JuliaSyntax.children(in_node)
                        if in_children !== nothing
                            in_parts = collect(in_children)
                            if length(in_parts) >= 2
                                var_name = string(in_parts[1])
                                collection_node = in_parts[2]

                                # Try to resolve the collection
                                collection_values = nothing
                                if Base.JuliaSyntax.kind(collection_node) == Base.JuliaSyntax.K"call"
                                    # Handle readlines(...) calls
                                    collection_values = _resolve_collection_expression(collection_node, file, variable_tracker)
                                elseif Base.JuliaSyntax.kind(collection_node) == Base.JuliaSyntax.K"Identifier"
                                    collection_var = string(collection_node)
                                    if haskey(variable_tracker, collection_var)
                                        collection_values = variable_tracker[collection_var]
                                    end
                                else
                                    # Direct collection literal
                                    collection_values = _extract_literal_value(collection_node, file, variable_tracker)
                                end

                                if collection_values !== nothing && isa(collection_values, Vector{String})
                                    # Process the for loop body for each iteration
                                    if length(children) >= 2
                                        loop_body = children[2]
                                        for value in collection_values
                                            # Create a new variable tracker with the loop variable
                                            loop_tracker = copy(variable_tracker)
                                            loop_tracker[var_name] = value

                                            # Process the loop body with this context
                                            _walk_syntax_tree(loop_body, file, testsets, follow_includes, visited_files, loop_tracker)
                                        end
                                        return  # Don't process the body again in normal recursion
                                    end
                                end
                            end
                        end
                    end
                end
            end
        end
    end

    # If we couldn't resolve the loop, fall back to normal processing
    for child in children
        _walk_syntax_tree(child, file, testsets, follow_includes, visited_files, variable_tracker)
    end
end

function _resolve_collection_expression(collection_node, base_file::String, variable_tracker::Dict{String, Any}=Dict())
    # Resolve expressions that return collections, like readlines(...)

    if Base.JuliaSyntax.kind(collection_node) == Base.JuliaSyntax.K"call"
        coll_children = Base.JuliaSyntax.children(collection_node)
        if coll_children !== nothing
            children = collect(coll_children)
            if length(children) >= 1
                func_name = children[1]
                if Base.JuliaSyntax.kind(func_name) == Base.JuliaSyntax.K"Identifier" && string(func_name) == "readlines"
                    if length(children) >= 2
                        readlines_arg = children[2]
                        resolved_path = _resolve_path_expression(readlines_arg, base_file, variable_tracker)
                        if resolved_path !== nothing && isfile(resolved_path)
                            try
                                return readlines(resolved_path)
                            catch
                                @debug "Failed to read file: $resolved_path"
                            end
                        end
                    end
                end
            end
        end
    end

    return nothing
end

function _process_include_file(filename::String, base_file::String, testsets::Vector{TestSetNode},
                              follow_includes::Bool, visited_files::Set{String})
    # Resolve relative path
    base_dir = dirname(base_file)
    include_file = isabspath(filename) ? filename : joinpath(base_dir, filename)

    # Recursively parse the included file
    included_testsets = _parse_file_for_testsets(include_file, follow_includes, visited_files)
    append!(testsets, included_testsets)
end

function _resolve_include_expression(expr_node, base_file::String, variable_tracker::Dict{String, Any})
    # Resolve complex include expressions like:
    # - include(file * ".jl")
    # - include(joinpath(@__DIR__, file))
    # - include(readlines(...))

    # Handle binary operations like file * ".jl"
    if Base.JuliaSyntax.kind(expr_node) == Base.JuliaSyntax.K"call"
        expr_children = Base.JuliaSyntax.children(expr_node)
        if expr_children !== nothing
            children = collect(expr_children)
            if length(children) >= 1
                func_name = children[1]

                # Handle infix operations like file * ".jl" (represented as call-i)
                if length(children) >= 3
                    left_arg = children[1]  # First operand
                    operator = children[2]  # Operator
                    right_arg = children[3] # Second operand

                    # Check for string concatenation
                    if Base.JuliaSyntax.kind(operator) == Base.JuliaSyntax.K"Identifier" && string(operator) == "*" &&
                       Base.JuliaSyntax.kind(left_arg) == Base.JuliaSyntax.K"Identifier" &&
                       Base.JuliaSyntax.kind(right_arg) == Base.JuliaSyntax.K"string"

                        var_name = string(left_arg)
                        suffix = _extract_string_literal(right_arg)

                        if haskey(variable_tracker, var_name) && suffix !== nothing
                            var_value = variable_tracker[var_name]
                            if isa(var_value, Vector{String})
                                return [v * suffix for v in var_value]
                            elseif isa(var_value, String)
                                return var_value * suffix
                            end
                        end
                    end
                end

                # Handle readlines function call
                if Base.JuliaSyntax.kind(func_name) == Base.JuliaSyntax.K"Identifier" && string(func_name) == "readlines"
                    if length(children) >= 2
                        readlines_arg = children[2]
                        resolved_path = _resolve_path_expression(readlines_arg, base_file, variable_tracker)
                        if resolved_path !== nothing && isfile(resolved_path)
                            try
                                return readlines(resolved_path)
                            catch
                                @debug "Failed to read file: $resolved_path"
                            end
                        end
                    end

                # Handle joinpath function call
                elseif Base.JuliaSyntax.kind(func_name) == Base.JuliaSyntax.K"Identifier" && string(func_name) == "joinpath"
                    path_parts = String[]
                    for i in 2:length(children)
                        part = _resolve_path_part(children[i], base_file, variable_tracker)
                        if part !== nothing
                            push!(path_parts, part)
                        end
                    end
                    if !isempty(path_parts)
                        return joinpath(path_parts...)
                    end
                end
            end
        end
    end

    return nothing
end

function _resolve_path_expression(path_node, base_file::String, variable_tracker::Dict{String, Any}=Dict())
    # Resolve path expressions like joinpath(@__DIR__, "testgroups")
    if Base.JuliaSyntax.kind(path_node) == Base.JuliaSyntax.K"call"
        path_children = Base.JuliaSyntax.children(path_node)
        if path_children !== nothing
            children = collect(path_children)
            if length(children) >= 1
                func_name = children[1]
                if Base.JuliaSyntax.kind(func_name) == Base.JuliaSyntax.K"Identifier" && string(func_name) == "joinpath"
                    path_parts = String[]
                    for i in 2:length(children)
                        part = _resolve_path_part(children[i], base_file, variable_tracker)
                        if part !== nothing
                            push!(path_parts, part)
                        end
                    end
                    if !isempty(path_parts)
                        return joinpath(path_parts...)
                    end
                end
            end
        end
    elseif Base.JuliaSyntax.kind(path_node) == Base.JuliaSyntax.K"string"
        return _extract_string_literal(path_node)
    end

    return nothing
end

function _resolve_path_part(part_node, base_file::String, variable_tracker::Dict{String, Any}=Dict())
    # Resolve individual parts of a path expression
    if Base.JuliaSyntax.kind(part_node) == Base.JuliaSyntax.K"string"
        return _extract_string_literal(part_node)
    elseif Base.JuliaSyntax.kind(part_node) == Base.JuliaSyntax.K"Identifier"
        # Look up variable in the tracker
        var_name = string(part_node)
        if haskey(variable_tracker, var_name)
            return variable_tracker[var_name]
        end
        return nothing
    elseif Base.JuliaSyntax.kind(part_node) == Base.JuliaSyntax.K"macrocall"
        # Handle @__DIR__ macro
        macro_children = Base.JuliaSyntax.children(part_node)
        if macro_children !== nothing
            children = collect(macro_children)
            if length(children) >= 1
                macro_name = children[1]
                if Base.JuliaSyntax.kind(macro_name) == Base.JuliaSyntax.K"MacroName" && string(macro_name) == "@__DIR__"
                    return dirname(base_file)
                end
            end
        end
    elseif Base.JuliaSyntax.kind(part_node) == Base.JuliaSyntax.K"."
        # Handle field access like Sys.BINDIR
        dot_children = Base.JuliaSyntax.children(part_node)
        if dot_children !== nothing
            children = collect(dot_children)
            if length(children) >= 2
                obj_name = string(children[1])
                field_name = string(children[2])

                if obj_name == "Sys" && field_name == "BINDIR"
                    # Return the current Julia binary directory
                    return Sys.BINDIR
                end
            end
        end
    elseif Base.JuliaSyntax.kind(part_node) == Base.JuliaSyntax.K"call"
        # Handle function calls like joinpath(...)
        call_children = Base.JuliaSyntax.children(part_node)
        if call_children !== nothing
            children = collect(call_children)
            if length(children) >= 1
                func_name = children[1]
                if Base.JuliaSyntax.kind(func_name) == Base.JuliaSyntax.K"Identifier" && string(func_name) == "joinpath"
                    # Resolve joinpath arguments
                    path_parts = String[]
                    for i in 2:length(children)
                        part = _resolve_path_part(children[i], base_file, variable_tracker)
                        if part !== nothing
                            push!(path_parts, string(part))
                        end
                    end
                    if !isempty(path_parts)
                        return joinpath(path_parts...)
                    end
                end
            end
        end
    end

    return nothing
end

function _parse_testset_args(args, variable_tracker::Dict{String, Any})
    name = "test set"  # default name
    body = nothing
    is_loop_testset = false

    for arg in args
        if Base.JuliaSyntax.kind(arg) == Base.JuliaSyntax.K"string"
            # String argument is the description
            name = _extract_string_literal(arg)
            if name === nothing
                name = "test set"
            end
        elseif Base.JuliaSyntax.kind(arg) == Base.JuliaSyntax.K"for"
            # This is a for-loop testset like @testset "$name" for var in collection
            body = arg
            is_loop_testset = true

            # Try to extract loop information for better naming
            if startswith(name, "Dynamic:")
                loop_info = _extract_loop_info(arg)
                if loop_info !== nothing
                    name = name * " " * loop_info
                end
            end
        elseif Base.JuliaSyntax.kind(arg) == Base.JuliaSyntax.K"block" ||
               Base.JuliaSyntax.kind(arg) == Base.JuliaSyntax.K"let" ||
               Base.JuliaSyntax.kind(arg) == Base.JuliaSyntax.K"call"
            # This is the test body
            body = arg
        elseif Base.JuliaSyntax.kind(arg) == Base.JuliaSyntax.K"Identifier" &&
               string(arg) == "begin"
            # Also treat bare 'begin' as start of body (this happens in macro parsing)
            # Look for the actual block in subsequent arguments
            continue
        end
    end

    return name, body, is_loop_testset
end

function _track_assignment(node, variable_tracker::Dict{String, Any}, base_file::String="")
    # Track simple variable assignments like: var_name = [...]
    node_children = Base.JuliaSyntax.children(node)
    if node_children === nothing
        return
    end

    children = collect(node_children)
    if length(children) >= 2
        lhs = children[1]
        rhs = children[2]

        # Only track simple identifier assignments
        if Base.JuliaSyntax.kind(lhs) == Base.JuliaSyntax.K"Identifier"
            var_name = string(lhs)
            value = _extract_literal_value(rhs, base_file, variable_tracker)
            if value !== nothing
                variable_tracker[var_name] = value
            end
        end
    end
end

function _track_const_declaration(node, variable_tracker::Dict{String, Any}, base_file::String="")
    # Track const declarations like: const VAR = value
    node_children = Base.JuliaSyntax.children(node)
    if node_children === nothing
        return
    end

    children = collect(node_children)
    if length(children) >= 1
        assignment_node = children[1]
        if Base.JuliaSyntax.kind(assignment_node) == Base.JuliaSyntax.K"="
            # This is a const assignment, delegate to normal assignment tracking
            _track_assignment(assignment_node, variable_tracker, base_file)
        end
    end
end

function _extract_literal_value(node, base_file::String="", variable_tracker::Dict{String, Any}=Dict())
    # Extract literal values from simple expressions
    if Base.JuliaSyntax.kind(node) == Base.JuliaSyntax.K"vect"
        # Array literal: [item1, item2, ...]
        result = String[]
        node_children = Base.JuliaSyntax.children(node)
        if node_children !== nothing
            for child in node_children
                if Base.JuliaSyntax.kind(child) == Base.JuliaSyntax.K"string"
                    str_val = _extract_string_literal(child)
                    if str_val !== nothing && !startswith(str_val, "Dynamic:")
                        push!(result, str_val)
                    end
                end
            end
        end
        return result
    elseif Base.JuliaSyntax.kind(node) == Base.JuliaSyntax.K"string"
        return _extract_string_literal(node)
    elseif Base.JuliaSyntax.kind(node) == Base.JuliaSyntax.K"call"
        # Handle function calls like joinpath(...)
        return _resolve_path_part(node, base_file, variable_tracker)
    end
    return nothing
end

function _expand_loop_testsets(node, file::String, line::Int, args, variable_tracker::Dict{String, Any},
                              follow_includes::Bool, visited_files::Set{String})
    # Try to expand loop-based testsets into individual testsets
    testsets = TestSetNode[]

    string_template = nothing
    loop_var = nothing
    collection_values = nothing
    body = nothing

    # Parse the arguments to find the string template and for loop
    for arg in args
        if Base.JuliaSyntax.kind(arg) == Base.JuliaSyntax.K"string"
            string_template = arg
        elseif Base.JuliaSyntax.kind(arg) == Base.JuliaSyntax.K"for"
            body = arg
            # Extract loop variable and collection
            loop_var, collection_values = _parse_for_loop(arg, variable_tracker, file)
        end
    end

    if string_template !== nothing && loop_var !== nothing && collection_values !== nothing
        # We can expand this loop!
        for value in collection_values
            # Create a testset name by substituting the loop variable
            expanded_name = _substitute_variable_in_string(string_template, loop_var, value)
            testset_node = TestSetNode(expanded_name, file, line)

            # If there's a body, recursively search for nested testsets
            if body !== nothing
                nested_testsets = TestSetNode[]

                # Create a modified variable tracker with the loop variable set to current value
                loop_tracker = copy(variable_tracker)
                loop_tracker[loop_var] = value

                _walk_syntax_tree_with_loop_context(body, file, nested_testsets, follow_includes, visited_files, loop_tracker, loop_var, value)
                append!(testset_node.children, nested_testsets)
            end

            push!(testsets, testset_node)
        end
    else
        # Fallback to the original dynamic representation
        name, body_fallback, _ = _parse_testset_args(args, variable_tracker)
        testset_node = TestSetNode(name, file, line)
        if body_fallback !== nothing
            nested_testsets = TestSetNode[]
            _walk_syntax_tree(body_fallback, file, nested_testsets, follow_includes, visited_files, variable_tracker)
            append!(testset_node.children, nested_testsets)
        end
        push!(testsets, testset_node)
    end

    return testsets
end

function _walk_syntax_tree_with_loop_context(node, file::String, testsets::Vector{TestSetNode},
                                           follow_includes::Bool, visited_files::Set{String},
                                           variable_tracker::Dict{String, Any}, loop_var::String, loop_value::String)
    # Special version of _walk_syntax_tree that handles dynamic includes within loops

    # Track variable assignments
    if Base.JuliaSyntax.kind(node) == Base.JuliaSyntax.K"="
        _track_assignment(node, variable_tracker)
    elseif Base.JuliaSyntax.kind(node) == Base.JuliaSyntax.K"macrocall"
        node_children = Base.JuliaSyntax.children(node)
        if node_children !== nothing
            children = collect(node_children)
            if length(children) >= 2
                macro_name = children[1]
                # Check if this is a @testset macro call
                if Base.JuliaSyntax.kind(macro_name) == Base.JuliaSyntax.K"MacroName" &&
                   string(macro_name) == "@testset"
                    _handle_testset_macro(node, file, testsets, follow_includes, visited_files, variable_tracker)
                    return  # Don't recurse into testset children here
                end
            end
        end
    elseif Base.JuliaSyntax.kind(node) == Base.JuliaSyntax.K"call"
        # Check for include() calls with special handling for loop variables
        if follow_includes
            _handle_include_call_with_loop_context(node, file, testsets, follow_includes, visited_files, variable_tracker, loop_var, loop_value)
        end
    end

    # Recursively walk all children
    node_children = Base.JuliaSyntax.children(node)
    if node_children !== nothing
        for child in node_children
            _walk_syntax_tree_with_loop_context(child, file, testsets, follow_includes, visited_files, variable_tracker, loop_var, loop_value)
        end
    end
end

function _handle_include_call_with_loop_context(node, file::String, testsets::Vector{TestSetNode},
                                              follow_includes::Bool, visited_files::Set{String},
                                              variable_tracker::Dict{String, Any}, loop_var::String, loop_value::String)
    node_children = Base.JuliaSyntax.children(node)
    if node_children === nothing
        return
    end
    children = collect(node_children)

    # Check if this is an include call
    if length(children) >= 2
        func_node = children[1]
        if Base.JuliaSyntax.kind(func_node) == Base.JuliaSyntax.K"Identifier" &&
           string(func_node) == "include"
            # Try to extract the filename argument
            if length(children) >= 2
                arg_node = children[2]

                # Handle both static and dynamic includes
                if Base.JuliaSyntax.kind(arg_node) == Base.JuliaSyntax.K"string"
                    # Static include: include("filename.jl")
                    filename = _extract_string_literal(arg_node)
                    if filename !== nothing && !startswith(filename, "Dynamic:")
                        _process_include_file(filename, file, testsets, follow_includes, visited_files)
                    end
                elseif Base.JuliaSyntax.kind(arg_node) == Base.JuliaSyntax.K"Identifier"
                    # Dynamic include: include(variable)
                    var_name = string(arg_node)

                    # Check if this is the loop variable
                    if var_name == loop_var
                        _process_include_file(loop_value, file, testsets, follow_includes, visited_files)
                    elseif haskey(variable_tracker, var_name)
                        value = variable_tracker[var_name]
                        if isa(value, Vector{String})
                            # Multiple files to include
                            for filename in value
                                _process_include_file(filename, file, testsets, follow_includes, visited_files)
                            end
                        elseif isa(value, String)
                            # Single file to include
                            _process_include_file(value, file, testsets, follow_includes, visited_files)
                        end
                    end
                end
            end
        end
    end
end

function _parse_for_loop(for_node, variable_tracker::Dict{String, Any}, base_file::String="")
    # Extract loop variable and collection from for node
    node_children = Base.JuliaSyntax.children(for_node)
    if node_children === nothing
        return nothing, nothing
    end

    children = collect(node_children)
    if length(children) >= 1
        iteration_node = children[1]
        if Base.JuliaSyntax.kind(iteration_node) == Base.JuliaSyntax.K"iteration"
            iter_children = Base.JuliaSyntax.children(iteration_node)
            if iter_children !== nothing
                iter_parts = collect(iter_children)
                if length(iter_parts) >= 1
                    in_node = iter_parts[1]
                    if Base.JuliaSyntax.kind(in_node) == Base.JuliaSyntax.K"in"
                        in_children = Base.JuliaSyntax.children(in_node)
                        if in_children !== nothing
                            in_parts = collect(in_children)
                            if length(in_parts) >= 2
                                var_name = string(in_parts[1])
                                collection_node = in_parts[2]

                                # Try to resolve the collection
                                if Base.JuliaSyntax.kind(collection_node) == Base.JuliaSyntax.K"Identifier"
                                    collection_var = string(collection_node)
                                    if haskey(variable_tracker, collection_var)
                                        return var_name, variable_tracker[collection_var]
                                    end
                                else
                                    # Direct collection literal
                                    collection_values = _extract_literal_value(collection_node, base_file, variable_tracker)
                                    if collection_values !== nothing
                                        return var_name, collection_values
                                    end
                                end
                            end
                        end
                    end
                end
            end
        end
    end
    return nothing, nothing
end

function _substitute_variable_in_string(string_node, var_name::String, value::String)
    # Substitute a variable in a string template
    node_children = Base.JuliaSyntax.children(string_node)
    if node_children === nothing
        return value
    end

    children = collect(node_children)

    # Simple case: just "$var"
    if length(children) == 1 && Base.JuliaSyntax.kind(children[1]) == Base.JuliaSyntax.K"Identifier" &&
       string(children[1]) == var_name
        return value
    end

    # More complex case: mixed string with interpolation
    result_parts = String[]
    for child in children
        if Base.JuliaSyntax.kind(child) == Base.JuliaSyntax.K"Identifier" && string(child) == var_name
            push!(result_parts, value)
        elseif Base.JuliaSyntax.kind(child) == Base.JuliaSyntax.K"String"
            # Static string part
            text = string(child)
            if length(text) >= 2 && text[1] == '"' && text[end] == '"'
                push!(result_parts, text[2:end-1])
            else
                push!(result_parts, text)
            end
        else
            push!(result_parts, "\$(...)")  # Complex expression we can't resolve
        end
    end

    return join(result_parts, "")
end

function _extract_loop_info(for_node)
    # Extract information about the for loop: "for var in collection"
    node_children = Base.JuliaSyntax.children(for_node)
    if node_children === nothing
        return nothing
    end

    children = collect(node_children)
    if length(children) >= 1
        iteration_node = children[1]
        if Base.JuliaSyntax.kind(iteration_node) == Base.JuliaSyntax.K"iteration"
            iter_children = Base.JuliaSyntax.children(iteration_node)
            if iter_children !== nothing
                iter_parts = collect(iter_children)
                if length(iter_parts) >= 1
                    in_node = iter_parts[1]
                    if Base.JuliaSyntax.kind(in_node) == Base.JuliaSyntax.K"in"
                        in_children = Base.JuliaSyntax.children(in_node)
                        if in_children !== nothing
                            in_parts = collect(in_children)
                            if length(in_parts) >= 2
                                var_name = string(in_parts[1])
                                collection_name = string(in_parts[2])
                                return "(for $(var_name) in $(collection_name))"
                            end
                        end
                    end
                end
            end
        end
    end
    return "(for loop)"
end

function _extract_string_literal(node)
    if Base.JuliaSyntax.kind(node) == Base.JuliaSyntax.K"string"
        # Check if this is a simple string literal or has interpolation
        node_children = Base.JuliaSyntax.children(node)
        if node_children === nothing
            return nothing
        end

        children = collect(node_children)

        # Case 1: Simple string literal with quoted content
        for child in children
            if Base.JuliaSyntax.kind(child) == Base.JuliaSyntax.K"String"
                # Remove the surrounding quotes
                text = string(child)
                if length(text) >= 2 && text[1] == '"' && text[end] == '"'
                    return text[2:end-1]
                end
                return text
            end
        end

        # Case 2: String interpolation - build a descriptive name
        if length(children) == 1 && Base.JuliaSyntax.kind(children[1]) == Base.JuliaSyntax.K"Identifier"
            # Simple case: "$variable"
            var_name = string(children[1])
            return "Dynamic: \$$(var_name)"
        elseif length(children) > 1
            # More complex interpolation
            parts = String[]
            for child in children
                if Base.JuliaSyntax.kind(child) == Base.JuliaSyntax.K"Identifier"
                    push!(parts, "\$$(string(child))")
                elseif Base.JuliaSyntax.kind(child) == Base.JuliaSyntax.K"String"
                    # Static string part
                    text = string(child)
                    if length(text) >= 2 && text[1] == '"' && text[end] == '"'
                        push!(parts, text[2:end-1])
                    else
                        push!(parts, text)
                    end
                else
                    push!(parts, "\$(...)")  # Complex expression
                end
            end
            return "Dynamic: " * join(parts, "")
        end
    end
    return nothing
end

function _get_source_line(node)
    try
        # Get source location information
        return Base.JuliaSyntax.source_line(node)
    catch
        return 1
    end
end
