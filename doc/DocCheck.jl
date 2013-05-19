# Julia utilities for checking documentation

# This file contains a number of functions for checking julia documentation
#
# isdeprecated(v)         :  test if v is deprecated
# isdocumented(v)         :  true if v is documented
# undefined_exports(m)    :  returns a list of undefined exports in module m
# undocumented(m)         :  returns a list of undocumented exports in module m
# undocumented_by_file(m) :  returns a dictionary of undocumented exports, 
#                            with file, function, and line number information
# undocumented_rst(m)     :  produce a list of undocumented function suitable for
#                            pasting into github issue #2242

module DocCheck

import Base.Help: init_help, FUNCTION_DICT, MODULE_DICT, CATEGORY_DICT

export isdeprecated, isdocumented, undefined_exports, undocumented, undocumented_by_file, undocumented_rst

isdeprecated(m::Module, v) = try endswith(functionloc(eval(m, v))[1], "deprecated.jl") catch return false end
isdeprecated(v)            = try endswith(functionloc(eval(v))[1], "deprecated.jl") catch return false end

isdocumented(v) = (s=string(v); haskey(FUNCTION_DICT, s) || haskey(MODULE_DICT, s))


modfuncjoin(m::String, f::String) = beginswith(f, '@') ? "@$m.$(f[2:end])" : "$m.$f"
modfuncjoin(m, f) = modfuncjoin(string(m), string(f))

# return a list of undefined exports in a module
undefined_exports(m::Module) = sort(filter(x->!isdefined(x), names(m)))
undefined_exports() = undefined(Base)

# Check for exported names that aren't documented,
# and return a Dict with (fn::Symbol, fullname::String) pairs
function undocumented(m::Module)
    init_help()
    undoc = Dict{Symbol, Array}()
    for v in sort(names(m))
        if isdefined(m,v) && !isdocumented(v) && !isdeprecated(m,v)
            ms = modfuncjoin(m,v)
            haskey(undoc, v) ? push!(undoc[v], ms) : (undoc[v] = [ms])
        end
    end
    undoc
end
undocumented() = undocumented(Base)

# Check for exported names that aren't documented, and
# return the file, function names, and line numbers, if available
function undocumented_by_file(m::Module)
    init_help()
    undocf = Dict{String, Dict}()
    for (f,_) in undocumented(m)
        s = string(f)
        try
            for (file, line) in functionlocs(eval(f))
                if beginswith(file, JULIA_HOME)
                    file = replace(file, JULIA_HOME, "\$JULIA_HOME", 1)
                end
                if !haskey(undocf, file)
                    undocf[file] = Dict{String, Vector{Integer}}()
                end
                if !haskey(undocf[file], s)
                    undocf[file][s] = [line]
                else
                    push!(undocf[file][s], line)
                end
            end
        catch
            if !haskey(undocf, "UNKNOWN_FILE")
                undocf["UNKNOWN_FILE"] = Dict{String, Vector{Integer}}()
            end
            undocf["UNKNOWN_FILE"][s] = Integer[-1]
        end
    end
    undocf
end
undocumented_by_file() = undocumented_by_file(Base)


# Unlike the above functions, this version parses base/exports.jl,
# because that file groups the functions in a more systematic manner.
# The output can be pasted into https://github.com/JuliaLang/julia/issues/2242
# This also only works with Base functions; the other "undocumented*"
# functions are more general.

# Based on code by @jihao
function _undocumented_rst()
    init_help()
    depdoc = havecount = total = 0
    out = String["The following exports are not documented:"]
    undoc_exports = Set()
    exports=[strip(x) for x in split(replace(open(readall, "$JULIA_HOME/../../base/exports.jl"),",",""),"\n")]
    for line in exports
        if search(line, "deprecated")!=0:-1; continue end
        if haskey(MODULE_DICT, line); havecount+=1; total+=1; continue end
        if length(line)>1
            if line[1]=='#'
               if line[2]!= ' ' continue end
            else
               s = symbol(line) # for submodules: string(:Sort) == "Base.Sort"
               if !isdefined(s) continue end
               if haskey(FUNCTION_DICT, line) || haskey(MODULE_DICT, line) || haskey(CATEGORY_DICT, string(s))
                  m = getkey(MODULE_DICT, line, "")
                  isdeprecated(m,s) && continue
                  havecount+=1; total+=1; continue
               end
               add!(undoc_exports, line)
               if line[1]=='@'; line = line[2:end] end
               line=string("- [ ] ", line)
               total+=1
            end
        end
        push!(out, line)
    end

    append!(out, String["", "Documented and deprecated functions/exports (please update docs)", ""])

    deprecated=[strip(x) for x in split(replace(open(readall, "$JULIA_HOME/../../base/deprecated.jl"),",",""),"\n")]
    for line in deprecated
        if beginswith(line, "@deprecated")
            fn = split(line, r" +")[2]
            if haskey(MODULE_DICT, fn); push!(out, string("- [ ] ", fn)); depdoc += 1 end
        elseif beginswith(line, "export")
            for fn in split(line, r"[ ,]+")[2:end]
                if haskey(MODULE_DICT, fn); push!(out, string("- [ ]", fn)); depdoc += 1 end
            end
        end
    end
    prepend!(out, String["$havecount/$total exports have been documented",
                         "(Additionally, $depdoc deprecated functions are still documentated)",
                         ""])
    (join(out, "\n"), undoc_exports)
end

undocumented_rst() = println(_undocumented_rst()[1])

end
