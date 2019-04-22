using CSTParser, BenchmarkTools, LibGit2

suite = BenchmarkGroup()

suite["1arg kw"] = BenchmarkGroup()
suite["1arg kw"]["const"] = @benchmarkable CSTParser.parse_expression(ps) setup = (ps = ParseState("const x"))
suite["1arg kw"]["return"] = @benchmarkable CSTParser.parse_expression(ps) setup = (ps = ParseState("return x"))
suite["1arg kw"]["return (empty)"] = @benchmarkable CSTParser.parse_expression(ps) setup = (ps = ParseState("return"))

suite["datatypes"] = BenchmarkGroup()
suite["datatypes"]["abstract"] = @benchmarkable CSTParser.parse_expression(ps) setup = (ps = ParseState("abstract type T end"))
suite["datatypes"]["primitive"] = @benchmarkable CSTParser.parse_expression(ps) setup = (ps = ParseState("primitive type T end"))
suite["datatypes"]["struct, no sig"] = @benchmarkable CSTParser.parse_expression(ps) setup = (ps = ParseState("struct end"))
suite["datatypes"]["m struct, no sig"] = @benchmarkable CSTParser.parse_expression(ps) setup = (ps = ParseState("mutable struct end"))
suite["datatypes"]["struct"] = @benchmarkable CSTParser.parse_expression(ps) setup = (ps = ParseState("struct Name end"))
suite["datatypes"]["struct 5 args"] = @benchmarkable CSTParser.parse_expression(ps) setup = (ps = ParseState(
"""
struct Name
    f
    f
    f
    f
    f
end"""))
suite["datatypes"]["m struct"] = @benchmarkable CSTParser.parse_expression(ps) setup = (ps = ParseState("mutable struct Name end"))

suite["kw as id"] = BenchmarkGroup()
suite["kw as id"]["abstract"] = @benchmarkable CSTParser.parse_expression(ps) setup = (ps = ParseState("abstract"))
suite["kw as id"]["primitive"] = @benchmarkable CSTParser.parse_expression(ps) setup = (ps = ParseState("primitive"))

suite["import"] = BenchmarkGroup()
suite["import"]["single"] = @benchmarkable CSTParser.parse_expression(ps) setup = (ps = ParseState("import x"))
suite["import"]["5 arg"] = @benchmarkable CSTParser.parse_expression(ps) setup = (ps = ParseState("import x, x, x, x, x"))
suite["import"]["3 dots"] = @benchmarkable CSTParser.parse_expression(ps) setup = (ps = ParseState("import x.x.x.x"))
suite["import"]["colon"] = @benchmarkable CSTParser.parse_expression(ps) setup = (ps = ParseState("import x:x, x.x"))

suite["export"] = BenchmarkGroup()
suite["export"]["single"] = @benchmarkable CSTParser.parse_expression(ps) setup = (ps = ParseState("export x"))
suite["export"]["5 arg"] = @benchmarkable CSTParser.parse_expression(ps) setup = (ps = ParseState("export x, x, x, x, x"))

suite["block"] = BenchmarkGroup()
suite["block"]["begin"] = @benchmarkable CSTParser.parse_expression(ps) setup = (ps = ParseState("begin end"))
suite["block"]["end"] = @benchmarkable CSTParser.parse_expression(ps) setup = (ps = ParseState("quote end"))
suite["block"]["begin 5 arg"] = @benchmarkable CSTParser.parse_expression(ps) setup = (ps = ParseState(
"""
begin 
    var
    var
    var
    var
    var
end"""))

suite["kw function decl"] = BenchmarkGroup()
suite["kw function decl"]["no sig"] = @benchmarkable CSTParser.parse_expression(ps) setup = (ps = ParseState("function end"))
suite["kw function decl"]["sig, no call"] = @benchmarkable CSTParser.parse_expression(ps) setup = (ps = ParseState("function f end"))
suite["kw function decl"]["sig, no args"] = @benchmarkable CSTParser.parse_expression(ps) setup = (ps = ParseState("function f() end"))
suite["kw function decl"]["sig, no args, 2 params"] = @benchmarkable CSTParser.parse_expression(ps) setup = (ps = ParseState("function f() where T where T end"))

suite["for"] = BenchmarkGroup()
suite["for"]["1 iter"] = @benchmarkable CSTParser.parse_expression(ps) setup = (ps = ParseState("for i = I end"))
suite["for"]["2 iter"] = @benchmarkable CSTParser.parse_expression(ps) setup = (ps = ParseState("for i = I, j = J, k = K end"))

suite["while"] = BenchmarkGroup()
suite["while"]["simple"] = @benchmarkable CSTParser.parse_expression(ps) setup = (ps = ParseState("while cond end"))

suite["if"] = BenchmarkGroup()
suite["if"]["if"] = @benchmarkable CSTParser.parse_expression(ps) setup = (ps = ParseState("if cond end"))
suite["if"]["ifelse"] = @benchmarkable CSTParser.parse_expression(ps) setup = (ps = ParseState("""if cond
else
end"""))
suite["if"]["elseif *5"] = @benchmarkable CSTParser.parse_expression(ps) setup = (ps = ParseState("""if cond
elseif cond
elseif cond
elseif cond
elseif cond
elseif cond
else
end"""))

suite["let"] = BenchmarkGroup()
suite["let"]["1 arg"] = @benchmarkable CSTParser.parse_expression(ps) setup = (ps = ParseState("let x = 1 end"))
suite["let"]["3 arg"] = @benchmarkable CSTParser.parse_expression(ps) setup = (ps = ParseState("let x = 1, y = 2, z = 3 end"))

suite["try"] = BenchmarkGroup()
# suite["try"][""] = @benchmarkable CSTParser.parse_expression(ps) setup = (ps = ParseState("try end"))
suite["try"]["no cap"] = @benchmarkable CSTParser.parse_expression(ps) setup = (ps = ParseState("try catch end"))
suite["try"]["cap"] = @benchmarkable CSTParser.parse_expression(ps) setup = (ps = ParseState("try catch e end"))
suite["try"]["finally"] = @benchmarkable CSTParser.parse_expression(ps) setup = (ps = ParseState("try catch e finally end"))

suite["module"] = BenchmarkGroup()
suite["module"]["module"] = @benchmarkable CSTParser.parse_expression(ps) setup = (ps = ParseState("module Name end"))
suite["module"]["baremodule"] = @benchmarkable CSTParser.parse_expression(ps) setup = (ps = ParseState("baremodule Name end"))

suite["do"] = BenchmarkGroup()
suite["do"]["1 arg"] = @benchmarkable CSTParser.parse_expression(ps) setup = (ps = ParseState("""f() do x
end"""))

suite["ops"] = BenchmarkGroup()
suite["ops"]["assign * 5"] = @benchmarkable CSTParser.parse_expression(ps) setup = (ps = ParseState("""x=x=x=x=x"""))
suite["ops"]["comp * 5"] = @benchmarkable CSTParser.parse_expression(ps) setup = (ps = ParseState("""x=>x=>x=>x=>x"""))
suite["ops"]["cond * 5"] = @benchmarkable CSTParser.parse_expression(ps) setup = (ps = ParseState("""x ? x : x ? x : x ? x : x ? x : x ? x : x"""))
suite["ops"]["bin syn * 5"] = @benchmarkable CSTParser.parse_expression(ps) setup = (ps = ParseState("""x||x||x||x||x"""))
suite["ops"]["dot * 5"] = @benchmarkable CSTParser.parse_expression(ps) setup = (ps = ParseState("""x.x.x.x.x"""))

suite["call"] = BenchmarkGroup()
suite["call"]["no arg"] = @benchmarkable CSTParser.parse_expression(ps) setup = (ps = ParseState("func()"))
suite["call"]["1 arg"] = @benchmarkable CSTParser.parse_expression(ps) setup = (ps = ParseState("func(arg)"))
suite["call"]["10 arg"] = @benchmarkable CSTParser.parse_expression(ps) setup = (ps = ParseState("func(arg,arg,arg,arg,arg,arg,arg,arg,arg,arg)"))
suite["call"]["10 kw"] = @benchmarkable CSTParser.parse_expression(ps) setup = (ps = ParseState("func(arg = arg, arg = arg,arg = arg,arg = arg,arg = arg,arg = arg,arg = arg,arg = arg,arg = arg,arg = arg,)"))

# results = run(suite)
# BenchmarkTools.save("bench1.ignore.json", results)
# baseline = BenchmarkTools.load("bench1.ignore.json")
# [leaves(minimum(baseline)) leaves(minimum(results)) leaves(ratio(minimum(baseline), minimum(results)))]

function compare(suite, against = "baseline.ignore.json", save = "")
    results = run(suite)
    if !isempty(save)
        BenchmarkTools.save(joinpath(@__DIR__, save), results)
    end
    baseline = BenchmarkTools.load(joinpath(@__DIR__, against))[1]
    display_comp(baseline, results)
end

function display_comp(baseline, results)
    comp = [leaves(minimum(baseline)) leaves(minimum(results)) leaves(ratio(minimum(baseline), minimum(results)))]
    println(rpad("name", 50), lpad("current", 15), lpad("baseline", 15), lpad("time ratio", 15), lpad("alloc ratio", 15))
    for (n, r) in leaves(results)
        print(rpad(join(n, "-"), 50))
        print(lpad(BenchmarkTools.prettytime(time(r)), 15))
        try
            br = baseline[n]
            print(lpad(BenchmarkTools.prettytime(time(br)), 15))
            print(lpad(BenchmarkTools.prettypercent(ratio(time(r), time(br))), 15))
            print(lpad(BenchmarkTools.prettypercent(ratio(allocs(r), allocs(br))), 15))
        catch
        end
        println()
    end
end


