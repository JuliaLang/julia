# Inference

## How inference works

In Julia compiler, "type inference" refers to the process of deducing the types of later
values from the types of input values. Julia's approach to inference has been described in
the blog posts below:
1. [Shows a simplified implementation of the data-flow analysis algorithm, that Julia's type inference routine is based on.](https://aviatesk.github.io/posts/data-flow-problem/)
2. [Gives a high level view of inference with a focus on its inter-procedural convergence guarantee.](https://info.juliahub.com/inference-convergence-algorithm-in-julia)
3. [Explains a refinement on the algorithm introduced in 2.](https://info.juliahub.com/inference-convergence-algorithm-in-julia-revisited)

## Debugging compiler.jl

You can start a Julia session, edit `compiler/*.jl` (for example to
insert `print` statements), and then replace `Core.Compiler` in your
running session by navigating to `base` and executing
`include("compiler/compiler.jl")`. This trick typically leads to much faster
development than if you rebuild Julia for each change.

Alternatively, you can use the [Revise.jl](https://github.com/timholy/Revise.jl)
package to track the compiler changes by using the command
`Revise.track(Core.Compiler)` at the beginning of your Julia session. As
explained in the [Revise documentation](https://timholy.github.io/Revise.jl/stable/),
the modifications to the compiler will be reflected when the modified files
are saved.

A convenient entry point into inference is `typeinf_code`. Here's a
demo running inference on `convert(Int, UInt(1))`:

```julia
# Get the method
atypes = Tuple{Type{Int}, UInt}  # argument types
mths = methods(convert, atypes)  # worth checking that there is only one
m = first(mths)

# Create variables needed to call `typeinf_code`
interp = Core.Compiler.NativeInterpreter()
sparams = Core.svec()      # this particular method doesn't have type-parameters
run_optimizer = true       # run all inference optimizations
types = Tuple{typeof(convert), atypes.parameters...} # Tuple{typeof(convert), Type{Int}, UInt}
Core.Compiler.typeinf_code(interp, m, types, sparams, run_optimizer)
```

If your debugging adventures require a `MethodInstance`, you can look it up by
calling `Core.Compiler.specialize_method` using many of the variables above.
A `CodeInfo` object may be obtained with
```julia
# Returns the CodeInfo object for `convert(Int, ::UInt)`:
ci = (@code_typed convert(Int, UInt(1)))[1]
```

## The inlining algorithm (`inline_worthy`)

Much of the hardest work for inlining runs in `ssa_inlining_pass!`.
However, if your question is "why didn't my function inline?"
then you will most likely be interested in `inline_worthy`,
which makes a decision to inline the function call or not.

`inline_worthy` implements a cost-model, where "cheap" functions get
inlined; more specifically, we inline functions if their anticipated
run-time is not large compared to the time it would take to
[issue a call](https://en.wikipedia.org/wiki/Calling_convention) to
them if they were not inlined. The cost-model is extremely simple and
ignores many important details: for example, all `for` loops are
analyzed as if they will be executed once, and the cost of an
`if...else...end` includes the summed cost of all branches. It's also
worth acknowledging that we currently lack a suite of functions
suitable for testing how well the cost model predicts the actual
run-time cost, although
[BaseBenchmarks](https://github.com/JuliaCI/BaseBenchmarks.jl)
provides a great deal of indirect information about the successes and
failures of any modification to the inlining algorithm.

The foundation of the cost-model is a lookup table, implemented in
`add_tfunc` and its callers, that assigns an estimated cost (measured
in CPU cycles) to each of Julia's intrinsic functions. These costs are
based on
[standard ranges for common architectures](http://ithare.com/wp-content/uploads/part101_infographics_v08.png)
(see
[Agner Fog's analysis](https://www.agner.org/optimize/instruction_tables.pdf)
for more detail).

We supplement this low-level lookup table with a number of special
cases. For example, an `:invoke` expression (a call for which all
input and output types were inferred in advance) is assigned a fixed
cost (currently 20 cycles). In contrast, a `:call` expression, for
functions other than intrinsics/builtins, indicates that the call will
require dynamic dispatch, in which case we assign a cost set by
`Params.inline_nonleaf_penalty` (currently set at `1000`). Note
that this is not a "first-principles" estimate of the raw cost of
dynamic dispatch, but a mere heuristic indicating that dynamic
dispatch is extremely expensive.

Each statement gets analyzed for its total cost in a function called
`statement_cost`. You can display the cost associated with each statement
as follows:
```jldoctest; filter=r"tuple.jl:\d+"
julia> Base.print_statement_costs(stdout, map, (typeof(sqrt), Tuple{Int},)) # map(sqrt, (2,))
map(f, t::Tuple{Any}) @ Base tuple.jl:358
  0 1 ─ %1  = $(Expr(:boundscheck, true))::Bool
  0 │   %2  =   builtin Base.getfield(_3, 1, %1)::Int64
  1 │   %3  = intrinsic Base.sitofp(Float64, %2)::Float64
  0 │   %4  = intrinsic Base.lt_float(%3, 0.0)::Bool
  0 └──       goto #3 if not %4
  0 2 ─          invoke Base.Math.throw_complex_domainerror(:sqrt::Symbol, %3::Float64)::Union{}
  0 └──       unreachable
 20 3 ─ %8  = intrinsic Base.Math.sqrt_llvm(%3)::Float64
  0 └──       goto #4
  0 4 ─       goto #5
  0 5 ─ %11 =   builtin Core.tuple(%8)::Tuple{Float64}
  0 └──       return %11

```

The line costs are in the left column. This includes the consequences of inlining and other forms of optimization.
