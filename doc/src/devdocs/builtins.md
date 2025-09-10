# [Core.Builtins](@id lib-builtins)

The following builtin functions are considered unstable, but provide the basic
definitions for what defines the abilities and behaviors of a Julia
program. They are typically accessed through a higher level generic API.

## Raw access to memory

```@docs
Core.Intrinsics.pointerref
Core.Intrinsics.pointerset
Core.Intrinsics.atomic_pointerref
Core.Intrinsics.atomic_pointerset
Core.Intrinsics.atomic_pointerswap
Core.Intrinsics.atomic_pointermodify
Core.Intrinsics.atomic_pointerreplace
```

## Managed memory

```@docs
Core.memorynew
Core.memoryrefnew
Core.memoryrefoffset
Core.memoryrefget
Core.memoryrefset!
Core.memoryref_isassigned
Core.memoryrefswap!
Core.memoryrefmodify!
Core.memoryrefreplace!
Core.memoryrefsetonce!
```

## Module bindings

```@docs
Core.get_binding_type
```

## Various helper functions
```
Base.quoted
Base.isa_ast_node
```

## Other

```@docs
Core.IntrinsicFunction
Core.Intrinsics
Core.IR
Core._task
Core.task_result_type
```

## Adding New Builtin Functions

This section documents the process for adding a new builtin function to Julia, (using `_task` as an example).

### Overview

Adding a new builtin function requires changes across multiple subsystems in Julia:
1. C runtime implementation
2. Julia inference integration
3. Complex inference integration (if required)
4. Optimization passes (if applicable)
5. Codegen changes (if applicable)

### Step-by-Step Process

#### 1. Add to Builtin Function Registry

**File: `src/builtin_proto.h`**
```c
// Add to JL_BUILTIN_FUNCTIONS macro (alphabetically)
#define JL_BUILTIN_FUNCTIONS \
    XX(apply_type,"apply_type") \
    XX(_task,"_task") \          // <-- Add your function here
    // ... other functions
```

**File: `src/builtins.c`**
```c
// Implement the C function
JL_CALLABLE(jl_f__task)
{
    JL_NARGS(_task, 2, 3);
    JL_TYPECHK(_task, long, args[1]);
    return jl_new_task_impl(args, nargs);
}
```

**Argument validation**: Always validate argument count and types in the C implementation.

#### 2. Julia Inference Integration

**File: `Compiler/src/tfuncs.jl`**
```julia
# Add simple tfunc if no special state is required
add_tfunc(Core._task, 2, 3, (@nospecialize(f), @nospecialize(size), @nospecialize(optional...)) -> Task, 20)

# OR for complex cases requiring AbstractInterpreter state:
# Leave out add_tfunc and implement in abstractinterpretation.jl instead
```

#### 3. Complex Inference Integration (if `add_tfunc` was insufficient)

**File: `Compiler/src/stmtinfo.jl`**
```julia
# For builtins that perform indirect calls, use IndirectCallInfo.
# Set add_edges=true, unless the info is only for inlining and not used by inference
#
info_result = IndirectCallInfo(callinfo.info, callinfo.effects, true)
```

**File: `Compiler/src/abstractinterpretation.jl`**
```julia
# Add to builtin handling in abstract_call_builtin
elseif f === Core._your_builtin
    return Future(abstract_eval_your_builtin(interp, arginfo, si, sv))

# Implement custom abstract evaluation if needed
function abstract_eval_your_builtin(interp::AbstractInterpreter, arginfo::ArgInfo, si::StmtInfo, sv::AbsIntState)
    # Validation and inference logic
    return CallMeta(return_type, exception_type, effects, call_info)
end
```

#### 4. Optimization Passes (if applicable)

For example, if the operator has a special inlining:

**File: `Compiler/src/ssair/inlining.jl`**
```julia
# Add to special builtin handling
if (f !== Core.invoke &&
    f !== Core.finalizer &&
    # ... other functions
    f !== Core._your_builtin)  # <-- Add here

# Add optimization logic in `assemble_inline_todo!`
elseif f === Core._your_builtin
    handle_your_builtin_call!(ir, idx, stmt, info, state)
end
```

#### 5. Documentation

**File: `base/docs/basedocs.jl`**
```julia
"""
    Core._your_builtin(arg1, arg2) -> ReturnType
    Core._your_builtin(arg1, arg2, optional_arg) -> ReturnType

This builtin is an implementation detail used by [higher-level function] and should
not be called directly by end-users. Use `HigherLevelFunction(args...)` instead.

Brief description of what the builtin does and its parameters.
The optional third argument `optional_arg` can be a [description of types/purpose].
"""
Core._your_builtin
```

**File: `doc/src/devdocs/builtins.md`**
```julia
# Add to the @docs block in the appropriate section
Core._your_builtin
```

#### 6. Testing

**File: `Compiler/test/effects.jl`**
```julia
# Add test for effects modeling (if your builtin has specific effects)
let effects = Base.infer_effects(Core._your_builtin, (ArgType,))
    @test !Compiler.is_consistent(effects)
    @test Compiler.is_nothrow(effects)
end
```

**File: `Compiler/test/inference.jl`**
```julia
# Add test for return type inference
@test Base.infer_return_type(Core._your_builtin, (ArgType,)) === ExpectedReturnType
```
