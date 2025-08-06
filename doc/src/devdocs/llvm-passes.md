# Custom LLVM Passes

Julia has a number of custom LLVM passes. Broadly, they can be classified into passes that are required to be run to maintain Julia semantics, and passes that take advantage of Julia semantics to optimize LLVM IR.

## Semantic Passes

These passes are used to transform LLVM IR into code that is legal to be run on a CPU. Their main purpose is to enable simpler IR to be emitted by codegen, which then enables other LLVM passes to optimize common patterns.

### CPUFeatures

* Filename: `llvm-cpufeatures.cpp`
* Class Name: `CPUFeaturesPass`
* Opt Name: `module(CPUFeatures)`

This pass lowers the `julia.cpu.have_fma.(f32|f64)` intrinsic to either true or false, depending on the target architecture and target features present on the function. This intrinsic is often used to determine if using algorithms dependent on fast [fused multiply-add](https://en.wikipedia.org/wiki/Multiply%E2%80%93accumulate_operation#Fused_multiply%E2%80%93add) operations is better than using standard algorithms not dependent on such instructions.

### DemoteFloat16

* Filename: `llvm-demote-float16.cpp`
* ClassName: `DemoteFloat16Pass`
* Opt Name `function(DemoteFloat16)`

This pass replaces [float16](https://en.wikipedia.org/wiki/Half-precision_floating-point_format) operations with float32 operations on architectures that do not natively support float16 operations. This is done by inserting `fpext` and `fptrunc` instructions around any float16 operation. On architectures that do support native float16 operations, this pass is a no-op.

### LateGCLowering

* Filename: `llvm-late-gc-lowering.cpp`
* Class Name: `LateLowerGCPass`
* Opt Name: `function(LateLowerGCFrame)`

This pass performs most of the GC rooting work required to track pointers between GC safepoints. It also lowers several intrinsics to their corresponding instruction translation, and is permitted to violate the non-integral invariants previously established (`pointer_from_objref` is lowered to a `ptrtoint` instruction here). This pass typically occupies the most time out of all the custom Julia passes, due to its dataflow algorithm to minimize the number of objects live at any safepoint.

### FinalGCLowering

* Filename: `llvm-final-gc-lowering.cpp`
* Class Name: `FinalLowerGCPass`
* Opt Name: `module(FinalLowerGC)`

This pass lowers a few last intrinsics to their final form targeting functions in the `libjulia` library. Separating this from `LateGCLowering` enables other backends (GPU compilation) to supply their own custom lowerings for these intrinsics, enabling the Julia pipeline to be used on those backends as well.
### RemoveNI

* Filename: `llvm-remove-ni.cpp`
* Class Name: `RemoveNIPass`
* Opt Name: `module(RemoveNI)`

This pass removes the non-integral address spaces from the module's datalayout string. This enables the backend to lower Julia's custom address spaces directly to machine code, without a costly rewrite of every pointer operation to address space 0.

### SIMDLoop

* Filename: `llvm-simdloop.cpp`
* Class Name: `LowerSIMDLoopPass`
* Opt Name: `loop(LowerSIMDLoop)`

This pass acts as the main driver of the `@simd` annotation. Codegen inserts a `!llvm.loopid` marker at the back branch of a loop, which this pass uses to identify loops that were originally marked with `@simd`. Then, this pass looks for a chain of floating point operations that form a reduce and adds the `contract` and `reassoc` fast math flags to allow reassociation (and thus vectorization). This pass does not preserve either loop information nor inference correctness, so it may violate Julia semantics in surprising ways. If the loop was annotated with `ivdep` as well, then the pass marks the loop as having no loop-carried dependencies (the resulting behavior is undefined if the user annotation was incorrect or gets applied to the wrong loop).

### LowerPTLS

* Filename: `llvm-ptls.cpp`
* Class Name: `LowerPTLSPass`
* Opt Name: `module(LowerPTLSPass)`

This pass lowers thread-local Julia intrinsics to assembly instructions. Julia relies on thread-local storage for garbage collection and multithreading task scheduling. When compiling code for system images and package images, this pass replaces calls to intrinsics with loads from global variables that are initialized at load time.

If codegen produces a function with a `swiftself` argument and calling convention, this pass assumes the `swiftself` argument is the pgcstack and will replace the intrinsics with that argument. Doing so provides speedups on architectures that have slow thread local storage accesses.

### RemoveAddrspaces

* Filename: `llvm-remove-addrspaces.cpp`
* Class Name: `RemoveAddrspacesPass`
* Opt Name: `module(RemoveAddrspaces)`

This pass renames pointers in one address space to another address space. This is used to remove Julia-specific address spaces from LLVM IR.

### RemoveJuliaAddrspaces

* Filename: `llvm-remove-addrspaces.cpp`
* Class Name: `RemoveJuliaAddrspacesPass`
* Opt Name: `module(RemoveJuliaAddrspaces)`

This pass removes Julia-specific address spaces from LLVM IR. It is mostly used for displaying LLVM IR in a less cluttered format. Internally, it is implemented off the RemoveAddrspaces pass.

### Multiversioning

* Filename: `llvm-multiversioning.cpp`
* Class Name: `MultiVersioningPass`
* Opt Name: `module(JuliaMultiVersioning)`

This pass performs modifications to a module to create functions that are optimized for running on different architectures (see sysimg.md and pkgimg.md for more details). Implementation-wise, it clones functions and applies different target-specific attributes to them to allow the optimizer to use advanced features such as vectorization and instruction scheduling for that platform. It also creates some infrastructure to enable the Julia image loader to select the appropriate version of the function to call based on the architecture the loader is running on. The target-specific attributes are controlled by the `julia.mv.specs` module flag, which during compilation is derived from the [`JULIA_CPU_TARGET`](@ref JULIA_CPU_TARGET) environment variable. The pass must also be enabled by providing a `julia.mv.enable` module flag with a value of 1.

!!! warning

    Use of `llvmcall` with multiversioning is dangerous. `llvmcall` enables access to features not typically exposed by the Julia APIs, and are therefore usually not available on all architectures. If multiversioning is enabled and code generation is requested for a target architecture that does not support the feature required by an `llvmcall` expression, LLVM will probably error out, likely with an abort and the message `LLVM ERROR: Do not know how to split the result of this operator!`.

### GCInvariantVerifier

* Filename: `llvm-gc-invariant-verifier.cpp`
* Class Name: `GCInvariantVerifierPass`
* Opt Name: `module(GCInvariantVerifier)`

This pass is used to verify Julia's invariants about LLVM IR. This includes things such as the nonexistence of `ptrtoint` in Julia's [non-integral address spaces](https://llvm.org/docs/LangRef.html#non-integral-pointer-type) [^nislides] and the existence of only blessed `addrspacecast` instructions (Tracked -> Derived, 0 -> Tracked, etc). It performs no transformations on IR.

[^nislides]: https://llvm.org/devmtg/2015-02/slides/chisnall-pointers-not-int.pdf

## Optimization Passes

These passes are used to perform transformations on LLVM IR that LLVM will not perform itself, e.g. fast math flag propagation, escape analysis, and optimizations on Julia-specific internal functions. They use knowledge about Julia's semantics to perform these optimizations.

### AllocOpt

* Filename: `llvm-alloc-opt.cpp`
* Class Name: `AllocOptPass`
* Opt Name: `function(AllocOpt)`

Julia does not have the concept of a program stack as a place to allocate mutable objects. However, allocating objects on the stack reduces GC pressure and is critical for GPU compilation. Thus, `AllocOpt` performs heap to stack conversion of objects that it can prove do not [escape](https://en.wikipedia.org/wiki/Escape_analysis) the current function. It also performs a number of other optimizations on allocations, such as removing allocations that are never used, optimizing typeof calls to freshly allocated objects, and removing stores to allocations that are immediately overwritten. The escape analysis implementation is located in `llvm-alloc-helpers.cpp`. Currently, this pass does not use information from `EscapeAnalysis.jl`, though that may change in the future.

### PropagateJuliaAddrspaces

* Filename: `llvm-propagate-addrspaces.cpp`
* Class Name: `PropagateJuliaAddrspacesPass`
* Opt Name: `function(PropagateJuliaAddrspaces)`

This pass is used to propagate Julia-specific address spaces through operations on pointers. LLVM is not allowed to introduce or remove addrspacecast instructions by optimizations, so this pass acts to eliminate redundant addrspace casts by replacing operations with their equivalent in a Julia address space. For more information on Julia's address spaces, see (TODO link to llvm.md).

### JuliaLICM

* Filename: `llvm-julia-licm.cpp`
* Class Name: `JuliaLICMPass`
* Opt Name: `loop(JuliaLICM)`

This pass is used to hoist Julia-specific intrinsics out of loops. Specifically, it performs the following transformations:
1. Hoist `gc_preserve_begin` and sink `gc_preserve_end` out of loops when the preserved objects are loop-invariant.
   1. Since objects preserved within a loop are likely preserved for the duration of the loop, this transformation can reduce the number of `gc_preserve_begin`/`gc_preserve_end` pairs in the IR. This makes it easier for the `LateLowerGCPass` to identify where particular objects are preserved.
2. Hoist write barriers with invariant objects
   1. Here we assume that there are only two generations that an object can be a part of. Given that, a write barrier needs to only execute once for any pair of the same object. Thus, we can hoist write barriers out of loops when the object being written to is loop-invariant.
3. Hoist allocations out of loops when they do not escape the loop
   1. We use a very conservative definition of escape here, the same as the one used in `AllocOptPass`. This transformation can reduce the number of allocations in the IR, even when an allocation escapes the function altogether.

!!! note

    This pass is required to preserve LLVM's [MemorySSA](https://llvm.org/docs/MemorySSA.html) ([Short Video](https://www.youtube.com/watch?v=bdxWmryoHak), [Longer Video](https://www.youtube.com/watch?v=1e5y6WDbXCQ)) and [ScalarEvolution](https://baziotis.cs.illinois.edu/compilers/introduction-to-scalar-evolution.html) ([Newer Slides](https://llvm.org/devmtg/2018-04/slides/Absar-ScalarEvolution.pdf) [Older Slides](https://llvm.org/devmtg/2009-10/ScalarEvolutionAndLoopOptimization.pdf)) analyses.
