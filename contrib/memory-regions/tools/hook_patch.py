"""Build a Compiler working copy with one hook: after the optimizer passes,
hand the IRCode to a function installed from outside. The region checker
installs its instrumentation pass there.

    python3 hook_patch.py <workdir> [<julia-executable>]

The optional second argument names the julia to take the Compiler source
from and to instantiate with; it defaults to `julia +1.13`. Pass the
patched build's executable when the hooked compiler must run on it.
"""
import shutil, subprocess, sys, os

workdir = os.path.abspath(sys.argv[1])
julia = sys.argv[2].split() if len(sys.argv) > 2 else ["julia", "+1.13"]
share = subprocess.run(
    julia + ["--startup-file=no", "-e",
     'print(abspath(joinpath(Sys.BINDIR, "..", "share", "julia")))'],
    capture_output=True, text=True, check=True).stdout.strip()

os.makedirs(workdir, exist_ok=True)
dst = os.path.join(workdir, "Compiler")
shutil.rmtree(dst, ignore_errors=True)
shutil.copytree(os.path.join(share, "Compiler"), dst)

p = os.path.join(dst, "src", "optimize.jl")
s = open(p).read()
old = """function optimize(interp::AbstractInterpreter, opt::OptimizationState, caller::InferenceResult)
    @zone "CC: OPTIMIZER" ir = run_passes_ipo_safe(opt.src, opt)"""
assert old in s, "anchor missing"
new = """# --- region checker hook ------------------------------------------------------
# Installed from outside; receives (ir, opt) after the passes and returns the
# possibly rewritten ir. `nothing` means no hook.
const IR_HOOK = RefValue{Any}(nothing)
# ------------------------------------------------------------------------------

function optimize(interp::AbstractInterpreter, opt::OptimizationState, caller::InferenceResult)
    @zone "CC: OPTIMIZER" ir = run_passes_ipo_safe(opt.src, opt)
    let hook = IR_HOOK[]
        if hook !== nothing
            ir = Core._call_latest(hook, ir, opt)::IRCode
        end
    end"""
s = s.replace(old, new, 1)
open(p, "w").write(s)

env = os.path.join(workdir, "env")
os.makedirs(env, exist_ok=True)
open(os.path.join(env, "Project.toml"), "w").write(
    '[deps]\nCompiler = "807dbc54-b67e-4c79-8afb-eafe4df6f2e1"\n\n'
    + '[sources]\nCompiler = {path = "' + dst + '"}\n')
subprocess.run(julia + ["--startup-file=no", "--project=" + env,
                "-e", "using Pkg; Pkg.instantiate()"], check=True)
print("hooked compiler ready in " + workdir)
