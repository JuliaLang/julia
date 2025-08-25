# Using AI agents to work on Julia

> ![WARNING]
> You are responsible for the code you submit in PRs. Do not submit PRs
> containing AI-generated code that you do not understand or that does not
> meet the ordinary quality bar for PRs to julia.

This page documents best practices for setting up AI agents to work with Julia.
If you find additional prompt instructions that work well for common tasks,
consider submitting a PR to add these to AGENTS.md.

## Google Jules

Use the following for your `Initial Setup` configuration.

```
curl -fsSL https://install.julialang.org | sh -s -- -y --default-channel nightly
. /home/swebot/.profile
```

Jules has access to the internet, so you can give it links to issues or additional
documentation in your prompting.

## OpenAI Codex

Configure the following:

Setup Script
```
apt update
apt install less
curl -fsSL https://install.julialang.org | sh -s -- -y --default-channel nightly
source /root/.bashrc
make -C /workspace/julia/doc alldeps JULIA_EXECUTABLE="/root/.juliaup/bin/julia"
make -C /workspace/julia/test install-revise-deps JULIA_EXECUTABLE="/root/.juliaup/bin/julia"
```

Environment Variables
```
JULIA_PKG_OFFLINE=true
```

Codex does not have internet access after initial setup, so you cannot give it
additional information as links - you will need to copy any relevant text into
the prompt.

Note that Codex rebuilds the environment after every invocation. This can
add significant latency. Codex work best for well-defined tasks that can
be solved in a single shot.
