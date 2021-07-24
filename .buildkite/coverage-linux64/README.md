# Coverage pipeline

We run coverage on a separate pipeline, that uses a scheduled build rather than webhooks.
The pipeline is here: https://buildkite.com/julialang/julia-coverage-linux64

It contains [its own webui steps](0_webuiy.ml) (listed here in this repository for clarity) and its own [pipeline.yml](pipeline.yml).
