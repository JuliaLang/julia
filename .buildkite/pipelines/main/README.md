## Main pipeline

This is the main pipeline. It contains most of the builders. These builders are triggered by GitHub webhook events, such as pushes and pull requests.

We have a different main pipeline for each permanent branch.

For example:

| Permanent Branch | Pipeline                                                                         |
| ---------------- | -------------------------------------------------------------------------------- |
| `master`         | [`julia-master`](https://buildkite.com/julialang/julia-master)                   |
| `release-1.6`    | [`julia-release-1.6`](https://buildkite.com/julialang/julia-release-1-dot-6) |
| `release-1.7`    | [`julia-release-1.7`](https://buildkite.com/julialang/julia-release-1-dot-7) |

(This is not a complete list.)
