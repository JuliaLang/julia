# Backporter

This directory contains some scripts that are useful for backporting commits to
older Julia releases.

Steps to run the backport script:

- Have `git` available.
- Have a git token set in the `GITHUB_AUTH` environment variable.
- Check out a branch that you want to backport to, typically `backports-1.x`.
- Set the various script options (repo, version to backport to, limit date).
- Run the script as `julia --project=contrib/Backportercontrib/Backporter/backporter.jl`.
- Push the branch to the remote.
- Update the PR with the output from the script.

The script is not perfect, sometimes you need to finish things up with some
manual tweaks.

