---
name: external-deps
description: Modify Julia's external dependencies and JLLs — patches under deps/patches/, version bumps in deps/, and JLL updates. Use when touching deps/ or any *_jll folder, including refreshing checksums.
---

# External dependencies & JLLs

Use this when modifying `deps/`, dependency patches, or a JLL.

## External dependencies (deps/, deps/patches/)

When modifying external dependencies (patches in `deps/patches/` or version
updates in `deps/`):

1. Always test builds with `USE_BINARYBUILDER=0` to ensure source builds work correctly.
2. For patches to external libraries:
   - Verify the patch applies cleanly by running the extraction and patch steps.
   - Test the full build of the dependency: `make -C deps USE_BINARYBUILDER=0 compile-<depname>`.
   - Prefer using the full upstream commit in `git am` format (e.g., `git format-patch`) which includes proper commit metadata.
3. When updating dependency versions, ensure all associated patches still apply.

## External JLLs

To update a JLL to the latest version:

- Update the version number in the appropriate jll folder.
- If the dependencies in the upstream jll changed, update the `Project.toml`.
- Run `make -f contrib/refresh_checksums.mk <jll>` to update the checksums. This may take a few minutes.
