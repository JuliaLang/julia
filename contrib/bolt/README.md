BOLT only works on x86_64 and arch64 on Linux.

DO NOT STRIP THE RESULTING .so FILES, https://github.com/llvm/llvm-project/issues/56738.
If you really need to, try adding `-use-gnu-stack` to `BOLT_ARGS`.

To build a BOLT-optimized version of Julia run the following commands (`cd` into this directory first)
```bash
make stage1
make copy_originals
make bolt_instrument
make finish_stage1
make merge_data
make bolt
```
After these commands finish, the optimized version of Julia will be built in the `optimized.build` directory.

This doesn't align the code to support huge pages as it doesn't seem that we do that currently, this decreases the size of the .so files by 2-4mb.
