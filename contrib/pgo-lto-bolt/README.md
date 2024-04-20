BOLT only works on x86_64 and arch64 on Linux or MacOS.

DO NOT STRIP THE RESULTING .so FILES, https://github.com/llvm/llvm-project/issues/56738.
If you really need to, try adding `-use-gnu-stack` to `BOLT_ARGS`.

Example usage: (`cd` into this directory first)
```bash
make stage1
make stage2
make copy_originals
make bolt_instrument
make pkgimage
make merge_data
make bolt
```

This doesn't align the code to support huge pages as it doesn't seem that we do that currently, this decreases the size of the .so files by 2-4mb.
