BOLT only works on x86_64 and arch64 on Linux or MacOS.

DO NOT STRIP THE RESULTING .so FILES, https://github.com/llvm/llvm-project/issues/56738

Example usage
```bash
make stage1
make move_originals
make bolt_instrument
make pkgimage
make merge_data
make bolt
make rebuild-pkgimage
```

YOU NEED TO REBUILD THE PKGIMAGES AFTER BOLTING OTHERWISE SEGFAULTS GALORE.

This doesn't align the code to support huge pages as it doesn't seem that we do that currently.
Decreases .so sizes by 2-4mb.