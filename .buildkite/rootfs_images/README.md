## Rootfs images

Our CI setup makes use of rootfs images that contain our build tools.
These rootfs images are built using the fairly simple scripts held within this directory.
Most images are based on Debian, making use of `debootstrap` to provide a quick and easy rootfs with packages installed through an initial `apt` invocation.

## Testing out a rootfs image

If you want to test a rootfs image locally, you can use the `test_roofs.jl` script, passing in the URL of the rootfs you want to test.  It will drop you into a shell within the build environment, where you can recreate build failures more reliably.
