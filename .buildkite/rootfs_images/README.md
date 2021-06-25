## Rootfs images

Our CI setup makes use of rootfs images that contain our build tools.
These rootfs images are built using the fairly simple scripts held within this directory.
Most images are based on Debian, making use of `debootstrap` to provide a quick and easy rootfs with packages installed through an initial `apt` invocation.
