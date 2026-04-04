
Generate the logo
```julia
using Luxor
Drawing(325, 325, joinpath(pwd(), "julia-dots.svg"))
origin()
translate(0, 25)
juliacircles(100)
finish()
```

Create the ico file
```sh
#!/bin/bash

for size in 16 20 24 32 40 48 64 128 256; do
    rsvg-convert -w $size -h $size julia-dots.svg -o $size.png
done

convert 256.png 128.png 64.png 48.png 40.png 32.png 24.png 20.png 16.png julia.ico

rm 256.png 128.png 64.png 48.png 40.png 32.png 24.png 20.png 16.png
```
