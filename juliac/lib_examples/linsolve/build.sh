#! /bin/bash
if [ ! -f Manifest.toml ]; then
    echo "Instantiating package environment..."
    ../../../julia --project -e 'using Pkg; Pkg.instantiate()'
    echo "done"
fi
echo "Building library..."
../../../julia --project ../../juliac.jl --output-lib liblinsolve --compile-ccallable --static-call-graph liblinsolve.jl
echo "done"
echo "Linking C main..."
gcc -o linsolve10 -L. -Wl,-rpath,\$ORIGIN ../../test/test.c -llinsolve
echo "done"
