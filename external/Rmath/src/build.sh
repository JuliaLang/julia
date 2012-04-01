for i in *.c; do gcc -std=gnu99 -I../include -DMATHLIB_STANDALONE -O2 -c $i; done
