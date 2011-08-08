#! /bin/sh

for i in `cat src/FILES`; do curl -o src/$i -O http://www.freebsd.org/cgi/cvsweb.cgi/~checkout~/src/lib/msun/src/$i?content-type=text%2Fplain;only_with_tag=RELENG_8_2_0_RELEASE; done

for i in `cat amd64/FILES`; do curl -o amd64/$i -O http://www.freebsd.org/cgi/cvsweb.cgi/~checkout~/src/lib/msun/amd64/$i?content-type=text%2Fplain;only_with_tag=RELENG_8_2_0_RELEASE; done

for i in `cat i387/FILES`; do curl -o i387/$i -O http://www.freebsd.org/cgi/cvsweb.cgi/~checkout~/src/lib/msun/i387/$i?content-type=text%2Fplain;only_with_tag=RELENG_8_2_0_RELEASE; done

for i in `cat bsdsrc/FILES`; do curl -o bsdsrc/$i -O http://www.freebsd.org/cgi/cvsweb.cgi/~checkout~/src/lib/msun/bsdsrc/$i?content-type=text%2Fplain;only_with_tag=RELENG_8_2_0_RELEASE; done
