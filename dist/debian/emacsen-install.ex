#! /bin/sh -e
# /usr/lib/emacsen-common/packages/install/julia

# Written by Jim Van Zandt <jrv@debian.org>, borrowing heavily
# from the install scripts for gettext by Santiago Vila
# <sanvila@ctv.es> and octave by Dirk Eddelbuettel <edd@debian.org>.

FLAVOR=$1
PACKAGE=julia

if [ ${FLAVOR} = emacs ]; then exit 0; fi

echo install/${PACKAGE}: Handling install for emacsen flavor ${FLAVOR}

#FLAVORTEST=`echo $FLAVOR | cut -c-6`
#if [ ${FLAVORTEST} = xemacs ] ; then
#    SITEFLAG="-no-site-file"
#else
#    SITEFLAG="--no-site-file"
#fi
FLAGS="${SITEFLAG} -q -batch -l path.el -f batch-byte-compile"

ELDIR=/usr/share/emacs/site-lisp/${PACKAGE}
ELCDIR=/usr/share/${FLAVOR}/site-lisp/${PACKAGE}

# Install-info-altdir does not actually exist.
# Maybe somebody will write it.
if test -x /usr/sbin/install-info-altdir; then
    echo install/${PACKAGE}: install Info links for ${FLAVOR}
    install-info-altdir --quiet --section "" "" --dirname=${FLAVOR} /usr/share/info/${PACKAGE}.info.gz
fi

install -m 755 -d ${ELCDIR}
cd ${ELDIR}
FILES=`echo *.el`
cp ${FILES} ${ELCDIR}
cd ${ELCDIR}

cat << EOF > path.el
(setq load-path (cons "." load-path) byte-compile-warnings nil)
EOF
${FLAVOR} ${FLAGS} ${FILES}
rm -f *.el path.el

exit 0
