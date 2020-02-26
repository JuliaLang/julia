## A rule for making a git-external dependency ##
# call syntax:
#   $(eval $(call git-external,dirname,VARNAME,file_from_download,file_from_compile,SRCDIR)
# dirname is the folder name to create
# VARNAME is the uppercased variable name prefix
# file_from_download (deprecated)
# file_from_compile (deprecated)
# SRCDIR is either $(SRCCACHE) or $(BUILDDIR), depending on whether the target supports out-of-tree builds
#
# also, in a file named dirname.version, define variables VARNAME_BRANCH and VARNAME_SHA1
#
# also, define a Makefile variable VARNAME_GIT_URL to use as the clone origin
# and a Makefile variable VARNAME_TAR_URL to use as the clone origin when DEPS_GIT is set
# this will pass along the VARNAME_SHA1 target in $1 for its use
#
# this defines rules for:
#   VARNAME_SRC_DIR = source directory for the output, relative to $(SRCCACHE) or $(BUILDDIR)
#   VARNAME_SRC_FILE = target file for make get-VARNAME target
#   dirname:
#   dirname/source-extracted:
#   distclean-dirname:
#
define git-external
include $$(SRCDIR)/$1.version
$2_SHA1 := $$(strip $$($2_SHA1))
$2_BRANCH := $$(strip $$($2_BRANCH))

ifneq (,$$(filter $1 1,$$(DEPS_GIT)))
$2_SRC_DIR := $1
$2_SRC_FILE := $$(SRCCACHE)/$1.git
$$($2_SRC_FILE)/HEAD: | $$(SRCCACHE)
	git clone -q --mirror --branch $$($2_BRANCH) $$($2_GIT_URL) $$(dir $$@)
$5/$1/.git/HEAD: | $$($2_SRC_FILE)/HEAD
	# try to update the cache, if that fails, attempt to continue anyways (the ref might already be local)
	-cd $$($2_SRC_FILE) && git fetch -q $$($2_GIT_URL) $$($2_BRANCH):remotes/origin/$$($2_BRANCH)
	git clone -q --depth=10 --branch $$($2_BRANCH) $$($2_SRC_FILE) $5/$1
	cd $5/$1 && git remote set-url origin $$($2_GIT_URL)
#ifneq ($3,)
	touch -c $5/$1/$3 # old target
#endif
	echo 1 > $5/$1/source-extracted
ifneq ($5,$$(BUILDDIR))
$$(BUILDDIR)/$1:
	mkdir -p $$@
$5/$1/source-extracted: | $$(BUILDDIR)/$1
endif
$5/$1/source-extracted: $$(SRCDIR)/$1.version | $5/$1/.git/HEAD
	# try to update the cache, if that fails, attempt to continue anyways (the ref might already be local)
	-cd $$(SRCCACHE)/$1.git && git fetch -q $$($2_GIT_URL) $$($2_BRANCH):remotes/origin/$$($2_BRANCH)
	cd $5/$1 && git fetch -q $$(SRCCACHE)/$1.git remotes/origin/$$($2_BRANCH):remotes/origin/$$($2_BRANCH)
	cd $5/$1 && git checkout -q --detach $$($2_SHA1)
	@[ '$$($2_SHA1)' = "$$$$(cd $5/$1 && git show -s --format='%H' HEAD)" ] || echo $$(WARNCOLOR)'==> warning: SHA1 hash did not match $1.version file'$$(ENDCOLOR)
	echo 1 > $$@
$5/$1/source-compiled: $5/$1/.git/HEAD
$$($2_SRC_FILE): | $$($2_SRC_FILE)/HEAD
	touch -c $$@

else # DEPS_GIT=0

$2_SRC_DIR := $1-$$($2_SHA1)
$2_SRC_FILE := $$(SRCCACHE)/$$($2_SRC_DIR).tar.gz
$$($2_SRC_FILE): | $$(SRCCACHE)
	$$(JLDOWNLOAD) $$@ $$(call $2_TAR_URL,$$($2_SHA1))
$5/$$($2_SRC_DIR)/source-extracted: $$($2_SRC_FILE)
	$$(JLCHECKSUM) $$<
	-[ ! \( -e $$(dir $$@) -o -h $$(dir $$@) \) ] || rm -r $$(dir $$@)
	mkdir -p $$(dir $$@)
	$(TAR) -C $$(dir $$@) --strip-components 1 -xf $$<
	echo 1 > $$@
endif # DEPS_GIT

$$(build_prefix)/manifest/$1: $$(SRCDIR)/$1.version # make the manifest stale if the version file is touched (causing re-install for compliant targets)
distclean-$1:
	-rm -rf $5/$$($2_SRC_DIR) $$($2_SRC_FILE) $$(BUILDDIR)/$$($2_SRC_DIR)
endef
