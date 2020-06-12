# include after all other files:
# defines uninstallers and version-checks
# based on the contents of the UNINSTALL_* variables and the manifest files

install: version-check
version-check: $(addprefix version-check-, $(DEP_LIBS_STAGED))
uninstall: $(addprefix uninstall-, $(DEP_LIBS_STAGED))

## read 'uninstall-*' definition from either the manifest or the current session
define define-uninstaller
MANIFEST_$1 := $$(shell [ -e $$(build_prefix)/manifest/$1 ] && cat $$(build_prefix)/manifest/$1)
ifeq (undefined,$$(flavor $$(word 2,$$(MANIFEST_$1))))
MANIFEST_$1 := $$(UNINSTALL_$1)
endif
UNINST_HOW_$1 := $$(word 2,$$(MANIFEST_$1))
ifneq ($$(UNINST_HOW_$1),)
UNINST_WHO_$1 := $$(firstword $$(MANIFEST_$1))
UNINST_WHERE_$1 := $$(wordlist 3,99,$$(MANIFEST_$1))
$$(eval $$(call $$(UNINST_HOW_$1),$1,$$(UNINST_WHO_$1),$$(UNINST_WHERE_$1)))
endif
endef
$(foreach dep,$(DEP_LIBS_STAGED),$(eval $(call define-uninstaller,$(dep))))

# for each subproject with a manifest, keep the user aware if something is not the expected version
$(addprefix version-check-,$(DEP_LIBS_STAGED)) : version-check-% : install-%
	@if [ ! -e $(build_prefix)/manifest/$* ] || ( \
			[ "1" != "`wc -w $(build_prefix)/manifest/$* | cut -f 1 -d ' '`" ] && \
			[ "$(UNINSTALL_$*)" != "`cat $(build_prefix)/manifest/$*`" ]) ; then \
		echo "WARNING: using mismatched version for $$(cat $(build_prefix)/manifest/$*):" ; \
		echo "  want $(UNINSTALL_$*)" ; \
		echo "  To resolve this warning, you could try either of the following suggestions: " ; \
		echo "  1. Run the following command: make -C deps uninstall" ; \
		echo "  2. Remove the JULIA_HOME/usr directory, where you replace JULIA_HOME with the path of the directory in which you are building Julia " ; \
	fi
