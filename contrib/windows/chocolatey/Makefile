JULIAHOME = $(abspath ../../..)
CHOCOLATEY_BUILD_DIR = $(JULIAHOME)/chocolatey_build
JULIA_VERSION = $(shell cat $(JULIAHOME)/VERSION)
JULIA_VERSION_NUMERIC_PART = $(subst -prerelease,,$(JULIA_VERSION))
BUILDNUMBER = $(shell date +%s)
JULIA_COMMIT = $(shell git rev-parse --short=10 HEAD)

all: dist-chocolatey

dist-chocolatey-clean:
	rm -fr $(CHOCOLATEY_BUILD_DIR)

dist-chocolatey: dist-chocolatey-clean
	mkdir $(CHOCOLATEY_BUILD_DIR)
	cp *.ps1 julia.nuspec $(CHOCOLATEY_BUILD_DIR)
ifeq (, $(findstring prerelease,$(JULIA_VERSION)))
	sed -i 's/VERSIONVERSION/$(JULIA_VERSION_NUMERIC_PART).$(BUILDNUMBER)/g' $(CHOCOLATEY_BUILD_DIR)/julia.nuspec
else
	sed -i 's/VERSIONVERSION/$(JULIA_VERSION_NUMERIC_PART).$(BUILDNUMBER)-prerelease$(JULIA_COMMIT)/g' $(CHOCOLATEY_BUILD_DIR)/julia.nuspec
endif
	cd $(CHOCOLATEY_BUILD_DIR) && \
	sed -i 's/COMMITCOMMIT/$(JULIA_COMMIT)/g' chocolateyInstall.ps1 && \
	sed -i 's/JULIAVERSIONJULIAVERSION/$(JULIA_VERSION)/g' chocolateyInstall.ps1 && \
	../dist-extras/nuget.exe pack -NoPackageAnalysis && \
	cp *.nupkg ../.
	rm -fr $(CHOCOLATEY_BUILD_DIR)

win-extras:
	$(JLDOWNLOAD) http://nuget.org/nuget.exe && \
	chmod a+x nuget.exe
