JULIAHOME = $(abspath ../../..)
CHOCOLATEY_BUILD_DIR = $(JULIAHOME)/chocolatey_build
JULIA_VERSION = $(shell cat $(JULIAHOME)/VERSION)
JULIA_VERSION_NUMERIC_PART = $(subst -prerelease,,$(JULIA_VERSION))
BUILDNUMBER = $(shell date +%s)
JULIA_COMMIT_SHORT = $(shell git rev-parse --short=7 HEAD)

all: dist-chocolatey

dist-chocolatey-clean:
	rm -fr $(CHOCOLATEY_BUILD_DIR)

dist-chocolatey: dist-chocolatey-clean
	mkdir $(CHOCOLATEY_BUILD_DIR)
	cp *.ps1 julia.nuspec $(CHOCOLATEY_BUILD_DIR)
ifeq (, $(findstring prerelease,$(JULIA_VERSION)))
	cd $(CHOCOLATEY_BUILD_DIR) && \
	sed -i 's/VERSIONVERSION/$(JULIA_VERSION_NUMERIC_PART).$(BUILDNUMBER)/g' julia.nuspec && \
	sed -i 's/VERSIONVERSION/$(JULIA_VERSION)/g' chocolateyInstall.ps1
else
	cd $(CHOCOLATEY_BUILD_DIR) && \
	sed -i 's/VERSIONVERSION/$(JULIA_VERSION_NUMERIC_PART).$(BUILDNUMBER)-pre-$(JULIA_COMMIT_SHORT)/g' julia.nuspec && \
	sed -i 's/VERSIONVERSION/$(JULIA_VERSION)-$(JULIA_COMMIT_SHORT)/g' chocolateyInstall.ps1
endif
	cd $(CHOCOLATEY_BUILD_DIR) && \
	../dist-extras/nuget.exe pack -NoPackageAnalysis && \
	cp *.nupkg ../.
	rm -fr $(CHOCOLATEY_BUILD_DIR)

win-extras:
		$(JLDOWNLOAD) http://nuget.org/nuget.exe && \
	chmod a+x nuget.exe
