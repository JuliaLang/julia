# Apple's documentation gives poor guidance on how to set CFBundleVersion
# because it's documented differently depending on the age and producer of the
# docs page.  It seems modern docs refer to it as a build string unique to a
# released or unreleased iteration of the bundle.  The common theme is a string
# made of up to three unsigned integers separated by a period.  We derive a
# hopefully unique build string from the Git DAG.
ifeq ($(NO_GIT), 1)
ifeq ($(BUNDLE_VERS),)
$(error If NO_GIT=1, BUNDLE_VERS must be provided)
endif
endif
GIT_MASTER_COUNT:=$(shell git rev-list $(shell git merge-base HEAD master) --count)
GIT_BRANCH_COUNT:=$(shell git rev-list $(shell git merge-base HEAD master)..HEAD --count)
# Use commit time as final disambiguator.
GIT_CTIME:=$(shell git log -1 --pretty=format:%ct)
ifeq ($(GIT_MASTER_COUNT),)
GIT_MASTER_COUNT=0
endif
ifeq ($(GIT_BRANCH_COUNT),)
GIT_BRANCH_COUNT=0
endif
ifeq ($(GIT_CTIME),)
GIT_CTIME=0
endif
BUNDLE_VERS?=$(GIT_MASTER_COUNT).$(GIT_BRANCH_COUNT).$(GIT_CTIME)
