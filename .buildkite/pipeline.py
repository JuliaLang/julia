import sys

import yaml

DOCKER_PLUGIN = "docker#v3.7.0"
IS_MASTER = "build.pull_request.id == null && build.branch == \"master\""
JULIA_QUEUE = "julia"
STAGING_BUCKET = "cdg-buildkite-staging"  # TODO: Change this.
TESTER_REPO = "degraafc/julia-buildkite:tester-linux-x86_64"  # TODO: Fix this.
PACKAGE_REPO = "staticfloat/julia_workerbase"
PACKAGE_TAGS = {
    ("linux", "aarch64"): "debian8-aarch64",
    ("linux", "armv7l"): "debian8-armv7l",
    ("linux", "i686"): "debian8_9-i686",
    ("linux", "ppc64le"): "debian9-ppc64le",
    ("linux", "x86_64"): "centos6_9-x86_64",
    ("musl", "x86_64"): "alpine3_8-x86_64",
}


def _agents(os, arch):
    return {
        "os": os,
        "arch": arch,
        "queue": JULIA_QUEUE,
    }


def _linux_package_docker(arch):
    return {
        DOCKER_PLUGIN: {
            "image": "{}:{}".format(PACKAGE_REPO, PACKAGE_TAGS[("linux", arch)]),
            "propagate-uid-gid": True,
            "propagate-environment": True,
            "environment": [
                "CCACHE_DIR=/tmp/ccache",
            ],
            "volumes": [
                "/var/lib/buildkite-agent/ccache:/tmp/ccache",
                "/var/lib/buildkite-agent/srccache:/tmp/srccache",
            ],
        },
    }


def _linux_promote_docker(os, arch):
    return {
        DOCKER_PLUGIN: {
            "image": "amazon/aws-cli",
            "entrypoint": "/bin/bash",
            "propagate-environment": True,
            "environment": [
                "STAGING_BUCKET={}".format(STAGING_BUCKET),
                "PROMOTE_OS={}".format(os),
                "PROMOTE_ARCH={}".format(arch),
            ],
        },
    }


def make_package(os, arch):
    step = {
        "label": "Package: {} {}".format(os, arch),
        "command": "bash .buildkite/package.sh",
        "key": "package-{}-{}".format(os, arch),
        "agents": _agents(os, arch),
    }
    if os == "mac":
        step["env"] = {
            "APPLEID": "staticfloat@gmail.com"
        }
    if os in ["linux", "musl"]:
        step["plugins"] = [
            _linux_package_docker(arch),
        ]
    return step


def make_tester(os, arch):
    step = {
        "label": "Tester: {} {}".format(os, arch),
        "command": "bash .buildkite/tester.sh",
        "key": "tester-{}-{}".format(os, arch),
        "agents": _agents(os, arch),
        "depends_on": [
            "package-{}-{}".format(os, arch),
        ],
    }
    rr = os == "linux" and arch in ["x86_64", "i686"]
    if rr or os == "winnt":
        step["artifacts"] = [
            "dumps/*"
        ]
    if os in ["linux", "musl"]:
        docker = {
            "image": TESTER_REPO,  # TODO per-arch
            "propagate-uid-gid": True,
            "propagate-environment": True,
        }
        if rr:
            docker["security-opts"] = [
                "seccomp=.buildkite/rr_profile.json",
            ]
            docker["add-caps"] = [
                "SYS_PTRACE",
            ]
        step["plugins"] = [
            {
                DOCKER_PLUGIN: docker,
            },
        ]
    return step


def make_promote(os, arch):
    return {
        "label": "Promote: {} {}".format(os, arch),
        "command": ".buildkite/promote.sh",
        "key": "promote-{}-{}".format(os, arch),
        "if": IS_MASTER,
        "agents": _agents("linux", "x86_64"),
        "depends_on": [
            "tester-{}-{}".format(os, arch),
        ],
        "plugins": [
            _linux_promote_docker(os, arch),
        ],
    }


# def make_analyze_gc():
#     return {
#         "label": "Analyze GC",
#         "command": "bash .buildkite/analyze_gc.sh",
#         "agents": _agents("linux", "x86_64"),
#         "plugins": [
#             _linux_package_docker("x86_64"),
#         ],
#     }


# def make_doctest():
#     return {
#         "label": "Doctest",
#         "command": "bash .buildkite/doctest.sh",
#         "key": "doctest",
#         "agents": _agents("linux", "x86_64"),
#         "plugins": [
#             _linux_package_docker("x86_64"),
#         ],
#     }


# def make_promote_doctest():
#     return {
#         "label": "Promote: Doctest",
#         "command": ".buildkite/promote_doctest.sh",
#         "key": "promote-doctest",
#         "if": IS_MASTER,
#         "agents": _agents("linux", "x86_64"),
#         "depends_on": [
#             "doctest",
#         ],
#         "plugins": [
#             _linux_promote_docker("linux", "x86_64"),
#         ],
#     }


# def make_llvm_passes():
#     return {
#         "label": "LLVM Passes",
#         "command": "bash .buildkite/llvm_passes.sh",
#         "agents": _agents("linux", "x86_64"),
#         "plugins": [
#             _linux_package_docker("x86_64"),
#         ],
#     }


# def make_whitespace():
#     return {
#         "label": "Whitespace",
#         "command": "bash .buildkite/whitespace.sh",
#         "agents": _agents("linux", "x86_64"),
#         "plugins": [
#             _linux_package_docker("x86_64"),
#         ],
#     }


if __name__ == "__main__":
    pairs = [
        # ("freebsd", "x86_64"),
        # ("linux", "aarch64"),
        # ("linux", "armv7l"),
        # ("linux", "i686"),
        # ("linux", "ppc64le"),
        ("linux", "x86_64"),
        # ("mac", "x86_64"),
        # ("musl", "x86_64"),
        # ("winnt", "i686"),
        # ("winnt", "x86_64"),
    ]
    steps = []
    for os, arch in pairs:
        steps.append(make_package(os, arch))
        steps.append(make_tester(os, arch))
        steps.append(make_promote(os, arch))
    # steps.append(make_analyze_gc())
    # steps.append(make_doctest())
    # steps.append(make_promote_doctest())
    # steps.append(make_llvm_passes())
    # steps.append(make_whitespace())
    yaml.dump(steps, sys.stdout)
