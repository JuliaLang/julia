import os
import sys
import re
import platform

import lit.util
import lit.formats

config.name = 'Julia-GCChecker'
config.suffixes = ['.c','.cpp']
config.test_source_root = os.path.dirname(__file__)
config.test_format = lit.formats.ShTest(False)
config.substitutions.append(('%{shlibext}', '.dylib' if platform.system() == 'Darwin' else '.dll' if
    platform.system() == 'Windows' else '.so'))
config.substitutions.append(("%{julia_home}", os.path.join(os.path.dirname(__file__), "../..")))

config.environment['HOME'] = "/tmp"

# lit's internal shell does not expand environment variables, so hand the
# compiler flags chosen by the build to the RUN lines as substitutions instead
for flags in ('CLANGSA_FLAGS', 'CLANGSA_CXXFLAGS', 'CPPFLAGS', 'CFLAGS', 'CXXFLAGS'):
    config.substitutions.append(('%{' + flags.lower() + '}', os.environ.get(flags, "")))

if platform.machine() == "x86_64":
    config.available_features.add('x86_64')
