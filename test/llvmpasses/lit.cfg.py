import os
import sys
import re
import platform

import lit.util
import lit.formats

config.name = 'Julia'
config.suffixes = ['.ll','.jl']
config.test_source_root = os.path.dirname(__file__)
config.test_format = lit.formats.ShTest(False)
config.substitutions.append(('%{shlibext}', '.dylib' if platform.system() == 'Darwin' else '.dll' if
    platform.system() == 'Windows' else '.so'))

config.environment['HOME'] = "/tmp"
# Always compile instead of reusing native code from the object cache in the
# depot under HOME, which is shared with earlier runs and other builds.
config.environment['JULIA_OBJCACHE'] = "0"

if platform.machine() == "x86_64":
    config.available_features.add('x86_64')
