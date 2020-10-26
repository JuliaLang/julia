import os
import sys
import re
import platform

import lit.util
import lit.formats

config.name = 'Julia'
config.suffixes = ['.ll','.jl']
config.test_source_root = os.path.dirname(__file__)
config.test_format = lit.formats.ShTest(True)
config.substitutions.append(('%shlibext', '.dylib' if platform.system() == 'Darwin' else '.dll' if
    platform.system() == 'Windows' else '.so'))

path = os.path.pathsep.join((os.path.join(os.path.dirname(__file__),"../../usr/tools"), os.path.join(os.path.dirname(__file__),"../../usr/bin"), config.environment['PATH']))
config.environment['PATH'] = path
config.environment['HOME'] = "/tmp"

if platform.machine() == "x86_64":
    config.available_features.add('x86_64')
