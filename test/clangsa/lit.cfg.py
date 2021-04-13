import os
import sys
import re
import platform

import lit.util
import lit.formats

config.name = 'Julia-GCChecker'
config.suffixes = ['.c','.cpp']
config.test_source_root = os.path.dirname(__file__)
config.test_format = lit.formats.ShTest(True)
config.substitutions.append(('%shlibext', '.dylib' if platform.system() == 'Darwin' else '.dll' if
    platform.system() == 'Windows' else '.so'))
config.substitutions.append(("%julia_home", os.path.join(os.path.dirname(__file__), "../..")))

path = os.path.pathsep.join((os.path.join(os.path.dirname(__file__),"../../usr/tools"), os.path.join(os.path.dirname(__file__),"../../usr/bin"), config.environment['PATH']))
config.environment['PATH'] = path
config.environment['HOME'] = "/tmp"
config.environment['CLANGSA_FLAGS'] = os.environ.get('CLANGSA_FLAGS', "")
config.environment['CLANGSA_CXXFLAGS'] = os.environ.get('CLANGSA_CXXFLAGS', "")
config.environment['CPPFLAGS'] = os.environ.get('CPPFLAGS', "")
config.environment['CFLAGS'] = os.environ.get('CFLAGS', "")
config.environment['CXXFLAGS'] = os.environ.get('CXXFLAGS', "")

if platform.machine() == "x86_64":
    config.available_features.add('x86_64')
