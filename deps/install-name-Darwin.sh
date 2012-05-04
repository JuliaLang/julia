#! /bin/bash

install_name_tool -id @executable_path/usr/lib/$1 $2
