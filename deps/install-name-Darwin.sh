#! /bin/bash

install_name_tool -id @executable_path/lib/$1 $2
