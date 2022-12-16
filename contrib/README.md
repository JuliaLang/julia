Installation
============

| Name                           |  Description                                                |
|  ----------------------------  |  ---------------------------------------------------------  |
|[ mac/ ](https://github.com/JuliaLang/julia/blob/master/contrib/mac/) | Mac install files |
|[ windows/ ](https://github.com/JuliaLang/julia/blob/master/contrib/windows/) | Windows install files |
|[ add_license_to_files.jl ](https://github.com/JuliaLang/julia/blob/master/contrib/add_license_to_files.jl ) | Add the Julia license to files in the Julia Project |
|[ check-whitespace.jl ](https://github.com/JuliaLang/julia/blob/master/contrib/check-whitespace.jl) | Check for white space issues |
|[ commit-name.sh ](https://github.com/JuliaLang/julia/blob/master/contrib/commit-name.sh) | Computes a version name for a commit |
|[ fixup-libgfortran.sh ](https://github.com/JuliaLang/julia/blob/master/contrib/fixup-libgfortran.sh) | Include libgfortran  and libquadmath for installations |
|[ fixup-libstdc++.sh ](https://github.com/JuliaLang/julia/blob/master/contrib/fixup-libstdc++.sh) | Include libstdc++ for    installations |
|[ install.sh ](https://github.com/JuliaLang/julia/blob/master/contrib/install.sh) | Installation script with different permissions |
|[ julia.appdata.xml ](https://github.com/JuliaLang/julia/blob/master/contrib/julia.appdata.xml) | Appdata config file |
|[ julia-config.jl ](https://github.com/JuliaLang/julia/blob/master/contrib/julia-config.jl) | Determines build parameters required by an embedded Julia |
|[ julia.desktop ](https://github.com/JuliaLang/julia/blob/master/contrib/julia.desktop) | Desktop entry file |
|[ julia.png ](https://github.com/JuliaLang/julia/blob/master/contrib/julia.png) | Julia png image file |
|[ julia.svg ](https://github.com/JuliaLang/julia/blob/master/contrib/julia.svg) | Julia svg image file |
|[ relative_path.py ](https://github.com/JuliaLang/julia/blob/master/contrib/relative_path.py) | Convert absolute paths into   relative paths |
|[ stringreplace.c ](https://github.com/JuliaLang/julia/blob/master/contrib/stringreplace.c) | Replace strings to hardcoded paths in binaries during `make install` |

Packagers may want to run this command via a script after package installation.

```
if [ -e /usr/share/icons/hicolor/icon-theme.cache ]; then
  if [ -x /usr/bin/gtk-update-icon-cache ]; then
    /usr/bin/gtk-update-icon-cache -f /usr/share/icons/hicolor >/dev/null 2>&1
  fi
fi
```

Debugging
=========

| Name                           |  Description                                                |
| ------------------------------ | ----------------------------------------------------------- |
|[ debug_bootstrap.gdb ](https://github.com/JuliaLang/julia/blob/master/contrib/debug_bootstrap.gdb) | Bootstrap process using the debug build |
|[ valgrind-julia.supp ](https://github.com/JuliaLang/julia/blob/master/contrib/valgrind-julia.supp) | Suppressions for Valgrind debugging tool |
