```@meta
EditURL = "https://github.com/JuliaLang/julia/blob/master/stdlib/FileWatching/docs/src/index.md"
```

# [File Events](@id lib-filewatching)

```@docs
poll_fd
poll_file
watch_file
watch_folder
unwatch_folder
```
```@docs
FileMonitor
FolderMonitor
PollingFileWatcher
FDWatcher
```

# Pidfile

```@meta
CurrentModule = FileWatching.Pidfile
```

A simple utility tool for creating advisory pidfiles (lock files).

## Primary Functions

```@docs
mkpidlock
trymkpidlock
close(lock::LockMonitor)
```


## Helper Functions

```@docs
Pidfile.open_exclusive
Pidfile.tryopen_exclusive
Pidfile.write_pidfile
Pidfile.parse_pidfile
Pidfile.stale_pidfile
Pidfile.isvalidpid
Base.touch(::Pidfile.LockMonitor)
```
