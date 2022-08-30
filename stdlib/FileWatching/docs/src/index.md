# [File Events](@id lib-filewatching)

```@docs
FileWatching.poll_fd
FileWatching.poll_file
FileWatching.watch_file
FileWatching.watch_folder
FileWatching.unwatch_folder
```

# Pidfile

```@meta
CurrentModule = FileWatching.Pidfile
```

A simple utility tool for creating advisory pidfiles (lock files).

## Primary Functions

```@docs
mkpidlock
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
