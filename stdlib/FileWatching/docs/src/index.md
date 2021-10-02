# [File Events](@id lib-filewatching)

## Basic File Watching
```@docs
FileWatching.poll_fd
FileWatching.poll_file
FileWatching.watch_file
FileWatching.watch_folder
FileWatching.unwatch_folder
```

## Pidfile-based File Locking

```@docs
FileWatching.mkpidlock
```

### Helper Functions
```@docs
FileWatching.open_exclusive
FileWatching.tryopen_exclusive
FileWatching.write_pidfile
FileWatching.parse_pidfile
FileWatching.stale_pidfile
FileWatching.isvalidpid
```
