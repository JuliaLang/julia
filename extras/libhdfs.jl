## file control flags ##
#Int32 O_WRONLY=1
#Int32 O_RDONLY=0
#Int32 O_RDWR=2
#Int32 O_CREAT=64
#Int32 O_EXCL=128
#Int32 O_APPEND=1024
#Int32 O_TRUNC=512
#Int32 O_NONBLOCK=2048


#allows ASCIIStrings to be passed to hdfsWrite
convert(::Type{Ptr{Void}},x::ASCIIString)=convert(Ptr{Void},convert(Ptr{Uint8},x))



libhdfs=dlopen("libhdfs")
_hdfsConnectAsUser= dlsym(libhdfs, :hdfsConnectAsUser)
_hdfsConnect= dlsym(libhdfs, :hdfsConnect)
_hdfsDisconnect= dlsym(libhdfs, :hdfsDisconnect)
_hdfsOpenFile= dlsym(libhdfs,:hdfsOpenFile)
_hdfsCloseFile= dlsym(libhdfs, :hdfsCloseFile)
_hdfsExists= dlsym(libhdfs, :hdfsExists)
_hdfsSeek= dlsym(libhdfs, :hdfsSeek)
_hdfsTell= dlsym(libhdfs, :hdfsTell)
_hdfsRead= dlsym(libhdfs, :hdfsRead)
_hdfsPread= dlsym(libhdfs, :hdfsPread)
_hdfsWrite= dlsym(libhdfs, :hdfsWrite)
_hdfsFlush= dlsym(libhdfs, :hdfsFlush)
_hdfsAvailable= dlsym(libhdfs, :hdfsAvailable)
_hdfsCopy= dlsym(libhdfs, :hdfsCopy)
_hdfsMove= dlsym(libhdfs, :hdfsMove)
_hdfsDelete= dlsym(libhdfs, :hdfsDelete)
_hdfsRename= dlsym(libhdfs, :hdfsRename)
_hdfsGetWorkingDirectory= dlsym(libhdfs, :hdfsGetWorkingDirectory)
_hdfsSetWorkingDirectory= dlsym(libhdfs, :hdfsSetWorkingDirectory)
_hdfsCreateDirectory= dlsym(libhdfs, :hdfsCreateDirectory)
_hdfsSetReplication= dlsym(libhdfs, :hdfsSetReplication)
_hdfsListDirectory=dlsym(libhdfs, :hdfsListDirectory)
_hdfsGetPathInfo=dlsym(libhdfs, :hdfsGetPathInfo)
_hdfsFreeFileInfo=dlsym(libhdfs, :hdfsFreeFileInfo)
_hdfsGetHosts=dlsym(libhdfs, :hdfsGetHosts)
_hdfsFreeHosts=dlsym(libhdfs, :hdfsFreeHosts)
_hdfsGetDefaultBlockSize= dlsym(libhdfs, :hdfsGetDefaultBlockSize)
_hdfsGetCapacity= dlsym(libhdfs, :hdfsGetCapacity)
_hdfsGetUsed= dlsym(libhdfs, :hdfsGetUsed)
_hdfsChown=dlsym(libhdfs, :hdfsChown)
_hdfsChmod=dlsym(libhdfs, :hdfsChmod)
_hdfsUtime=dlsym(libhdfs, :hdfsUtime)


## libhdfs functions as ccalls ##
# most things are passed around as Ptr{Void}
# strings can be passed to Ptr{Uint8} (char *)
# buffers can be made by allocating an array of Uint8, then read with cstring (see example with gethostname in the Julia Manual's Calling C and Fortran Code)
# most functions will segfault if passed anything unexpected as fs
# commented functions have not been tried out

hdfsConnectAsUser(host, port, user)=ccall(_hdfsConnectAsUser, Ptr{Void}, (Ptr{Uint8},Int32,Ptr{Uint8}),host,port,user)

hdfsConnect(host,port)=ccall(_hdfsConnect, Ptr{Void}, (Ptr{Uint8},Int32),host,port)

hdfsDisconnect(fs)=ccall(_hdfsDisconnect, Int32, (Ptr{Void},),fs)

# file control flags need to be done manually
hdfsOpenFile(fs,path,flags,bufferSize,replication,blocksize)=ccall(_hdfsOpenFile, Ptr{Void}, (Ptr{Void},Ptr{Uint8},Int32,Int32,Int16,Int32),fs,path,flags,bufferSize,replication,blocksize)

hdfsCloseFile(fs, file)=ccall(_hdfsCloseFile,Int32,(Ptr{Void},Ptr{Void}),fs,file)

hdfsExists(fs, path)=ccall(_hdfsExists,Int32,(Ptr{Void},Ptr{Uint8}),fs,path)

hdfsSeek(fs, file, desiredPos)=ccall(_hdfsSeek,Int32,(Ptr{Void},Ptr{Void},Int64),fs, file, desiredPos)

hdfsTell(fs, file)=ccall(_hdfsTell,Int64,(Ptr{Void},Ptr{Void}), fs, file)

hdfsRead(fs, file, buffer, length)=ccall(_hdfsRead,Int32, (Ptr{Void},Ptr{Void},Ptr{Void},Int32), fs, file, buffer, length)

hdfsPread(fs, file, position, buffer, length)=ccall(_hdfsPread,Int32, (Ptr{Void},Ptr{Void},Int64,Ptr{Void},Int32), fs, file, position, buffer, length)

hdfsWrite(fs, file, buffer, length)=ccall(_hdfsWrite,Int32,(Ptr{Void},Ptr{Void},Ptr{Void},Int32),fs, file, buffer, length)

hdfsFlush(fs, file)=ccall(_hdfsFlush, Int32, (Ptr{Void},Ptr{Void}), fs, file)

hdfsAvailable(fs, file)=ccall(_hdfsAvailable, Int32, (Ptr{Void},Ptr{Void}),fs ,file)

hdfsCopy(srcFS, src, dstFS, dst)=ccall(_hdfsCopy, Int32, (Ptr{Void},Ptr{Uint8},Ptr{Void},Ptr{Uint8}), srcFS, src, dstFS, dst)

hdfsMove(srcFS, src, dstFS, dst)=ccall(_hdfsMove, Int32, (Ptr{Void},Ptr{Uint8},Ptr{Void},Ptr{Uint8}), srcFS, src, dstFS, dst)

hdfsDelete(fs, path)=ccall(_hdfsDelete, Int32, (Ptr{Void},Ptr{Uint8}), fs, path)

hdfsRename(fs, oldPath, newPath)=ccall(_hdfsRename, Int32, (Ptr{Void}, Ptr{Uint8},Ptr{Uint8}), fs, oldPath, newPath)

hdfsGetWorkingDirectory(fs, buffer, bufferSize)=ccall(_hdfsGetWorkingDirectory, Ptr{Uint8}, (Ptr{Void}, Ptr{Uint8}, Int32), fs, buffer, bufferSize)

hdfsSetWorkingDirectory(fs, path)=ccall(_hdfsSetWorkingDirectory, Int32, (Ptr{Void}, Ptr{Uint8}), fs, path)

hdfsCreateDirectory(fs, path)=ccall(_hdfsCreateDirectory, Int32, (Ptr{Void}, Ptr{Uint8}), fs, path)

hdfsSetReplication(fs, path, replication)=ccall(_hdfsSetReplication, Int32, (Ptr{Void}, Ptr{Uint8}, Int16), fs, path, replication)

#hdfsListDirectory(fs, path, numEntries)=ccall(_hdfsListDirectory, Ptr{Void}, (Ptr{Void}, Ptr{Uint8}, Int32), fs, path, numEntries)

#hdfsGetPathInfo(fs, path)=ccall(_hdfsGetPathInfo, Ptr{Void}, (Ptr{Void}, Ptr{Uint8}), fs, path)

#should be passed a pointed to hdfsFileInfo
#hdfsFreeFileInfo(hdfsFileInfo, numEntries)=ccall(_hdfsFreeFileInfo, Void, (Ptr{Ptr{Void}}, Int32), hdfsFileInfo, numEntries)

#hdfsGetHosts(fs, path, start, length)=ccall(_hdfsGetHosts, Ptr{Ptr{Ptr{Uint8}}}, (Ptr{Void}, Ptr{Uint8}, Int64, Int64), fs, path, start, length)

#hdfsFreeHosts(blockHosts)=ccall(_hdfsFreeHosts, Void, (Ptr{Ptr{Ptr{Uint8}}},), blockHosts)

hdfsGetDefaultBlockSize(fs)=ccall(_hdfsGetDefaultBlockSize, Int64, (Ptr{Void},),fs)

hdfsGetCapacity(fs)=ccall(_hdfsGetCapacity, Int64, (Ptr{Void},),fs)

hdfsGetUsed(fs)=ccall(_hdfsGetUsed, Int64, (Ptr{Void},),fs)

#hdfsChown(fs, path, owner, group)=ccall(_hdfsChown, Int32, (Ptr{Void}, Ptr{Uint8}, Ptr{Uint8}, Ptr{Uint8}), fs, path, owner, group)

#hdfsChmod(fs, path, mode)=ccall(_hdfsChmod, Int32, (Ptr{Void}, Ptr{Uint8}, Int16), fs, path, mode)

#last two arguments are to be time_t
#hdfsUtime(fs, path, mtime, atime)=ccall(_hdfsUtime, Int32, (Ptr{Void}, Ptr{Uint8}, int, int), fs, path, mtime, atime)
