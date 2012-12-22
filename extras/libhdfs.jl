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

## used to enforce typing ##

type HdfsFS
  ptr::Ptr{Void}
end

type HdfsFile
  ptr::Ptr{Void}
end

type HdfsFileInfo
  ptr::Ptr{Void}
end


## libhdfs functions as ccalls ##
# strings can be passed to Ptr{Uint8} (char *)
# functions segfault if passed bad hdfsFS arguments
# multiple dispatch is used for convenience
# commented functions have not been tried out

function hdfs_connect_as_user(host, port, user)
  fs = ccall(_hdfsConnectAsUser, Ptr{Void}, (Ptr{Uint8},Int32,Ptr{Uint8}),host,port,user)
  return HdfsFS(fs)
end
hdfs_connect_as_user(host, port)=hdfs_connect(host, port)
hdfs_connect_as_user()=hdfs_connect()

function hdfs_connect(host,port)
  fs = ccall(_hdfsConnect, Ptr{Void}, (Ptr{Uint8},Int32),host,port)
  return HdfsFS(fs)
end
function hdfs_connect()
  fs = ccall(_hdfsConnect, Ptr{Void}, (Ptr{Uint8},Int32),"default",0)
  return HdfsFS(fs)
end

hdfs_disconnect(fs::HdfsFS)=ccall(_hdfsDisconnect, Int32, (Ptr{Void},),fs.ptr)

# file control flags need to be done manually
function hdfs_open_file(fs::HdfsFS,path,flags,bufferSize,replication,blocksize)
  file = ccall(_hdfsOpenFile, Ptr{Void}, (Ptr{Void},Ptr{Uint8},Int32,Int32,Int16,Int32),fs.ptr,path,flags,bufferSize,replication,blocksize)
  return HdfsFile(file)
end
function hdfs_open_file(fs::HdfsFS,path,flags)
  file = ccall(_hdfsOpenFile, Ptr{Void}, (Ptr{Void},Ptr{Uint8},Int32,Int32,Int16,Int32),fs.ptr,path,flags,0,0,0)
  return HdfsFile(file)
end

hdfs_close_file(fs::HdfsFS, file::HdfsFile)=ccall(_hdfsCloseFile,Int32,(Ptr{Void},Ptr{Void}),fs.ptr,file.ptr)

hdfs_exists(fs::HdfsFS, path)=ccall(_hdfsExists,Int32,(Ptr{Void},Ptr{Uint8}),fs.ptr,path)

hdfs_seek(fs::HdfsFS, file::HdfsFile, desiredPos)=ccall(_hdfsSeek,Int32,(Ptr{Void},Ptr{Void},Int64),fs.ptr, file.ptr, desiredPos)

hdfs_tell(fs::HdfsFS, file::HdfsFile)=ccall(_hdfsTell,Int64,(Ptr{Void},Ptr{Void}), fs.ptr, file.ptr)

hdfs_read(fs::HdfsFS, file::HdfsFile, buffer, length)=ccall(_hdfsRead,Int32, (Ptr{Void},Ptr{Void},Ptr{Void},Int32), fs.ptr, file.ptr, buffer, length)
function hdfs_read(fs::HdfsFS, file::HdfsFile, length)
  buffer = Array(Uint8,length) 
  r = ccall(_hdfsRead,Int32, (Ptr{Void},Ptr{Void},Ptr{Void},Int32), fs.ptr, file.ptr, buffer, length)
  if r==-1
    error("-1")
  else
    print("read ",r," bytes\n")
    return buffer
  end
end

hdfs_pread(fs::HdfsFS, file::HdfsFile, position, buffer, length)=ccall(_hdfsPread,Int32, (Ptr{Void},Ptr{Void},Int64,Ptr{Void},Int32), fs.ptr, file.ptr, position, buffer, length)
function hdfs_pread(fs::HdfsFS, file::HdfsFile, position, length)
  buffer = Array(Uint8,length) 
  r = ccall(_hdfsPread,Int32, (Ptr{Void},Ptr{Void},Ptr{Void},Int32), fs.ptr, file.ptr, buffer, position, length)
  if r==-1
    error("-1")
  else
    print("read ",r," bytes\n")
    return buffer
  end
end

#can be passed an ASCIIString (length not necessary in that case)
hdfs_write(fs::HdfsFS, file::HdfsFile, buffer, length)=ccall(_hdfsWrite,Int32,(Ptr{Void},Ptr{Void},Ptr{Void},Int32),fs.ptr, file.ptr, buffer, length)
hdfs_write(fs::HdfsFS, file::HdfsFile, buffer::ASCIIString, length)=ccall(_hdfsWrite,Int32,(Ptr{Void},Ptr{Void},Ptr{Void},Int32),fs.ptr, file.ptr, convert(Ptr{Uint8},buffer), length)
hdfs_write(fs::HdfsFS, file::HdfsFile, buffer::ASCIIString)=ccall(_hdfsWrite,Int32,(Ptr{Void},Ptr{Void},Ptr{Void},Int32),fs.ptr, file.ptr, convert(Ptr{Uint8},buffer), length(buffer))

hdfs_flush(fs::HdfsFS, file::HdfsFile)=ccall(_hdfsFlush, Int32, (Ptr{Void},Ptr{Void}), fs.ptr, file.ptr)

hdfs_available(fs::HdfsFS, file::HdfsFile)=ccall(_hdfsAvailable, Int32, (Ptr{Void},Ptr{Void}), fs.ptr, file.ptr)

hdfs_copy(srcFS::HdfsFS, src, dstFS::HdfsFS, dst)=ccall(_hdfsCopy, Int32, (Ptr{Void},Ptr{Uint8},Ptr{Void},Ptr{Uint8}), srcFS.ptr, src, dstFS.ptr, dst)

hdfs_move(srcFS::HdfsFS, src, dstFS::HdfsFS, dst)=ccall(_hdfsMove, Int32, (Ptr{Void},Ptr{Uint8},Ptr{Void},Ptr{Uint8}), srcFS.ptr, src, dstFS.ptr, dst)

hdfs_delete(fs::HdfsFS, path)=ccall(_hdfsDelete, Int32, (Ptr{Void},Ptr{Uint8}), fs.ptr, path)

hdfs_rename(fs::HdfsFS, oldPath, newPath)=ccall(_hdfsRename, Int32, (Ptr{Void}, Ptr{Uint8},Ptr{Uint8}), fs.ptr, oldPath, newPath)

hdfs_get_working_directory(fs::HdfsFS, buffer, bufferSize)=ccall(_hdfsGetWorkingDirectory, Ptr{Uint8}, (Ptr{Void}, Ptr{Uint8}, Int32), fs.ptr, buffer, bufferSize)
function hdfs_get_working_directory(fs::HdfsFS,bufferSize)
  buffer = Array(Uint8, bufferSize)
  path = ccall(_hdfsGetWorkingDirectory, Ptr{Uint8}, (Ptr{Void}, Ptr{Uint8}, Int32), fs.ptr, buffer, bufferSize)
  return bytestring(path)
end
function hdfs_get_working_directory(fs::HdfsFS)
  buffer = Array(Uint8, 128)
  path = ccall(_hdfsGetWorkingDirectory, Ptr{Uint8}, (Ptr{Void}, Ptr{Uint8}, Int32), fs.ptr, buffer, 128)
  return bytestring(path)
end

hdfs_set_working_directory(fs::HdfsFS, path)=ccall(_hdfsSetWorkingDirectory, Int32, (Ptr{Void}, Ptr{Uint8}), fs.ptr, path)

hdfs_create_directory(fs::HdfsFS, path)=ccall(_hdfsCreateDirectory, Int32, (Ptr{Void}, Ptr{Uint8}), fs.ptr, path)

hdfs_set_replication(fs::HdfsFS, path, replication)=ccall(_hdfsSetReplication, Int32, (Ptr{Void}, Ptr{Uint8}, Int16), fs.ptr, path, replication)

#function hdfs_list_directory(fs::HdfsFS, path, numEntries)
#  fileInfo = ccall(_hdfsListDirectory, Ptr{Void}, (Ptr{Void}, Ptr{Uint8}, Int32), fs.ptr, path, numEntries)
#  return HdfsFileInfo(fileInfo)
#end

#function hdfs_get_path_info(fs::HdfsFS, path)
# fileInfo = ccall(_hdfsGetPathInfo, Ptr{Void}, (Ptr{Void}, Ptr{Uint8}), fs.ptr, path)
# return HdfsFileInfo(fileInfo)
#end

#hdfs_free_file_info(fileInfo::HdfsFileInfo, numEntries)=ccall(_hdfsFreeFileInfo, Void, (Ptr{Void}, Int32), fileInfo.ptr, numEntries)

#hdfs_get_hosts(fs::HdfsFS, path, start, length)=ccall(_hdfsGetHosts, Ptr{Ptr{Ptr{Uint8}}}, (Ptr{Void}, Ptr{Uint8}, Int64, Int64), fs.ptr, path, start, length)

#hdfs_free_hosts(blockHosts)=ccall(_hdfsFreeHosts, Void, (Ptr{Ptr{Ptr{Uint8}}},), blockHosts)

hdfs_get_default_block_size(fs::HdfsFS)=ccall(_hdfsGetDefaultBlockSize, Int64, (Ptr{Void},),fs.ptr)

hdfs_get_capacity(fs::HdfsFS)=ccall(_hdfsGetCapacity, Int64, (Ptr{Void},),fs.ptr)

hdfs_get_used(fs::HdfsFS)=ccall(_hdfsGetUsed, Int64, (Ptr{Void},),fs.ptr)

#hdfs_chown(fs::HdfsFS, path, owner, group)=ccall(_hdfsChown, Int32, (Ptr{Void}, Ptr{Uint8}, Ptr{Uint8}, Ptr{Uint8}), fs.ptr, path, owner, group)

#hdfs_chmod(fs::HdfsFS, path, mode)=ccall(_hdfsChmod, Int32, (Ptr{Void}, Ptr{Uint8}, Int16), fs.ptr, path, mode)

#last two arguments are to be time_t (system dependent)
#hdfs_utime(fs::HdfsFS, path, mtime, atime)=ccall(_hdfsUtime, Int32, (Ptr{Void}, Ptr{Uint8}, int, int), fs.ptr, path, mtime, atime)



## file control flags ##
# these are system-dependent
#Int32 O_WRONLY=1
#Int32 O_RDONLY=0
#Int32 O_RDWR=2
#Int32 O_CREAT=64
#Int32 O_EXCL=128
#Int32 O_APPEND=1024
#Int32 O_TRUNC=512
#Int32 O_NONBLOCK=2048
