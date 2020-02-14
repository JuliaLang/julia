using Base.Filesystem
using Dates

const TIMEOUT = 2*60*60 # seconds

const HANDLE = Ptr{Cvoid}
const LPVOID = Ptr{Cvoid}
const DWORD = UInt32
const BOOL = Cint
const UINT = Cuint
const JOBOBJECTINFOCLASS = Cint

function CloseHandle(handle)
    Base.windowserror(:CloseHandle,
        0 == ccall(:CloseHandle, stdcall, Cint, (HANDLE,), handle))
    nothing
end
CloseHandle(handle::File) = CloseHandle(handle.handle)

struct WindowsJobObject
    handle::HANDLE
end
Base.cconvert(::Type{HANDLE}, job::WindowsJobObject) = job.handle

struct SECURITY_ATTRIBUTES
    nLength::DWORD
    lpSecurityDescriptor::LPVOID
    bInheritHandle::Cint
end

function CreateJobObject(; sec_attrs = C_NULL, name = C_NULL)
    job = ccall((:CreateJobObjectA, "kernel32"), stdcall, HANDLE, (Ptr{SECURITY_ATTRIBUTES}, Cwstring), sec_attrs, name)
    Base.windowserror(:CreateJobObject, job == C_NULL)
    WindowsJobObject(job)
end

function TerminateJobObject(job, code=1)
    Base.windowserror(:TerminateJobObject,
        ccall((:TerminateJobObject, "kernel32"), BOOL,
            (HANDLE, UINT), job, code) == 0)
    nothing
end

const JobObjectBasicProcessIdList = 3

function QueryJobObjectBasicProcessIdList(job)
    nprocesses = 1024

    while true
        data = Vector{UInt8}(undef, 2 * sizeof(DWORD) + nprocesses * sizeof(UInt))
        outsize = Ref{DWORD}(sizeof(data))

        Base.windowserror(:QueryInformationJobObject, 0 ==
            ccall((:QueryInformationJobObject, "kernel32"), stdcall, BOOL,
                (HANDLE, JOBOBJECTINFOCLASS, Ptr{Cvoid}, DWORD, Ref{DWORD}),
                job, JobObjectBasicProcessIdList, data, sizeof(data), outsize))

        header_end = 2sizeof(DWORD)
        NumberOfAssignedProcesses, NumberOfProcessIdsInList = reinterpret(DWORD, data[1:header_end])
        if NumberOfProcessIdsInList < NumberOfAssignedProcesses
            # Give a bit of margin for new processes to be spawned
            nprocesses = NumberOfAssignedProcesses + 100
            continue
        end

        return reinterpret(UInt, data[header_end+1:end])[1:NumberOfProcessIdsInList]
    end
end

function AssignProcessToJobObject(job::WindowsJobObject, process)
    Base.windowserror(:AssignProcessToJobObject,
        0 == ccall((:AssignProcessToJobObject, "kernel32"), stdcall, BOOL, (HANDLE, HANDLE), job, process))
end

const MINIDUMP_TYPE = Cint
const MiniDumpNormal = 0x00000000
const MiniDumpWithDataSegs = 0x00000001
const MiniDumpWithFullMemory = 0x00000002
const MiniDumpWithHandleData = 0x00000004
const MiniDumpFilterMemory = 0x00000008
const MiniDumpScanMemory = 0x00000010
const MiniDumpWithUnloadedModules = 0x00000020
const MiniDumpWithIndirectlyReferencedMemory = 0x00000040
const MiniDumpFilterModulePaths = 0x00000080
const MiniDumpWithProcessThreadData = 0x00000100
const MiniDumpWithPrivateReadWriteMemory = 0x00000200
const MiniDumpWithoutOptionalData = 0x00000400
const MiniDumpWithFullMemoryInfo = 0x00000800
const MiniDumpWithThreadInfo = 0x00001000
const MiniDumpWithCodeSegs = 0x00002000
const MiniDumpWithoutAuxiliaryState = 0x00004000
const MiniDumpWithFullAuxiliaryState = 0x00008000
const MiniDumpWithPrivateWriteCopyMemory = 0x00010000
const MiniDumpIgnoreInaccessibleMemory = 0x00020000
const MiniDumpWithTokenInformation = 0x00040000
const MiniDumpWithModuleHeaders = 0x00080000
const MiniDumpFilterTriage = 0x00100000
const MiniDumpValidTypeFlags = 0x001fffff

const PROCESS_QUERY_INFORMATION = 0x0400
const PROCESS_VM_READ           = 0x0010
const PROCESS_DUP_HANDLE        = 0x0040
const PROCESS_TERMINATE         = 0x0001
const PROCESS_SET_QUOTA         = 0x0100

function OpenProcess(id::Integer, rights = PROCESS_QUERY_INFORMATION | PROCESS_VM_READ | PROCESS_DUP_HANDLE)
    proc = ccall((:OpenProcess, "kernel32"), stdcall, HANDLE, (DWORD, BOOL, DWORD),
        rights,
        false, id)
    Base.windowserror(:OpenProcess, proc == C_NULL)
    proc
end

function CreateMinidump(id::UInt; filename = "dump-$id-$(Dates.format(now(), dateformat"yyyy-mm-dd_HH_MM_SS")).dmp", kind = MiniDumpWithProcessThreadData)
    proc_handle = OpenProcess(id)
    file_handle = Filesystem.open(filename, JL_O_CREAT | JL_O_WRONLY, 0666)

    Base.windowserror(:MiniDumpWriteDump, 0 ==
        ccall((:MiniDumpWriteDump, "dbghelp"), stdcall, BOOL,
            (HANDLE, DWORD, HANDLE, MINIDUMP_TYPE,
                Ptr{Cvoid},
                Ptr{Cvoid},
                Ptr{Cvoid}),
            proc_handle, id, file_handle.handle, kind,
            C_NULL, C_NULL, C_NULL))

    CloseHandle(file_handle)
    CloseHandle(proc_handle)
end

if isempty(ARGS)
    println(stderr, "Usage: autodump.jl [command...]")
    exit(2)
end

proc = run(`$ARGS`, (stdin, stdout, stderr); wait=false)
job = CreateJobObject()
AssignProcessToJobObject(job, OpenProcess(getpid(proc), PROCESS_SET_QUOTA | PROCESS_TERMINATE))

@async begin
    sleep(TIMEOUT)
    try
        if isopen(proc)
            println(stderr, "\n\nProcess timed out. Creating core dump for each running process!")
            proc_ids = QueryJobObjectBasicProcessIdList(job)
            foreach(proc_ids) do id
                try
                    CreateMinidump(id)
                catch e
                    bt = catch_backtrace()
                    println(stderr, "Failed to create minidump for process $id")
                    Base.display_error(stderr, e, bt)
                end
            end
        end
    finally
        TerminateJobObject(job)
        exit(1)
    end
end

wait(proc)
exit(0)
