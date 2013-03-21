global hasRunJulia = 0
global JuliaPID = 0
global NppPID = 0

OpenJulia()
{
  if %hasRunJulia% = 0
  {
    Run julia.bat,,,JuliaPID
    hasRunJulia = 1
    ReturnToNpp()
    return
  }
  Process, Exist, %JuliaPID%
  if %ErrorLevel% = 0
  {
    Run julia.bat,,,JuliaPID
    hasRunJulia = 1
    ReturnToNpp()
    return
  }
}

ReOpenJulia()
{
  WinClose, ahk_pid %JuliaPID%
  OpenJulia()
}

SendToJulia()
{
  OpenJulia()
  WinWait, ahk_pid %JuliaPID%
  WinActivate, ahk_pid %JuliaPID%
  WinWaitActive, ahk_pid %JuliaPID%
  Send {Alt Down}{Space}{Alt up}ep{Enter}
}

ReturnToNpp()
{
  WinActivate, ahk_pid %NppPID%
}
  

PassLine()
{
  WinGetCLASS, currentClass, A
  If currentClass = Notepad++
  { 
    WinGet, NppPID, PID, A
    send {Home}{Shift Down}{End}{Shift Up}{Ctrl Down}c{Ctrl Up}{End}
    SendToJulia()
    ReturnToNpp()
  }
}

PassBlock()
{
  WinGetCLASS, currentClass, A
  If currentClass = Notepad++
  {
    WinGet, NppPID, PID, A
    send {Ctrl Down}c{Ctrl Up}
    SendToJulia()
    ReturnToNpp()
  }
}
  

LShift & Enter:: PassLine()

Ctrl & d:: PassBlock()

$F3:: ReOpenJulia()
