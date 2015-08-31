;=====================================
; npp_julia.ahk
; Connect Julia and notepad++
; AutoHotkey:     1.x
; Language:       English
; Platform:       Windows7
; Author:         ViralBShah - 2013
; Modified by:    cameyo - 2015
; License:        MIT license
;=====================================
; Hotkeys:
; Win-F12 -> Start Julia
; Left_Shift-Enter -> Evaluate current line
; Right_Shift-Enter -> Evaluate selected block

;=====================================
; Basic hotkeys symbol
;=====================================
; # Win key
; ! Alt key
; ^ Control key
; + Shift key

;=====================================
; General statements
;=====================================
#NoEnv  ; Recommended for performance and compatibility with future AutoHotkey releases.
SendMode Input  ; Recommended for new scripts due to its superior speed and reliability.
SetWorkingDir %A_ScriptDir%  ; Ensures a consistent starting directory.
#SingleInstance force ; Allow reload script
#WinActivateForce ; activate window with forceful method

;=====================================
; Global variables
;=====================================
#EscapeChar \
global hasRunJulia = 0
global JuliaPID = 0
global NppPID = 0

;---------------------------
OpenJulia()
{
  if %hasRunJulia% = 0
  {
    Run julia.exe,,,JuliaPID
    hasRunJulia = 1
    ReturnToNpp()
    return
  }
  Process, Exist, %JuliaPID%
  if %ErrorLevel% = 0
  {
    Run julia.exe,,,JuliaPID
    hasRunJulia = 1
    ReturnToNpp()
    return
  }
}
;---------------------------
ReOpenJulia()
{
  WinClose, ahk_pid %JuliaPID%
  OpenJulia()
}
;---------------------------
SendToJulia()
{
  OpenJulia()
  WinWait, ahk_pid %JuliaPID%
  WinActivate, ahk_pid %JuliaPID%
  WinWaitActive, ahk_pid %JuliaPID%

  ; Past via menu (must disable SendMode Input)
  ;Send {Alt Down}{Space}{Alt up}ep{Enter}

  ; Past with send clipboard 
  StringReplace clipboard2, clipboard, \r\n, \n, All
  SendInput {Raw}%clipboard2%\n
}
;---------------------------
ReturnToNpp()
{
  WinActivate, ahk_pid %NppPID%
}
;---------------------------
PassLine()
{
  WinGetCLASS, currentClass, A
  If currentClass = Notepad++
  { 
    WinGet, NppPID, PID, A
    SendInput {Home}{Shift Down}{End}{Shift Up}{Ctrl Down}c{Ctrl Up}{End}
    SendToJulia()
    ReturnToNpp()
  }
}
;---------------------------
PassBlock()
{
  WinGetCLASS, currentClass, A
  If currentClass = Notepad++
  {
    WinGet, NppPID, PID, A
    SendInput {Ctrl Down}c{Ctrl Up}
    SendToJulia()
    ReturnToNpp()
  }
}
;---------------------------
; Eval current line (Left Shift - Enter)
LShift & Enter:: PassLine()
;---------------------------
; Eval selected block (Right Shift - Enter)
RShift & Enter:: PassBlock()
;---------------------------
; Run Julia (Win-F12)
#$F12:: ReOpenJulia()

;================================================
; REPL enhancements
; Work only on console window
#IfWinActive ahk_class ConsoleWindowClass

;=============================
; Scroll command window back and forward
; Ctrl+PageUp / PageDown
;=============================
^PgUp:: SendInput {WheelUp}

^PgDn:: SendInput {WheelDown}

;=============================
; Paste in REPL
; Ctrl-V
; Better version
;=============================
^V::
StringReplace clipboard2, clipboard, \r\n, \n, All
SendInput {Raw}%clipboard2%
return

;=============================
; Paste in REPL
; Ctrl-V
; Old version (must disable SendMode Input)
;=============================
;^V::
; English menu (Edit->Paste)
;Send !{Space}ep
;return

#IfWinActive
;================================================

;=============================
; Greek Letters (lowercase)
; Control-Win-<char>
; char 'j' is free...
;=============================
^#a:: SendInput {U+03B1} ; alpha
^#b:: SendInput {U+03B2} ; beta
^#g:: SendInput {U+03B3} ; gamma
^#d:: SendInput {U+03B4} ; delta
^#y:: SendInput {U+03B5} ; epsilon (y)
^#z:: SendInput {U+03B6} ; zeta
^#e:: SendInput {U+03B7} ; eta
^#h:: SendInput {U+03B8} ; theta (h)
^#i:: SendInput {U+03B9} ; iota
^#k:: SendInput {U+03BA} ; kappa
^#l:: SendInput {U+03BB} ; lambda
^#m:: SendInput {U+03BC} ; mu
^#n:: SendInput {U+03BD} ; nu
^#x:: SendInput {U+03BE} ; xi
^#o:: SendInput {U+03BF} ; omicron
^#p:: SendInput {U+03C0} ; pi
^#r:: SendInput {U+03C1} ; rho
^#s:: SendInput {U+03C3} ; sigma
^#t:: SendInput {U+03C4} ; tau
^#u:: SendInput {U+03C5} ; upsilon
^#f:: SendInput {U+03C6} ; phi (f)
^#c:: SendInput {U+03C7} ; chi
^#v:: SendInput {U+03C8} ; psi (v)
^#w:: SendInput {U+03C9} ; omega (w)
