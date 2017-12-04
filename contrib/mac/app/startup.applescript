-- find out preferred terminal
try
	tell application "Finder" to get application file id "com.googlecode.iterm2"
	set termapp to "iTerm2"
on error
	set termapp to "Terminal"
end try

set RootPath to POSIX path of (path to me)
set cmd to "exec '" & RootPath & "Contents/Resources/julia/bin/julia'"

tell application termapp
	activate
	-- create a new window, to avoid typing into existing one 
	tell application "System Events" to tell application termapp to keystroke "n" using command down
	tell application "System Events" to tell application termapp to keystroke cmd & return
end tell
