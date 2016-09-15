set RootPath to POSIX path of (path to me)
tell application "Terminal"
  do script ("exec '" & RootPath & "Contents/Resources/julia/bin/julia'")
  activate
end tell
