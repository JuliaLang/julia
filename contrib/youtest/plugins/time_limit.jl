# ToDo

type TookTooLong <: FailedSomehow
    target :: Exp
    actual :: Exp
end

s = sec = seconds = 1
m = min = minutes = 60
h = hours = 60m
ms = milliseconds = s/1000
us = microseconds = s/1000000


@test begin
    time_limit := 4s   # Pass if completed more quickly
    kill_after := 20s  # Abort the test if exceeded
    ...
end
