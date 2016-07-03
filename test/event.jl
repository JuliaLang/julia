# This file is a part of Julia. License is MIT: http://julialang.org/license

# Tests for @timeout
event_const_a = 10
event_fun_x(a) = ( sleep(0.1); 100a )
event_fun_xa(a) = 100a
event_fun_y() = ( sleep(0.1); 200 )
event_fun_w(a, b_) = a + b_
event_fun_w2(a, b_) = (sleep(0.1); a + b_)
event_msg = "error_message"

function event_fun_z()
    sleep(0.1)
    throw(ErrorException("error during event_fun_z()"))
end

@test @timeout(1, event_fun_x(5)) == 100*5

event_result_r = @timeout 1 event_fun_x(5)
@test event_result_r == 100*5

@test @timeout(1, event_fun_xa(2)) == 200
@test_throws TimeoutException @timeout 0.01 event_fun_z()

@test_throws TimeoutException @timeout 0.01 event_fun_y()
@test_throws TimeoutException @timeout 0.01 event_fun_x(1)
@test_throws ErrorException @timeout 1 event_fun_z() "error"
@test_throws TimeoutException @timeout 0.01 event_fun_z() "error"

@test @timeout(1, event_fun_w(1, event_const_a)) == 11
@test @timeout(1, event_fun_w(1, event_const_a), "error") == 11
@test @timeout(1, event_fun_w2(1, event_const_a)) == event_fun_w(1, event_const_a)
@test_throws TimeoutException @timeout(0.01, event_fun_w2(1, event_const_a))
@test_throws TimeoutException @timeout(0.01, event_fun_w2(1, event_const_a), "error " * "message")
@test_throws TimeoutException @timeout(0.01, event_fun_w2(1, event_const_a), event_msg)
