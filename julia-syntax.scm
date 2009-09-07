; first passes:
; - expand lvalues, e.g. (= (call ref A i) x) => (= A (call assign A i x))
; - expand operators like +=
; - identify type name contexts and sort out ranges from declarations
; - expand for into while
; - expand -> and function into lambda/addmethod
; - replace (. a b) with (call get a (quote b))
