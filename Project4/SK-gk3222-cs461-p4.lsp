; Sohrab Kazak
; Mar. 12 2024
; CS461 | Project 4 

; Defining rules for states in Missionaries And Cannibals
(defun is-valid (in-state)
    (let ((l-m (car in-state))
          (l-c (cadr in-state))
          (r-m (- 3 (car in-state)))
          (r-c (- 3 (cadr in-state))))
     (and (or (> l-m l-c) (= l-m l-c) (= l-m 0)) 
          (or (> r-m r-c) (= r-m r-c) (= r-m 0))
          (and (< l-m 4) (< l-c 4) (> l-m -1) (> l-c -1))
    )
))

; Recursive List Trimming Function, Inverts Ordering from mac-next
(defun trim-list (list rule)
  (cond ((null list) nil)
        (t (append 
              (trim-list (cdr list) rule)
              (if (funcall rule (car list))
                (list (car list))
                nil)))))
                
; State Generating Function
(defun mac-next (in-state)
    ; Check if given state is legal
    (if (is-valid in-state)
        ; All possible actions that can happen including states that will be illegal
        (let ((possible-actions (list :one-missionary :two-missionaries :one-cannibal :two-cannibals :both))
          ; Binding for input-state
          (m (car in-state)) ; missionaries
          (c (cadr in-state)) ; cannibals 
          (b (caddr in-state)) ; boat state
          ; Flip the boat state for the new states
          (boat-state (cond ((equal (caddr in-state) 'r) 'l)
                            ((equal (caddr in-state) 'l) 'r)
                            (t "State:Boat:Invalid"))))
        (trim-list (mapcar (lambda (action)
            ; Where is the boat currently
            ; ; Right
            (cond ((eq b 'R)
                    (cond   ((eq action :one-missionary)    (list (+ m 1) c boat-state))
                            ((eq action :two-missionaries)  (list (+ m 2) c boat-state))
                            ((eq action :one-cannibal)      (list m (+ c 1) boat-state))
                            ((eq action :two-cannibals)     (list m (+ c 2) boat-state))
                            ((eq action :both)              (list (+ m 1) (+ c 1) boat-state))
                    ))
            ; ; Left
                ((eq b 'L)
                    (cond   ((eq action :one-missionary)    (list (- m 1) c boat-state))
                            ((eq action :two-missionaries)  (list (- m 2) c boat-state))
                            ((eq action :one-cannibal)      (list m (- c 1) boat-state))
                            ((eq action :two-cannibals)     (list m (- c 2) boat-state))
                            ((eq action :both)              (list (- m 1) (- c 1) boat-state))
                    ))
            ; ; Boat is not R or L
                ((t "State:Boat:Invalid")))) 
            possible-actions ) #'is-valid) ; Trim list against the is-valid function
    )
    ;Else
    "State:Invalid" ))
            
            