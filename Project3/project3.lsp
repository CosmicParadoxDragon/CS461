;; Question 1 - Count Atoms in all levels of a list
(defun count-atoms (lsp) 
    (cond ((null lsp) 0) ; Base case
          ((listp (car lsp)) (count-atoms (car lsp))) ; Level Down Case
          (t (+ 1 (count-atoms (cdr lsp)))))) ; Count atom case
; Question 1 - Test Function
(defun test-count ()
    (let ((list1 '(a b c (d (e f g)))))
        (format t "The list: ~a~%" list1)
        (format t "Number of elements: ~A~%" (count-atoms list1)))
)
;; Question 2 - Check if atom is a member
(defun member-any (a lsp)
    (cond ((null lsp) nil)
          ((listp (car lsp)) (member-any a (car lsp)))
          ((equal a (car lsp)) t)
          (t (member-any a (cdr lsp))))
)
; Question 2 - Test Function
(defun test-any ()
    ; ‘c ‘(((b c) ((a)) c)) > t
    (let ((list1 '(((b c) ((a)) c)) )
          (tar `c))
          (member-any tar list1))
)
;; Question 3 - Find Depth
; (find-depth ‘(a b c d e f g h))
; 0
; (find-depth ‘(a (b c (d e) (f g h))))
; 2
(defun find-depth (lsp)
    (if (atom lsp) 0
        (+ 1 (apply #'max (mapcar #'find-depth lsp))))
)
; Question 3 - Test Function
(defun test-depth ()
    (let ((list1 '(a (b c (d e) (f g h)))))
    (find-depth list1))
)
;; Question 4 - Flatten
(defun flatten (lsp)
    (cond ((null lsp) 0)
          ((atom lsp) (list lsp))
          (t (mapcan #'flatten lsp)))
)
; Question 4 - Test Function
(defun test-flatten ()
    (let ((list1 '(a b c (d (e f) g (h i)))))
        (flatten list1))
)
