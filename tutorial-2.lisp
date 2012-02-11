;;; LISP Tutorial 2: Advanced Functional Programming in LISP

;; Copied examples

(defun slow-list-reverse (L)
  "Create a new list containing the elements of L in reversed order."
  (if (null L)
      nil
    (list-append (slow-list-reverse (rest L)) 
                 (list (first L)))))

;; Exercise solutions

;; Tests

(define-test slow-list-reverse
  (assert-equal nil (slow-list-reverse nil))
  (assert-equal '(C B A) (slow-list-reverse '(A B C)))
  )