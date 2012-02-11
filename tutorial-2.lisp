;;; LISP Tutorial 2: Advanced Functional Programming in LISP

;; Copied examples

(defun slow-list-reverse (L)
  "Create a new list containing the elements of L in reversed order."
  (if (null L)
      nil
    (list-append (slow-list-reverse (rest L)) 
                 (list (first L)))))

(defun list-reverse-aux (L A)
  "Append list A to the reversal of list L."
  (if (null L)
      A
    (list-reverse-aux (rest L) (cons (first L) A))))

(defun list-reverse (L)
  "Create a new list containing the elements of L in reversed order."
  (list-reverse-aux L nil))

;; Exercise solutions

;; Tests

(define-test slow-list-reverse
  (assert-equal nil (slow-list-reverse nil))
  (assert-equal '(C B A) (slow-list-reverse '(A B C)))
  )

(define-test list-reverse
  (assert-equal nil (list-reverse nil))
  (assert-equal '(C B A) (list-reverse '(A B C)))
  )