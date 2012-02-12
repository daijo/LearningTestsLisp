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

(defun fast-factorial-aux (N A)
  "Multiply A by the factorial of N."
  (if (= N 1)
      A
    (fast-factorial-aux (- N 1) (* N A))))

(defun fast-factorial (N)
  "A tail-recursive version of factorial."
  (fast-factorial-aux N 1))

;; Exercise solutions

(defun fast-triangular-aux (N A)
  (if (= N 1)
      A
    (fast-triangular-aux (- N 1) (+ N A))))

(defun fast-triangular (N)
  "A tail-recursive version of triangular."
  (fast-triangular-aux N 1))

;; Tests

(define-test slow-list-reverse
  (assert-equal nil (slow-list-reverse nil))
  (assert-equal '(C B A) (slow-list-reverse '(A B C)))
  )

(define-test list-reverse
  (assert-equal nil (list-reverse nil))
  (assert-equal '(C B A) (list-reverse '(A B C)))
  )

(define-test fast-factorial
  (assert-equal 1 (fast-factorial 1))
  (assert-equal 2 (fast-factorial 2))
  (assert-equal 6 (fast-factorial 3))
  (assert-equal 24 (fast-factorial 4))
  )

(define-test fast-triangular
  (assert-equal 1 (fast-triangular 1))
  (assert-equal 3 (fast-triangular 2))
  (assert-equal 6 (fast-triangular 3))
  (assert-equal 10 (fast-triangular 4))
  )