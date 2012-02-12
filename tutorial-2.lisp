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

(defun repeat-transformation (F N X)
  "Repeat applying function F on object X for N times."
  (if (zerop N)
      X
    (repeat-transformation F (1- N) (funcall F X))))

;; Exercise solutions

(defun fast-triangular-aux (N A)
  (if (= N 1)
      A
    (fast-triangular-aux (- N 1) (+ N A))))

(defun fast-triangular (N)
  "A tail-recursive version of triangular."
  (fast-triangular-aux N 1))

(defun fast-power-aux (B N A)
  (if (= N 1)
      A
    (fast-power-aux B (- N 1) (* B A))))

(defun fast-power (B E)
  "A tail-recursive version of power."
  (if (= E 0)
      1
    (fast-power-aux B E B)))

(defun fast-list-length-aux (L A)
  (if (null L)
      A
    (fast-list-length-aux (rest L) (1+ A))))

(defun fast-list-length (L)
  "A tail-recursive version of list length."
  (if (null L)
      0
    (fast-list-length-aux (rest L) 1)))

(defun apply-func-list (L X)
  "Applies a list of functions to an object."
  (if (null L)
      X
    (apply-func-list (my-butlast L) (funcall (first (my-last L)) X))))

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

(define-test fast-power
  (assert-equal 0 (fast-power 0 1))
  (assert-equal 1 (fast-power 1 0))
  (assert-equal 0 (fast-power 0 2))
  (assert-equal 1 (fast-power 2 0))
  (assert-equal 2 (fast-power 2 1))
  (assert-equal 4 (fast-power 2 2))
  (assert-equal 8 (fast-power 2 3))
  )

(define-test fast-list-length
  (assert-equal 0 (fast-list-length nil))
  (assert-equal 1 (fast-list-length '(1)))
  (assert-equal 5 (fast-list-length '(1 2 3 4 5)))
  )

(define-test repeat-transformation
  (assert-equal 16 (repeat-transformation (function my-double) 4 1))
  (assert-equal 'g (first (repeat-transformation #'rest 6 '(a b c d e f g h i j))))
  (assert-equal 16 (repeat-transformation #'(lambda (X) (* 2 X)) 4 1))
  )

(define-test apply-func-list
  (assert-equal 4 (apply-func-list (list #'my-double) 2))
  (assert-equal 4 (apply-func-list (list #'my-double #'list-length #'rest) '(1 2 3)))
  (assert-equal 400 (apply-func-list (list #'(lambda (X) (* 10 X)) #'fourth) '(10 20 30 40 50)))
  (assert-equal 5 (apply-func-list (list #'third #'second) '((1 2) (3 4 5) (6))))
  (assert-equal 4 (apply-func-list (list #'(lambda (X) (- 10 X)) #'fast-list-length) '(a b c d e f)))
  (assert-equal '((blah)) (apply-func-list (list #'list #'list) 'blah))
  )