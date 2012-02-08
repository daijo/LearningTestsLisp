;;; Solutions to LISP Tutorial 1: Basic LISP Programming

;; Examples

(defun my-double (x) (* x 2))

(defun factorial (N)
  "Compute the factorial of N."
  (if (= N 1)
      1
    (* N (factorial (- N 1)))))

;; Exercise solutions

(defun triangular (N)
  "Compute the factorial of N."
  (if (= N 1)
      1
    (+ N (triangular (- N 1)))))

;; Tests

(define-test my-double
  (assert-equal 2 (my-double 1))
  (assert-equal 4 (my-double 2))
  (assert-equal 6 (my-double 3))
  )

(define-test factorial
  (assert-equal 1 (factorial 1))
  (assert-equal 2 (factorial 2))
  (assert-equal 6 (factorial 3))
  (assert-equal 24 (factorial 4))
  )

(define-test triangular
  (assert-equal 1 (triangular 1))
  (assert-equal 3 (triangular 2))
  (assert-equal 6 (triangular 3))
  (assert-equal 10 (triangular 4))
  )