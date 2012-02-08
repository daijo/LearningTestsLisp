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
  "Compute the triangular of N."
  (if (= N 1)
      1
    (+ N (triangular (- N 1)))))

(defun power (B E)
  "Compute B to the power of E"
  (if (= E 0)
      1
    (* B (power B (- E 1)))))

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

(define-test power
  (assert-equal 0 (power 0 1))
  (assert-equal 1 (power 1 0))
  (assert-equal 0 (power 0 2))
  (assert-equal 1 (power 2 0))
  (assert-equal 2 (power 2 1))
  (assert-equal 4 (power 2 2))
  (assert-equal 8 (power 2 3))
  )