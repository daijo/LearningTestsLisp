;;; Solutions to LISP Tutorial 1: Basic LISP Programming

;; Solutions

(defun my-double (x) (* x 2))

;; Tests

(define-test my-double
  (assert-equal 0 (my-double 0))
  (assert-equal 2 (my-double 1))
  (assert-equal 4 (my-double 2))
  (assert-equal 6 (my-double 3))
  )