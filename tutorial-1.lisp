;;; Solutions to LISP Tutorial 1: Basic LISP Programming

;; Copied examples

(defun my-double (x) (* x 2))

(defun factorial (N)
  "Compute the factorial of N."
  (if (= N 1)
      1
    (* N (factorial (- N 1)))))

(defun fibonacci (N)
  "Compute the N'th Fibonacci number."
  (if (or (zerop N) (= N 1))
      1
    (+ (fibonacci (- N 1)) (fibonacci (- N 2)))))

(defun recursive-list-length (L)
  "A recursive implementation of list-length."
  (if (null L)
      0
    (1+ (recursive-list-length (rest L)))))

(defun list-nth (N L)
  "Return the N'th member of a list L."
  (if (null L)
      nil
    (if (zerop N)
        (first L)
      (list-nth (1- N) (rest L)))))

(defun list-member (E L)
  "Test if E is a member of L."
  (cond
   ((null L)          nil)   
   ((equal E (first L))  t)     
   (t                 (list-member E (rest L))))) 

(defun list-append (L1 L2)
  "Append L1 by L2."
  (if (null L1)
      L2
    (cons (first L1) (list-append (rest L1) L2))))

(defun list-intersection (L1 L2)
  "Return a list containing elements belonging to both L1 and L2."
  (cond
   ((null L1) nil)
   ((member (first L1) L2) 
    (cons (first L1) (list-intersection (rest L1) L2)))
   (t (list-intersection (rest L1) L2))))

;; Exercise solutions

(defun triangular (N)
  "Compute the triangular of N."
  (if (= N 1)
      1
    (+ N (triangular (- N 1)))))

(defun power (B E)
  "Compute B to the power of E."
  (if (= E 0)
      1
    (* B (power B (- E 1)))))

(defun binomial (N R)
  "Compute the binominal."
  (if (or (= R 0) (= R N))
      1
    (+ (binomial (- N 1) (- R 1)) (binomial (- N 1) R))))

(defun sum (L)
  "Compute the sum of all elements of a list."
  (if (null L)
      0
    (+ (first L) (sum (rest L)))))

(defun my-last (L)
  "Get the last cons structure of a list."
  (cond
    ((null L)         nil)
    ((null (rest L))  (cons (first L) nil))
    (t                (my-last (rest L)))))

(defun my-butlast (L)
  "Return the list L without its last element."
  (cond
    ((null L)  nil)
    ((consp (rest L)) (cons (first L) (my-butlast (rest L))))))

(defun list-union (L1 L2)
  "Return the union of L1 and L2."
  (cond
    ((null L1) L2)
    ((member (first L1) L2) (list-union (rest L1) L2))
    (t (cons (first L1) (list-union (rest L1) L2)))))

(defun list-difference (L1 L2)
  "Return the (non-symmetric) difference of L1 and L2."
  (cond
    ((null L1) L1)
    ((member (first L1) L2) (list-difference (rest L1) L2))
    (t (cons (first L1) (list-difference (rest L1) L2)))))

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

(define-test fibonacci
  (assert-equal 1 (fibonacci 0))
  (assert-equal 1 (fibonacci 1))
  (assert-equal 2 (fibonacci 2))
  (assert-equal 3 (fibonacci 3))
  (assert-equal 5 (fibonacci 4))
  )

(define-test binomial
  (assert-equal 1 (binomial 2 0))
  (assert-equal 1 (binomial 2 2))
  (assert-equal 6 (binomial 4 2))
  )

(define-test recursive-list-length
  (assert-equal 0 (recursive-list-length nil))
  (assert-equal 1 (recursive-list-length '(1)))
  (assert-equal 5 (recursive-list-length '(1 2 3 4 5)))
  )

(define-test sum
  (assert-equal 0 (sum nil))
  (assert-equal 1 (sum '(1)))
  (assert-equal 15 (sum '(1 2 3 4 5)))
  )

(define-test list-nth
  (assert-equal nil (list-nth 2 nil))
  (assert-equal 'a (list-nth 0 '(a b c)))
  (assert-equal 'c (list-nth 2 '(a b c)))
  )

(define-test my-last
  (assert-equal nil (my-last nil))
  (assert-equal '(c) (my-last '(a b c)))
  (assert-equal '(3) (my-last '(1 2 3)))
  )

(define-test list-member
  (assert-equal nil (list-member 'a nil))
  (assert-equal nil (list-member 'i '(APRIL is the cruellest month breeding)))
  (assert-equal t (list-member 'is '(APRIL is the cruellest month breeding)))
  )

(define-test list-append
  (assert-equal '(a b c)   (list-append nil '(a b c)))
  (assert-equal '(a b c d) (list-append '(a b) '(c d)))
  )

(define-test my-butlast
  (assert-equal nil (my-butlast nil))
  (assert-equal '(a b c) (my-butlast '(a b c d)))
  )

(define-test list-intersection
  (assert-equal nil (list-intersection nil '(a b c)))
  (assert-equal '(c) (list-intersection '(a b c) '(c d e)))
  (assert-equal '(b c) (list-intersection '(a b c) '(b c d)))
  )

(define-test list-union
  (assert-equal '(a b c) (list-union nil '(a b c)))
  (assert-equal '(a b c) (list-union '(a b c) '(a b c)))
  (assert-equal '(a b c d) (list-union '(a b c) '(b c d)))
  )

;; set-difference list1 list2 &key (test #'eql) test-not (key #'identity)
;;   Function
;;   Returns a list of elements of list1 that do not appear in list2.
(define-test list-difference
  (assert-equal nil (list-difference nil '(a b c)))
  (assert-equal nil (list-difference '(a b c) '(a b c)))
  (assert-equal '(a) (list-difference '(a b c) '(b c d)))
  )