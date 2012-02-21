;;; LISP Tutorial Lecture 3: Data Abstraction

;;
;; Binary Trees
;;

;;
;; Constructors for binary trees
;;

(defun make-bin-tree-leaf (E)
  "Create a leaf."
  (list E))

(defun make-bin-tree-node (E B1 B2)
  "Create a node with element K, left subtree B1 and right subtree B2."
  (list E B1 B2))

;;
;; Selectors for binary trees
;;

(defun bin-tree-leaf-element (L)
  "Retrieve the element of a leaf L."
  (first L))

(defun bin-tree-node-element (N)
  "Retrieve the element of a node N."
  (first N))

(defun bin-tree-node-left (N)
  "Retrieve the left subtree of a node N."
  (second N))

(defun bin-tree-node-right (N)
  "Retrieve the right subtree of a node N."
  (third N))

;;
;; Recognizers for binary trees
;;

(defun bin-tree-leaf-p (B)
  "Test if binary tree B is a leaf."
  (and (listp B) (= (list-length B) 1)))

(defun bin-tree-node-p (B)
  "Test if binary tree B is a node."
  (and (listp B) (= (list-length B) 3)))

;; Copied examples

(defun bin-tree-member-p (B E)
  "Test if E is an element in binary tree B."
  (if (bin-tree-leaf-p B)
      (equal E (bin-tree-leaf-element B))
    (let
	((elmt  (bin-tree-node-element B))
	 (left  (bin-tree-node-left    B))
	 (right (bin-tree-node-right   B)))
      (or (equal E elmt)
	  (bin-tree-member-p left E)
	  (bin-tree-member-p right E)))))

;; Exercise solutions

;; Tests

(define-test make-bin-tree
  (assert-equal '(* (+ (2) (3)) (- (7) (8))) (make-bin-tree-node '*
                             (make-bin-tree-node '+
                                                 (make-bin-tree-leaf 2)
                                                 (make-bin-tree-leaf 3))
                             (make-bin-tree-node '-
                                                 (make-bin-tree-leaf 7)
                                                 (make-bin-tree-leaf 8))))
  )

(define-test bin-tree-member-p
  (assert-false (bin-tree-member-p '(+ (* (2) (3)) (- (7) (8))) 1))
  (assert-true (bin-tree-member-p '(+ (* (2) (3)) (- (7) (8))) 7))
)