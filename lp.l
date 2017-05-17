;;singleton-p - determines whether a list contains only one element
(defun singleton-p (list)
  (cond
   ((null List) NIL)
   ((null (cdr List)) T)
   (T NIL)
   )
  )

;;returns the last element of a list
(defun rac (list)
  (cond
   ((singleton-p list)(car list))
   (T (rac (cdr list)))
   )
  )

;;returns all elements of the list except the last one
(defun rdc(list)
  (cond
   ((singleton-p list)())
   (T (cons (car list)(rdc (cdr list))))
   )
  )

;;adds an element to the end of a list
(defun snoc (value list)
  (cond
   ((null list)(list value))
   (T (cons (car list)(snoc value (cdr list))))
   )
  )

;;determines whether a list is the same forwards as it's backwards
(defun palindrome-p (list)
  (cond
   ((null list) T)
   ((singleton-p list) T)
   ((equal (car list)(rac list))(palindrome-p (cdr (rdc list))))
   (T NIL)
   )
  )

;;takes in an non-integer integer, which returns the element of the list in the given index
(defun select (index list)
  (cond
   ((null list) T)
   ((equal index 0)(car list))
   (T (select (- index 1) (cdr list)))
   )
  )
;;returns a random element from a list
(defun pick (list)
  (select (random (length list)) list )
  )

;;returns the sum of a given list
(defun sum(list)
  (cond
   ((null list) nil)
   ((singleton-p list) (car list))
   (T(+ (car list) (sum (cdr list))))
   )
  )

;;returns the product of a given list
(defun product(list)
  (cond
   ((null list) nil)
   ((singleton-p list) (car list))
   (T (* (car list) (product (cdr list))))
   )
  )

;;generating a list containing number smaller than a given non-negative integer
(defun iota (n)
  (cond
   ((equal n 0) ())
   (T (snoc n (iota (- n 1))))
   )
  )
;;;;generates a list containing n duplicates of a list
(defun duplicate(n list)
  (cond
   ((equal n 0) ())
   (T (cons list (duplicate (- n 1)list)))
   )
  )

;;factorial of a given number
(defun factorial (n)
  (product (iota n))
  )

;;power of a number
(defun power(x n)
  (cond
   ((equal n 0) 1)
   (T (product (duplicate n x)))
   )
  )

;;takes a predicate and a list as parameters, returns the list of elements which are true with respect to the predicate
(defun filter-in (predicate list)
  (cond
   ((null list)())
   ((funcall predicate (car list))(cons (car list)(filter-in predicate (cdr list))))
   (T (filter-in predicate (cdr list)))
   )
  )

;;takes a predicate and a list, returns the lists of elements which are false with respect to the predicate
(defun filter-out (predicate list)
  (cond
   ((null list)())
   ((funcall predicate (car list))(filter-out predicate (cdr list)))
   (T (cons (car list)(filter-out predicate (cdr list))))
   )
  )

;;takes an object and list as parameters, and returns the list with all occurrences of the objecct removed from the list
(defun take-from (object list)
  (cond
   ((null list) ())
   ((equal object (car list)) (take-from object (cdr list)))
   (T (cons (car list)(take-from object (cdr list))))
   )
  )

;;permutation of a list
(defun random-permutation (list &aux element remainder)
  (cond
   ((null list)())
   (T
    (setf element (pick list))
    (setf remainder (remove element list :count 1))
    (cons element (random-permutation remainder))
    )
   )
  )
   
