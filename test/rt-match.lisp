(in-package #:match-nptcl)

(deftest match-true.1
  (with-match
    (match true))
  nil t)

(deftest-error match-true.2
  (with-match
    (match 'true)))

(deftest match-fail.1
  (with-match
    (match fail))
  nil nil)

(deftest-error match-fail.2
  (with-match
    (match 'fail)))

(deftest match-symbol.1
  (with-match
    (defrule hello)
    (match hello))
  nil t)

(deftest-error match-symbol.2
  (with-match
    (defrule hello)
    (match abc)))


;;
;;  rule
;;
(deftest match-rule.1
  (with-match
    (defrule (aaa bbb ccc))
    (match (aaa bbb ccc)))
  nil t)

(deftest match-rule.2
  (with-match
    (defrule (aaa bbb ccc))
    (match (aaa bbb ddd)))
  nil nil)

(deftest match-rule.3
  (with-match
    (defrule (aaa bbb ccc))
    (match (aaa bbb ?x)))
  ((?x . ccc)) t)


;;
;;  and
;;
(deftest match-and.1
  (with-match
    (match (and)))
  nil t)

(deftest match-and.2
  (with-match
    (match (and true)))
  nil t)

(deftest match-and.3
  (with-match
    (match (and true true true)))
  nil t)

(deftest match-and.4
  (with-match
    (match (and true true fail true)))
  nil nil)


;;
;;  or
;;
(deftest match-or.1
  (with-match
    (match (or)))
  nil nil)

(deftest match-or.2
  (with-match
    (match (or true)))
  nil t)

(deftest match-or.3
  (with-match
    (match (or fail fail fail)))
  nil nil)

(deftest match-or.4
  (with-match
    (match (or fail fail true fail)))
  nil t)


;;
;;  not
;;
(defmacro with-match-not (&body body)
  `(with-match
     (defrule (not ?p)
       (or (and ?p ! fail) true))
     ,@body))

(deftest match-not.1
  (with-match-not
    (match (not true)))
  nil nil)

(deftest match-not.2
  (with-match-not
    (match (not fail)))
  nil t)


;;
;;  is
;;
(deftest match-is.1
  (with-match
    (match (is ?x 100)))
  ((?x . 100)) t)

(defun match-value (expr &aux value)
  (match-lisp
    expr
    (lambda (list)
      (setq value (cdr (assoc '?value list)))
      t))
  value)

(deftest match-is.2
  (with-match
    (defrule (aaa 100))
    (match-value '(and (aaa ?x) (is ?value (+ 200 ?x)))))
  300)

(deftest match-is.3
  (with-match
    (defrule (aaa 100))
    (let (*value*)
      (declare (special *value*))
      (match-lisp
        '(and (aaa ?x) (is _ (setf (symbol-value '*value*) (* ?x 999))))
        (constantly t))
      *value*))
  99900)


;;
;;  progn
;;
(deftest match-progn.1
  (with-match
    (defrule (< ?x ?y) (progn (< ?x ?y)))
    (match (< 10 20)))
  nil t)

(deftest match-progn.2
  (with-match
    (defrule (< ?x ?y) (progn (< ?x ?y)))
    (match (< 30 20)))
  nil nil)


;;
;;  cut
;;
(deftest match-cut.1
  (with-match
    (defrule (< ?x ?y) (progn (< ?x ?y)))
    (defrule (<= ?x ?y) (progn (<= ?x ?y)))
    (defrule (zzz ?x 0) (< ?x 3))
    (defrule (zzz ?x 2) (<= 3 ?x) (< ?x 6))
    (defrule (zzz ?x 4) (<= 6 ?x))
    (values
      (match (and (zzz 1 ?y)))
      (match (and (zzz 5 ?y)))
      (match (and (zzz 6 ?y)))))
  ((?y . 0))
  ((?y . 2))
  ((?y . 4)))

(deftest match-cut.2
  (with-match
    (defrule (< ?x ?y) (progn (< ?x ?y)))
    (defrule (<= ?x ?y) (progn (<= ?x ?y)))
    (defrule (zzz ?x 0) (< ?x 3))
    (defrule (zzz ?x 2) (<= 3 ?x) (< ?x 6))
    (defrule (zzz ?x 4) (<= 6 ?x))
    (match (and (zzz 1 ?y) (< 2 ?y))))
  nil nil)

(defvar *test-match-cut*)
(defun test-match-cut ()
  (incf *test-match-cut* 1)
  t)

(deftest match-cut.3
  (with-match
    (defrule (< ?x ?y) (progn (< ?x ?y)))
    (defrule (<= ?x ?y) (progn (<= ?x ?y)))
    (defrule (zzz ?x 0) (progn (test-match-cut)) (< ?x 3))
    (defrule (zzz ?x 2) (progn (test-match-cut)) (<= 3 ?x) (< ?x 6))
    (defrule (zzz ?x 4) (progn (test-match-cut)) (<= 6 ?x))
    (let ((*test-match-cut* 0))
      (match (and (zzz 1 ?y) (< 2 ?y)))
      *test-match-cut*))
  3)

(deftest match-cut.4
  (with-match
    (defrule (< ?x ?y) (progn (< ?x ?y)))
    (defrule (<= ?x ?y) (progn (<= ?x ?y)))
    (defrule (zzz ?x 0) (progn (test-match-cut)) (< ?x 3) !)
    (defrule (zzz ?x 2) (progn (test-match-cut)) (<= 3 ?x) (< ?x 6) !)
    (defrule (zzz ?x 4) (progn (test-match-cut)) (<= 6 ?x))
    (let ((*test-match-cut* 0))
      (match (and (zzz 1 ?y) (< 2 ?y)))
      *test-match-cut*))
  1)


;;
;;  variable
;;
(deftest match-var.1
  (with-match
    (defrule (aaa ?a ?b ?c) (is ?c (+ ?a ?b)))
    (defrule (bbb 10 20))
    (let (a b c d)
      (match-lisp
        '(and (bbb ?b ?a) (aaa ?a ?b ?d))
        (lambda (list)
          (setq a (cdr (assoc '?a list)))
          (setq b (cdr (assoc '?b list)))
          (setq c (cdr (assoc '?c list)))
          (setq d (cdr (assoc '?d list)))
          t))
      (values a b c d)))
  20 10 nil 30)

(deftest match-var.2
  (with-match
    (defrule (aaa ?aaa ?bbb ?ccc) (is ?ccc (+ ?aaa ?bbb)))
    (defrule (bbb 10 20))
    (let (aaa bbb ccc ddd)
      (match-lisp
        '(and (bbb ?bbb ?aaa) (aaa ?aaa ?bbb ?ddd))
        (lambda (list)
          (setq aaa (cdr (assoc '?aaa list)))
          (setq bbb (cdr (assoc '?bbb list)))
          (setq ccc (cdr (assoc '?ccc list)))
          (setq ddd (cdr (assoc '?ddd list)))
          t))
      (values aaa bbb ccc ddd)))
  20 10 nil 30)

(deftest match-var.3
  (with-match
    (defrule (aaa 10 20 30))
    (match (aaa _ _ ?x)))
  ((?x . 30)) t)

(deftest match-var.4
  (with-match
    (defrule (aaa 10 (20 30 40 50) 30))
    (match (aaa _aaa _bbb ?x)))
  ((?x . 30)) t)


;;
;;  cons
;;
(deftest match-cons.1
  (with-match
    (defrule (aaa (10 20)))
    (match (aaa ?x)))
  ((?x 10 20)) t)

(deftest match-cons.2
  (with-match
    (defrule (aaa (10 20)))
    (match (aaa (_ ?x))))
  ((?x . 20)) t)

(deftest match-cons.3
  (with-match
    (defrule (aaa 10 20 (30 40 (50 . 60)) 70))
    (match (aaa _ _ (?x _ ?y) . _)))
  ((?x . 30) (?y 50 . 60)) t)


;;
;;  vector
;;
(deftest match-vector.1
  (with-match
    (defrule (aaa #(10 20 30)))
    (match (aaa ?x)))
  ((?x . #(10 20 30))) t)

(deftest match-vector.2
  (with-match
    (defrule (aaa #(10 20 30)))
    (match (aaa #(_ ?x 30))))
  ((?x . 20)) t)

(deftest match-vector.3
  (with-match
    (defrule (aaa #(10 (20 #(40)) 30)))
    (match (aaa #(_ (20 #(?x)) 30))))
  ((?x . 40)) t)


;;
;;  value
;;
(deftest match-value.1
  (with-match
    (defrule (aaa 10 symbol "Hello"))
    (match (aaa 10 _ _)))
  nil t)

(deftest match-value.2
  (with-match
    (defrule (aaa 10 symbol "Hello"))
    (match (aaa _ symbol _)))
  nil t)

(deftest match-value.3
  (with-match
    (defrule (aaa 10 symbol "Hello"))
    (match (aaa _ _ "Hello")))
  nil t)

(deftest match-value.4
  (with-match
    (defrule (aaa 10 symbol "Hello"))
    (match (aaa ?x ?y ?z)))
  ((?x . 10) (?y . symbol) (?z . "Hello")) t)


;;
;;  test
;;
(do-tests :delete t)

