(in-package #:match)

;;
;;  append
;;
(defmacro defrule-append ()
  `(progn
     (defrule (append () ?x ?x))
     (defrule (append (?u . ?x) ?y (?u . ?z))
       (append ?x ?y ?z))))

(defmacro with-match-append (&body body)
  `(with-match
     (defrule-append)
     ,@body))

(deftest append.1
  (with-match-append
    (match (append (a b c) (d e f) (a b c d e f))))
  nil t)

(deftest append.2
  (with-match-append
    (match (append ?x (d e f) (a b c d e f))))
  ((?x a b c)) t)

(deftest append.3
  (with-match-append
    (match (append (a b c) ?x (a b c d e f))))
  ((?x d e f)) t)

(deftest append.4
  (with-match-append
    (match (append (a b c) (d e f) ?x)))
  ((?x a b c d e f)) t)

(deftest append.5
  (with-match-append
    (let (list)
      (match-lisp
        '(append ?x ?y (a b c d))
        (lambda (a)
          (push (list (cdr (assoc '?x a))
                      (cdr (assoc '?y a))) list)
          nil))
      (nreverse list)))
  ((() (a b c d))
   ((a) (b c d))
   ((a b) (c d))
   ((a b c) (d))
   ((a b c d) ())))


;;
;;  member
;;
(defmacro defrule-member ()
  `(progn
     (defrule (member ?x (?x . _)))
     (defrule (member ?x (_ . ?ys)) (member ?x ?ys))))

(defmacro with-match-member (&body body)
  `(with-match
     (defrule-member)
     ,@body))

(deftest member.1
  (with-match-member
    (match (member a (a))))
  nil t)

(deftest member.2
  (with-match-member
    (match (member d (a b c d e))))
  nil t)

(deftest member.3
  (with-match-member
    (match (member z (a b c d e))))
  nil nil)

(deftest member.4
  (with-match-member
    (match (member ?x (a b c d e))))
  ((?x . a)) t)

(deftest member.5
  (with-match-member
    (match (member z (a b ?x d e))))
  ((?x . z)) t)


;;
;;  prefix / suffix
;;
(defmacro defrule-prefix ()
  `(progn
     (defrule (prefix () _))
     (defrule (prefix (?x . ?xs) (?x . ?ys)) (prefix ?xs ?ys))))

(defmacro defrule-suffix ()
  `(progn
     (defrule (suffix ?xs ?xs))
     (defrule (suffix ?xs (_ . ?ys)) (suffix ?xs ?ys))))

(defmacro with-match-prefix (&body body)
  `(with-match
     (defrule-prefix)
     (defrule-suffix)
     ,@body))

(deftest prefix.1
  (with-match-prefix
    (match (prefix () ())))
  nil t)

(deftest prefix.2
  (with-match-prefix
    (match (prefix (a b c) (a b c d e f))))
  nil t)

(deftest prefix.3
  (with-match-prefix
    (match (prefix (a b e) (a b c d e f))))
  nil nil)

(deftest prefix.4
  (with-match-prefix
    (match (prefix (a b ?x) (a b c d e f))))
  ((?x . c)) t)

(deftest prefix.5
  (with-match-prefix
    (match (prefix (a b c) (a b ?x d e f))))
  ((?x . c)) t)

(deftest prefix.6
  (with-match-prefix
    (match-lisp
      '(prefix ?x (a b c d e f))
      (lambda (x)
        (eql 4 (length (cdr (assoc '?x x)))))))
  ((?x a b c d)) t)

(deftest suffix.1
  (with-match-prefix
    (match (suffix () (a b c d))))
  nil t)

(deftest suffix.2
  (with-match-prefix
    (match (suffix (c d) (a b c d))))
  nil t)

(deftest suffix.3
  (with-match-prefix
    (match (suffix (a c d) (a b c d))))
  nil nil)

(deftest suffix.4
  (with-match-prefix
    (match (suffix (b ?x d) (a b c ?y))))
  ((?x . c) (?y . d)) t)


;;
;;  sublist
;;
(defmacro with-match-sublist (&body body)
  `(with-match
     (defrule-prefix)
     (defrule-suffix)
     (defrule-append)
     (defrule (sublist1 ?xs ?ys) (prefix ?ps ?ys) (suffix ?xs ?ps))
     (defrule (sublist2 ?xs ?ys) (prefix ?xs ?ss) (suffix ?ss ?ys))
     (defrule (sublist3 ?xs ?ys) (prefix ?xs ?ys))
     (defrule (sublist3 ?xs (_ . ?ys)) (sublist3 ?xs ?ys))
     (defrule (sublist4 ?x ?x3) (append _ ?x2 ?x3) (append ?x _ ?x2))
     (defrule (sublist5 ?x ?x3) (append ?x2 _ ?x3) (append _ ?x ?x2))
     ,@body))

(deftest sublist.1
  (with-match-sublist
    (match (sublist1 (d e) (a b c d e f))))
  nil t)

(deftest sublist.2
  (with-match-sublist
    (match (sublist2 (d e) (a b c d e f))))
  nil t)

(deftest sublist.3
  (with-match-sublist
    (match (sublist3 (d e) (a b c d e f))))
  nil t)

(deftest sublist.4
  (with-match-sublist
    (match (sublist4 (d e) (a b c d e f))))
  nil t)

(deftest sublist.5
  (with-match-sublist
    (match (sublist5 (d e) (a b c d e f))))
  nil t)

(deftest sublist.6
  (with-match-sublist
    (match (sublist1 (d e) (a b c ?x e f))))
  ((?x . d)) t)

(deftest sublist.7
  (with-match-sublist
    (match (sublist2 (d e) (a b c ?x e f))))
  ((?x . d)) t)

(deftest sublist.8
  (with-match-sublist
    (match (sublist3 (d e) (a b c ?x e f))))
  ((?x . d)) t)

(deftest sublist.9
  (with-match-sublist
    (match (sublist4 (d e) (a b c ?x e f))))
  ((?x . d)) t)

(deftest sublist.10
  (with-match-sublist
    (match (sublist5 (d e) (a b c ?x e f))))
  ((?x . d)) t)


;;
;;  length
;;
(defmacro defrule-length ()
  `(progn
     (defrule (length () 0))
     (defrule (length (_ . ?tail) ?n) (length ?tail ?n1) (is ?n (1+ ?n1)))))

(defmacro with-match-length (&body body)
  `(with-match
     (defrule-length)
     ,@body))

(deftest length.1
  (with-match-length
    (match (length () ?x)))
  ((?x . 0)) t)

(deftest length.2
  (with-match-length
    (match (length (a b c) ?x)))
  ((?x . 3)) t)


;;
;;  quicksort
;;
(defmacro with-match-quicksort (&body body)
  `(with-match
     (defrule (<= ?x ?y) (progn (<= ?x ?y)))
     (defrule (> ?x ?y) (progn (> ?x ?y)))
     (defrule-append)
     ;;  partition
     (defrule (partition (?x . ?xs) ?y (?x . ?ls) ?bs)
       (<= ?x ?y) (partition ?xs ?y ?ls ?bs))
     (defrule (partition (?x . ?xs) ?y ?ls (?x . ?bs))
       (> ?x ?y) (partition ?xs ?y ?ls ?bs))
     (defrule (partition () ?y () ()))
     ;;  quicksort
     (defrule (quicksort (?x . ?xs) ?ys)
       (partition ?xs ?x ?littles ?bigs)
       (quicksort ?littles ?ls)
       (quicksort ?bigs ?bs)
       (append ?ls (?x . ?bs) ?ys))
     (defrule (quicksort () ()))
     ,@body))

(deftest quicksort.1
  (with-match-quicksort
    (match (quicksort (5 4) ?x)))
  ((?x . (4 5))) t)

(deftest quicksort.2
  (with-match-quicksort
    (match (quicksort (4 5 1 2 9 10 6 7 8 3) ?x)))
  ((?x . (1 2 3 4 5 6 7 8 9 10))) t)


;;
;;  test
;;
(do-tests :delete t)

