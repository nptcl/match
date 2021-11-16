(in-package #:match)

;;
;;  Rule
;;
(deftest readme-init-match.1
  (init-match))

(deftest readme-free-match.1
  (free-match))

(deftest readme-free-match.2
  (progn
    (free-match)
    (free-match)
    (free-match)))

(deftest readme-clear-match.1
  (progn
    (init-match)
    (clear-match)))

(deftest readme-clear-match.2
  (progn
    (init-match)
    (defrule aaa)
    (clear-match)
    (prog1 (hash-table-count *rule*)
      (free-match)))
  0)

(deftest readme-with-match.1
  (progn
    (free-match)
    (init-match)
    (defrule aaa)
    (defrule bbb)
    (defrule ccc)
    (prog1
      (with-match
        (defrule ddd)
        (defrule eee)
        (hash-table-count *rule*))
      (free-match)))
  2)

(deftest readme-delete-rule.1
  (with-match
    (defrule aaa)
    (defrule bbb)
    (delete-rule 'bbb)
    (values
      (nth-value 1 (gethash 'aaa *rule*))
      (nth-value 1 (gethash 'bbb *rule*))))
  t nil)

(deftest readme-define-rule.1
  (with-match
    (define-rule 'aaa 'true)
    (match aaa))
  nil t)

(deftest readme-define-rule.2
  (with-match
    (define-rule '(aaa 10) '(and))
    (match (aaa ?x)))
  ((?x . 10)) t)

(deftest readme-defrule.1
  (with-match
    (defrule aaa)
    (match aaa))
  nil t)

(deftest readme-defrule.2
  (with-match
    (defrule (aaa 10) (and true))
    (match (aaa ?x)))
  ((?x . 10)) t)


;;
;;  Match
;;
(deftest readme-match-lisp.1
  (with-match
    (defrule (aaa 10))
    (match-lisp
      '(aaa ?x)
      (lambda (list)
        (unless (equal list '((?x . 10)))
          (error "error"))
        t)))
  ((?x . 10)) t)

(deftest readme-match-lisp.2
  (with-match
    (defrule (aaa 10))
    (match-lisp
      '(aaa ?x)
      (lambda (list)
        (unless (equal list '((?x . 10)))
          (error "error"))
        nil)))
  nil nil)

(deftest readme-match-true.1
  (with-match
    (defrule aaa)
    (match-true 'aaa))
  nil t)

(deftest readme-match-fail.2
  (with-match
    (defrule aaa)
    (match-fail 'aaa))
  nil nil)

(deftest readme-match.1
  (with-match
    (defrule aaa)
    (match aaa))
  nil t)

(deftest readme-match.2
  (with-match
    (defrule aaa)
    (defrule bbb)
    (match aaa bbb))
  nil t)

(deftest readme-query-lisp.1
  (with-match
    (query-lisp 'fail))
  nil nil)

(deftest readme-query.1
  (with-match
    (query fail fail fail))
  nil nil)


;;
;;  Readme
;;
(defmacro define-readme-example ()
  `(progn
     (defrule aaa)
     (defrule (bbb ccc ddd))
     (defrule (eee ?x) (bbb ?x ddd) aaa)))

(deftest readme.1
  (with-match
    (defrule aaa)
    (defrule (bbb ccc ddd))
    (defrule (eee ?x) (bbb ?x ddd) aaa)
    (match (eee ccc)))
  nil t)

(deftest readme.2
  (with-match
    (define-rule 'aaa)
    (define-rule '(bbb ccc ddd))
    (define-rule '(eee ?x) '(and (bbb ?x ddd) aaa))
    (match (bbb ccc ddd)))
  nil t)

(deftest readme.3
  (with-match
    (define-readme-example)
    (match (bbb ccc eee)))
  nil nil)

(deftest readme.4
  (with-match
    (define-readme-example)
    (match (bbb ?x ?y)))
  ((?x . ccc) (?y . ddd)) t)

(deftest readme.5
  (with-match
    (define-readme-example)
    (match (bbb _ ?y)))
  ((?y . ddd)) t)

(deftest readme.6
  (with-match
    (define-readme-example)
    (match (eee ccc)))
  nil t)

(deftest readme.7
  (with-match
    (define-readme-example)
    (match (eee zzz)))
  nil nil)

(defmacro define-readme-append ()
  `(progn
     (defrule (append () ?x ?x))
     (defrule (append (?u . ?x) ?y (?u . ?z))
       (append ?x ?y ?z))
     (match (append ?x ?y (a b c d e f)))))

(deftest readme-append.1
  (with-match
    (define-readme-append)
    (match (append ?x ?y (a b c d e f))))
  ((?x) (?y a b c d e f)) t)

(deftest readme-append.2
  (with-match
    (define-readme-append)
    (match-lisp
      '(append ?x ?y (a b c d e f))
      (lambda (list)
        (= 3 (length (cdr (assoc '?x list)))))))
  ((?x a b c) (?y d e f)) t)

(defun match-collect (expr)
  (let (list)
    (match-lisp
      expr
      (lambda (alist)
        (push alist list)
        nil))
    (nreverse list)))

(deftest readme-append.3
  (with-match
    (define-readme-append)
    (match-collect
      '(append ?x ?y (a b c d e f))))
  (((?x) (?y a b c d e f))
   ((?x a) (?y b c d e f))
   ((?x a b) (?y c d e f))
   ((?x a b c) (?y d e f))
   ((?x a b c d) (?y e f))
   ((?x a b c d e) (?y f))
   ((?x a b c d e f) (?y))))

(deftest readme-is.1
  (with-match
    (defrule (aaa 100))
    (match (aaa ?x) (is ?y (+ 200 ?x))))
  ((?x . 100) (?y . 300)) t)

(deftest readme-is.2
  (with-match
    (defrule (aaa 100))
    (let (value)
      (declare (special value))
      (match (and (aaa ?x) (is _ (setf (symbol-value 'value)
                                       (format nil "<<<~S>>>" '?x)))))
      value))
  "<<<100>>>")

(deftest readme-less.1
  (with-match
    (defrule (< ?x ?y) (progn (< ?x ?y)))
    (match (< 10 20)))
  nil t)

(deftest readme-less.2
  (with-match
    (defrule (< ?x ?y) (progn (< ?x ?y)))
    (match (< 30 20)))
  nil nil)

(deftest readme-fail.1
  (with-match
    (defrule (prefix () _))
    (defrule (prefix (?x . ?xs) (?x . ?ys)) (prefix ?xs ?ys))
    (let (value)
      (declare (special value))
      (match-fail
        '(and (prefix ?x (a b c d))
              (is _ (push '?x (symbol-value 'value)))))
      (nreverse value)))
  (()
   (a)
   (a b)
   (a b c)
   (a b c d)))


;;
;;  test
;;
(do-tests :delete t)

