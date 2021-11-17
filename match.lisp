;;
;;  match for Common Lisp
;;    https://github.com/nptcl/match
;;
(defpackage #:match-nptcl
  (:use #:cl)
  (:nicknames #:match)
  (:export
    ;;  rule
    #:init-match
    #:free-match
    #:clear-match
    #:with-match
    #:delete-rule
    #:define-rule
    #:defrule

    ;;  match
    #:match-lisp
    #:match-true
    #:match-fail
    #:match
    #:query-lisp
    #:query
    ))

(in-package #:match-nptcl)

;;
;;  tools
;;
(defmacro defun! (name &rest args)
  (when (fboundp name)
    (error "The function ~S is already exists." name))
  `(defun ,name ,@args))

(defmacro defmacro! (name &rest args)
  (when (fboundp name)
    (error "The macro ~S is already exists." name))
  `(defmacro ,name ,@args))

(defun single (x)
  (and (consp x)
       (null (cdr x))))

(defmacro dbind (&rest args)
  `(destructuring-bind ,@args))

(defmacro mvbind (&rest args)
  `(multiple-value-bind ,@args))

(defun assoc2 (&rest args)
  (let ((x (apply #'assoc args)))
    (if x
      (values (cdr x) t)
      (values nil nil))))

(defmacro or2 (&rest args)
  (when args
    (destructuring-bind (car . cdr) args
      (if cdr
        (let ((value (gensym))
              (check (gensym)))
          `(multiple-value-bind (,value ,check) ,car
             (if ,check
               (values ,value ,check)
               (or2 ,@cdr))))
        car))))

(defmacro mapfn ((var) expr &body body)
  `(mapcar
     (lambda (,var)
       ,@body)
     ,expr))

(defmacro dobind (bind data &body body)
  (let ((one (gensym)))
    `(dolist (,one ,data)
       (destructuring-bind ,bind ,one ,@body))))

(defmacro aif (expr then &optional else)
  (let ((g (gensym)))
    `(let ((,g ,expr))
       (if ,g
         (let ((it ,g))
           ,then)
         ,else))))

(defmacro awhen (expr &body body)
  (let ((g (gensym)))
    `(let ((,g ,expr))
       (if ,g
         (let ((it ,g))
           ,@body)))))

(defmacro aif2 (expr then &optional else)
  (let ((value (gensym))
        (check (gensym)))
    `(multiple-value-bind (,value ,check) ,expr
       (if ,check
         (let ((it ,value))
           ,then)
         ,else))))

(defmacro awhen2 (expr &body body)
  (let ((value (gensym))
        (check (gensym)))
    `(multiple-value-bind (,value ,check) ,expr
       (if ,check
         (let ((it ,value))
           (progn ,@body))))))


;;
;;  Queue
;;
(defstruct queue root tail)

(defun enqueue (inst value)
  (declare (type queue inst))
  (let ((cons (list value)))
    (if (queue-root inst)
      (setf (cdr (queue-tail inst)) cons
            (queue-tail inst) cons)
      (setf (queue-root inst) cons
            (queue-tail inst) cons)))
  value)


;;
;;  Error
;;
(define-condition match-error (simple-error) ())

(defun error! (control &rest arguments)
  (error (make-condition 'match-error
                         :format-control control
                         :format-arguments arguments)))

(in-package #:match-nptcl)

;;
;;  (defrule name goal.1 goal.2)   -> clause.1
;;  (defrule name goal.3 goal.4)   -> clause.2
;;  (gethash 'name *rule*)         -> rule
;;  clause.1 -> (and pattern-match.1 goal.1 goal.2)
;;  clause.2 -> (and pattern-match.2 goal.3 goal.4)
;;  rule     -> (or clause.1 clause.2)
;;
(defstruct clause
  name index args vars expr gensymp)
(defstruct (rule (:constructor make-rule-empty))
  name index queue)


;;
;;  Rule
;;
(defun! make-rule (name)
  (declare (type symbol name))
  (let ((queue (make-queue)))
    (the rule (make-rule-empty :name name :index 0 :queue queue))))

(defun! rule-enqueue (rule clause)
  (declare (type rule rule)
           (type clause clause))
  (let ((index (rule-index rule)))
    (setf (clause-index clause) index)
    (incf index 1)
    (setf (rule-index rule) index)
    (enqueue (rule-queue rule) clause))
  (values))

(defun! rule-list (rule)
  (queue-root
    (rule-queue rule)))


;;
;;  Rule Table
;;
(defvar *rule*)
(defvar *variable-index*)

(defun! init-match ()
  (setq *rule* (make-hash-table :test 'eq))
  (setq *variable-index* 0)
  (values))

(defun! free-match ()
  (makunbound '*rule*)
  (makunbound '*variable-index*)
  (values))

(defun! clear-match ()
  (clrhash *rule*)
  (values))

(defmacro! with-match (&body body)
  `(let ((*rule*) (*variable-index* 0))
     (init-match)
     ,@body))

(defun! rule-get (name)
  (declare (type symbol name))
  (or2 (gethash name *rule*)
       (error! "The rule ~S is not exist." name)))

(defun! rule-push-new (name clause)
  (declare (type symbol name)
           (type clause clause))
  (let ((rule (make-rule name)))
    (rule-enqueue rule clause)
    (setf (gethash name *rule*) rule))
  (values))

(defun! rule-push (clause)
  (declare (type clause clause))
  (let ((name (clause-name clause)))
    (aif2 (gethash name *rule*)
      (rule-enqueue it clause)
      (rule-push-new name clause)))
  (values))


;;
;;  Variable
;;
(defun! symbol-char? (c x n)
  (and (symbolp x)
       (let ((x (symbol-name x)))
         (and (<= n (length x))
              (char= (char x 0) c)))))

(defun! var? (x)
  (symbol-char? #\? x 2))

(defun! any? (x)
  (symbol-char? #\_ x 1))

(defun! make-variable (v)
  (let ((name (symbol-name v))
        (index (incf *variable-index* 1)))
    (make-symbol
      (format nil "~A-~A" name index))))

(defvar *make-expr-table*)
(defvar *make-expr-list*)

(defun! make-expr-var (v gensymp)
  (if gensymp
    (or2 (assoc2 v *make-expr-table* :test #'eq)
         (let ((g (make-variable v)))
           (push (cons v g) *make-expr-table*)
           (push g *make-expr-list*)
           g))
    (progn
      (pushnew v *make-expr-list* :test #'eq)
      v)))

(defun! make-expr-call (v gensymp)
  (cond ((var? v)
         (make-expr-var v gensymp))
        ((consp v)
         (cons (make-expr-call (car v) gensymp)
               (make-expr-call (cdr v) gensymp)))
        ((simple-vector-p v)
         (map 'vector (lambda (x) (make-expr-call x gensymp)) v))
        (t v)))

(defun! make-expr2 (v1 v2 gensymp)
  (let (*make-expr-table* *make-expr-list*)
    (values
      (make-expr-call v1 gensymp)
      (make-expr-call v2 gensymp)
      (nreverse *make-expr-list*))))

(defun! parse-clause (name args expr gensymp)
  (mvbind (args expr vars) (make-expr2 args expr gensymp)
    (unless (symbolp name)
      (error! "The name ~S must be a symbol." name))
    (make-clause
      :name name :args args :expr expr :vars vars :gensymp gensymp)))


;;
;;  Define Rule
;;
(defun! delete-rule (name)
  (remhash name *rule*))

(defun! define-rule-name (name)
  (etypecase name
    (symbol (values name nil))
    (list (values (car name) (cdr name)))))

(defun! define-rule (name &optional (expr 'true))
  (mvbind (name args) (define-rule-name name)
    (rule-push
      (parse-clause name args expr t))))

(defmacro! defrule (name &rest goals)
  (when (symbolp name)
    (setq name (list name)))
  `(define-rule ',name '(and ,@goals)))

(in-package #:match-nptcl)

;;
;;  bind-get
;;
(defvar *bind-route*)

(defun! bind-find-assoc2 (v list)
  (awhen (member v list :key #'car :test #'eq)
    (dbind (cons . next) it
      (if (member cons *bind-route* :test #'eq)
        (bind-find-assoc2 v next)
        (progn
          (push cons *bind-route*)
          (values (cdr cons) t))))))

(defun! bind-find-var (v bind)
  (aif2 (bind-find-assoc2 v bind)
    (if (var? it)
      (bind-find-var it bind)
      (values it t))))

(defun! bind-find (v bind)
  (let (*bind-route*)
    (bind-find-var v bind)))


;;
;;  lexical
;;
(defvar *lexical*)
(defstruct lexical next bind vars)

(declaim (ftype function match-find-stack))

(defun! match-find-var (v bind)
  (aif2 (bind-find-assoc2 v bind)
    (if (var? it)
      (match-find-stack it *lexical*)
      (values it t))))

(defun! match-find-stack (v stack)
  (when stack
    (or2 (match-find-var v (lexical-bind stack))
         (unless (member v (lexical-vars stack) :test #'eq)
           (match-find-stack v (lexical-next stack))))))

(defun! match-find-quick (v stack)
  (when stack
    (or2 (assoc2 v (lexical-bind stack) :test #'eq)
         (unless (member v (lexical-vars stack) :test #'eq)
           (match-find-quick v (lexical-next stack))))))

(defun! match-find (v)
  (let (*bind-route*)
    (aif2 (match-find-quick v *lexical*)
      (if (var? it)
        (match-find-stack it *lexical*)
        (values it t)))))

(defun! lexical-current-set (v value &optional (stack *lexical*))
  (when stack
    (let ((list (lexical-bind stack)))
      (push (cons v value) list)
      (setf (lexical-bind stack) list)
      t)))

(defun! lexical-current-undefined-p (v)
  (mvbind (var check) (match-find v)
    (or (null check)
        (var? var))))

;;  variable
(defmacro! with-variable (&body body)
  `(let (*lexical*)
     ,@body))

(defun! lexical-get-error (var)
  (or2 (match-find var)
       (error! "Invalid variable, ~S." var)))

(defun! variable-replace (value)
  (cond ((var? value)
         (variable-replace
           (lexical-get-error value)))
        ((consp value)
         (cons (variable-replace (car value))
               (variable-replace (cdr value))))
        ((simple-vector-p value)
         (map 'vector #'variable-replace value))
        (t value)))

(defun! variable-get (var)
  (aif2 (match-find var)
    (values (variable-replace it) t)
    (error! "Invalid variable, ~S." var)))

(defun! variable-set (var value)
  (or (lexical-current-set var value)
      (error! "Invalid variable, ~S." var)))

(defun! variable-undefined-p (var)
  (lexical-current-undefined-p var))

(defun! variable-push (bind vars)
  (let ((lexical (make-lexical :next *lexical* :bind bind :vars vars)))
    (setq *lexical* lexical)))

(defun! variable-pop (check)
  (unless (eq *lexical* check)
    (error! "bind error."))
  (setq *lexical* (lexical-next *lexical*)))

(defun! bind-replace (value)
  (cond ((var? value)
         (aif2 (match-find value)
           (bind-replace it)
           value))
        ((consp value)
         (cons (bind-replace (car value))
               (bind-replace (cdr value))))
        ((simple-vector-p value)
         (map 'vector #'bind-replace value))
        (t value)))


;;
;;  bind-match
;;
(defvar *bind-table*)

(declaim (ftype (function (* *) *) bind-any))

(defun! bind-var2 (x y)
  (mvbind (a1 b1) (bind-find x *bind-table*)
    (mvbind (a2 b2) (bind-find y *bind-table*)
      (cond ((and b1 b2) (bind-any a1 a2))
            (b1 (bind-any a1 y))
            (b2 (bind-any x a2))
            (t (progn
                 (push (cons x y) *bind-table*)
                 (push (cons y x) *bind-table*)
                 t))))))

(defun! bind-varx (x y)
  (aif2 (bind-find x *bind-table*)
    (bind-any it y)
    (progn
      (push (cons x y) *bind-table*)
      t)))

(defun! bind-vary (x y)
  (aif2 (bind-find y *bind-table*)
    (bind-any x it)
    (progn
      (push (cons y x) *bind-table*)
      t)))

(defun! bind-cons (x y)
  (and (consp x)
       (consp y)
       (dbind (car1 . cdr1) x
         (dbind (car2 . cdr2) y
           (and (bind-any car1 car2)
                (bind-any cdr1 cdr2))))))

(defun! bind-vector (x y)
  (and (simple-vector-p x)
       (simple-vector-p y)
       (let ((size1 (length x))
             (size2 (length y)))
         (when (= size1 size2)
           (dotimes (i size1 t)
             (unless (bind-any (aref x i) (aref y i))
               (return nil)))))))

(defun! bind-var (x y)
  (let ((xp (var? x))
        (yp (var? y)))
    (cond ((and xp yp) (bind-var2 x y))
          (xp (bind-varx x y))
          (yp (bind-vary x y))
          ((consp x) (bind-cons x y))
          ((simple-vector-p x) (bind-vector x y))
          ((vectorp x) (equal x y))
          (t (eql x y)))))

(defun! bind-any (x y)
  (or (any? x)
      (any? y)
      (bind-var x y)))

(defun! bind-match (x y)
  (let (*bind-table*)
    (when (bind-any x y)
      (values *bind-table* t))))

(in-package #:match-nptcl)

;;
;;  Match
;;
(defvar *match-level*)
(defvar *match-cut*)

(defmacro! with-execute (&body body)
  `(with-variable
     (let ((*match-level* 0) *match-cut*)
       ,@body)))


;;
;;  Run
;;
(declaim (ftype function run-lambda))

(defun! run-true (next)
  (lambda ()
    (funcall next)))

(defun! run-fail (next)
  (declare (ignore next))
  (lambda () nil))

(defun! run-cut (next)
  (lambda ()
    (or (funcall next)
        (progn
          (setq *match-cut* *match-level*)
          nil))))

;;  and
(defun! run-and-list (cdr next)
  (if cdr
    (dbind (car . cdr) cdr
      (let* ((and-next (run-and-list cdr next))
             (car-next (run-lambda car and-next)))
        (lambda ()
          (funcall car-next))))
    next))

(defun! run-and (cdr next)
  (cond ((null cdr) (run-true next))
        ((single cdr) (run-lambda (car cdr) next))
        (t (run-and-list cdr next))))

;;  or
(defun! run-or-list (cdr next)
  (lambda ()
    (dolist (x cdr)
      (when (funcall (run-lambda x next))
        (return t))
      (when *match-cut*
        (return nil)))))

(defun! run-or (cdr next)
  (cond ((null cdr) (run-fail next))
        ((single cdr) (run-lambda (car cdr) next))
        (t (run-or-list cdr next))))

;;  is
(defun! run-is-var (var value next)
  (cond ((variable-undefined-p var)
         (variable-set var value)
         (funcall next))
        ((eql (variable-get var) value)
         (funcall next))
        (t (error! "The variable ~S already set." var))))

(defun! run-is-eql (var value next)
  (unless (eql var value)
    (error! "Invalid is call, ~S, ~S." var value))
  (funcall next))

(defun! run-is (cdr next)
  (dbind (var expr) cdr
    (lambda ()
      (let ((value (eval (variable-replace expr))))
        (cond ((any? var) (funcall next))
              ((var? var) (run-is-var var value next))
              (t (run-is-eql var value next)))))))

;;  progn
(defun! run-progn (cdr next)
  (lambda ()
    (when (eval `(progn ,@(variable-replace cdr)))
      (funcall next))))

;;  clause
(defun! run-clause-args (clause)
  (make-expr2
    (clause-args clause)
    (clause-expr clause)
    (clause-gensymp clause)))

(defun! run-clause (next clause cdr)
  (mvbind (args expr vars) (run-clause-args clause)
    (setq cdr (bind-replace cdr))
    (awhen2 (bind-match args cdr)
      (let ((lexical (variable-push it vars)))
        (prog1 (funcall (run-lambda expr next))
          (variable-pop lexical))))))

;;  run-lambda
(defun! run-lambda-next (next level)
  (incf *match-level* 1)
  (lambda ()
    (prog1 (funcall next)
      (setq *match-level* level))))

(defun! run-lambda-cut (level)
  (prog1 *match-cut*
    (when (eql *match-cut* level)
      (setq *match-cut* nil))))

(defun! run-lambda-call (car cdr next)
  (lambda ()
    (let* ((rule (rule-get car))
           (level *match-level*)
           (next (run-lambda-next next level)))
      (dolist (clause (rule-list rule))
        (when (run-clause next clause cdr)
          (return t))
        (when (run-lambda-cut level)
          (return nil))))))

(defun! run-lambda-p (expr symbol)
  (and (symbolp expr)
       (string= (symbol-name expr) (symbol-name symbol))))

(defun! run-lambda-list (list next)
  (dbind (car . cdr) list
    (cond ((run-lambda-p car 'and) (run-and cdr next))
          ((run-lambda-p car 'or) (run-or cdr next))
          ((run-lambda-p car 'is) (run-is cdr next))
          ((run-lambda-p car 'progn) (run-progn cdr next))
          (t (run-lambda-call car cdr next)))))

(defun! run-lambda-symbol (expr next)
  (if (var? expr)
    (run-lambda (variable-get expr) next)
    (run-lambda-call expr nil next)))

(defun! run-lambda (expr next)
  (cond ((run-lambda-p expr 'true) (run-true next))
        ((run-lambda-p expr 'fail) (run-fail next))
        ((run-lambda-p expr '!) (run-cut next))
        ((symbolp expr) (run-lambda-symbol expr next))
        ((consp expr) (run-lambda-list expr next))
        (t (error! "Invalid value, ~S." expr))))


;;
;;  Interface
;;
(defun! match-execute-next (clause next)
  (lambda ()
    (prog1 (run-clause next clause nil)
      (setq *match-cut* nil))))

(defun! match-execute (clause next)
  (with-execute
    (funcall (match-execute-next clause next))))

(defun! match-lisp-clause (name expr)
  (parse-clause (make-symbol name) nil expr nil))

(defun! match-lisp-alist (clause)
  (mapfn (x) (clause-vars clause)
    (cons x (variable-get x))))

(defun! match-lisp (expr call)
  (let ((clause (match-lisp-clause "MATCH" expr))
        list check)
    (match-execute
      clause
      (lambda ()
        (setq list (match-lisp-alist clause))
        (setq check (funcall call list))))
    (if check
      (values list check)
      (values nil nil))))

(defun! match-true (expr)
  (match-lisp expr (lambda (list) (declare (ignore list)) t)))

(defun! match-fail (expr)
  (match-lisp expr (lambda (list) (declare (ignore list)) nil)))

(defmacro! match (&rest args)
  `(match-true '(and ,@args)))

(defun! query-lisp (expr)
  (match-lisp
    expr
    (lambda (list)
      (let ((s *query-io*))
        (fresh-line s)
        (dobind (x . y) list
          (format s "~S = ~S~%" x y))
        (y-or-n-p)))))

(defmacro! query (&rest args)
  `(query-lisp '(and ,@args)))

