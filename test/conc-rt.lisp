(cl:in-package #:common-lisp-user)
(defpackage #:conc-rt
  (:use #:common-lisp)
  (:export
    #:equalrt
    #:deftest
    #:deftest-error
    #:deftest-error!
    #:do-tests))
(in-package #:conc-rt)

(defconstant +error+ 'error)

;;
;;  equalrt
;;
(declaim (ftype (function (* *) boolean) equalrt))

(defun equalrt-cons (x y)
  (and (consp y)
       (equalrt (car x) (car y))
       (equalrt (cdr x) (cdr y))))

(defun equalrt-array-element (x y)
  (block nil
    (dotimes (i (array-total-size x))
      (unless (equalrt (row-major-aref x i)
                       (row-major-aref y i))
        (return nil)))
    t))

(defun equalrt-array (x y)
  (and (arrayp y)
       (equal (array-dimensions x)
              (array-dimensions y))
       (equalrt-array-element x y)))

(defun equalrt-hash-table-every (x y)
  (block nil
    (maphash
      (lambda (key value)
        (unless (equalrt (gethash key y) value)
          (return nil)))
      x)
    t))

(defun equalrt-hash-table (x y)
  (and (hash-table-p y)
       (eq (hash-table-test x)
           (hash-table-test y))
       (= (hash-table-count x)
          (hash-table-count y))
       (equalrt-hash-table-every x y)))

(defun equalrt-type (x y)
  (typecase x
    (cons (equalrt-cons x y))
    (array (equalrt-array x y))
    (pathname (equal x y))
    (hash-table (equalrt-hash-table x y))
    (otherwise nil)))

(defun equalrt (x y)
  (or (eql x y)
      (equalrt-type x y)))


;;
;;  variables
;;
(defvar *index*)
(defvar *entries*)
(defvar *entries-table*)
(defvar *entries-warning*)

;;  initialize
(setq *entries* (list nil))
(setq *entries-warning* (list nil))
(setq *entries-table* (make-hash-table :test 'eq))


;;
;;  rt
;;
(defun push-queue (symbol value)
  (let ((cons (symbol-value symbol))
        (new (cons value nil)))
    (if (car cons)
      (setf (cddr cons) new (cdr cons) new)
      (setf (car cons) new (cdr cons) new))
    nil))

(defun push-entries (name expr values)
  (multiple-value-bind (value check) (gethash name *entries-table*)
    (declare (ignore value))
    (if check
      (progn
        (warn "The deftest ~S is already exist." name)
        (push-queue '*entries-warning* name))
      (push-queue '*entries* name)))
  (setf (gethash name *entries-table*) (cons expr values))
  nil)


;;
;;  interface
;;
(defun rem-all-tests ()
  (setq *entries* (list nil))
  (setq *entries-warning* (list nil))
  (clrhash *entries-table*)
  nil)

(defmacro deftest (name expr &rest values)
  (declare (type symbol name))
  `(push-entries ',name ',expr ',values))

(defmacro deftest-error (name expr &optional (error 'error))
  `(deftest ,name
     (handler-case
       ,expr
       (,error () ',+error+))
     ,+error+))

(defmacro deftest-error! (name expr &optional (error 'error))
  `(deftest-error ,name
     (handler-bind ((warning #'muffle-warning))
       ,expr)
     ,error))


;;
;;  do-tests
;;
(defun do-tests-output (name expect actual)
  (let ((s *debug-io*)
        (check (equalrt expect actual)))
    (if check
      (format s "~&[RT] ~6@A: ~A~%" *index* name)
      (progn
        (format s "~&[ERROR] ~6@A: ~A~%" *index* name)
        (format s "  *** Expect:~{ ~S~}~%" expect)
        (format s "  *** Actual:~{ ~S~}~%" actual)))
    check))

(defun do-tests-test (name expr values)
  (do-tests-output
    name values
    (multiple-value-list
      (eval expr))))

(defun do-tests-class-name (c)
  (if (not (typep c 'class))
    "..."
    (let ((x (class-name c)))
      (if (not (symbolp x))
        (string x)
        (symbol-name x)))))

(defun do-tests-simple-condition (c)
  (format nil "~A: ~?"
          (do-tests-class-name (class-of c))
          (simple-condition-format-control c)
          (simple-condition-format-arguments c)))

(defun do-tests-unhandling (name values c)
  (let ((s *debug-io*))
    (format s "~&[ERROR] ~6@A: ~A~%" *index* name)
    (format s "  *** Expect:~{ ~S~}~%" values)
    (if (typep c 'simple-condition)
      (format s "  *** Actual: ~A" (do-tests-simple-condition c))
      (format s "  *** Actual: ~S" c)))
  nil)

(defun do-tests-call (name expr values)
  (handler-case
    (do-tests-test name expr values)
    (error (c)
      (do-tests-unhandling name values c))))

(defun do-tests-check (name)
  (multiple-value-bind (expr check) (gethash name *entries-table*)
    (unless check
      (error "The deftest ~S is not exist." name))
    (destructuring-bind (expr . values) expr
      (do-tests-call name expr values))))

(defun do-tests-error (count2)
  (unless (zerop count2)
    (let ((s *debug-io*))
      (format s "~%")
      (format s "*************~%")
      (format s "*** ERROR ***~%")
      (format s "*************~2%")
      (format s "ERROR = ~A~%" count2))))

(defun do-tests-duplicated ()
  (let ((list (car *entries-warning*))
        (s *debug-io*))
    (when list
      (format s "~&[DUPLICATED] These testcases is ignored.~%")
      (format s "  *** Testcase: ~A~2%" list))))

(defun do-tests-finish (count2)
  (let ((list (car *entries-warning*))
        (s *debug-io*))
    (when (and (null list)
               (zerop count2))
      (format s "~&OK.~%"))))

(defun do-tests-exit ()
  #+sbcl
  (progn
    (format *debug-io* "~&do-tests error.~%")
    (sb-ext:exit :code 1))
  #+npt
  (progn
    (format *debug-io* "~&do-tests error.~%")
    (npt-system:exit 1))
  (error "do-tests error."))

(defun do-tests-execute ()
  (let ((count2 0))
    (dolist (name (car *entries*))
      (unless (do-tests-check name)
        (incf count2 1))
      (incf *index* 1))
    (do-tests-error count2)
    (do-tests-duplicated)
    (do-tests-finish count2)
    (unless (zerop count2)
      (do-tests-exit))))

(defun do-tests (&key delete)
  (let ((*index* 1))
    (do-tests-execute))
  (when delete
    (rem-all-tests))
  nil)

