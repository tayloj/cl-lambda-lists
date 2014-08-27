
(defpackage #:cl-parsing-lambda-lists
  (:use "COMMON-LISP")
  (:documentation
   "A small package of utility functions for parsing different types of
lambda lists.  This functionality is present in every implementation in
some form or another, but lack of a standard interface makes it
difficult to write macros that proces lambda lists."))

(in-package #:cl-parsing-lambda-lists)

(defun to-list (object)
  "If object is a list, returns object, otherwise returns a
one-element list containing object."
  (if (listp object) object
      (list object)))

(defun lambda-list-keyword-p (x)
  "Returns true if x is in LAMBDA-LIST-KEYWORDS."
  (member x lambda-list-keywords))

(defmacro do-parameters ((var lambda-list &optional (result nil)) &body body)
  "Iterate though lambda list parameter specifiers pulled from
LAMBDA-LIST until either a lambda-list keyword or the end of the
lambda-list (which may be an improper list) is encountered.  The tail of
the list is stored back into LAMBDA-LIST (which must be a place)."
  (let ((list (gensym (symbol-name '#:list-))))
    `(do ((,list ,lambda-list))
         ((or (atom ,list) (lambda-list-keyword-p (first ,list)))
          (setf ,lambda-list ,list)
          ,result)
       (let ((,var (pop ,list)))
         ,@body))))

(defmacro with-keyword ((keywords list &optional (var nil varp)) &body body)
  "Evalute BODY if the first element of LIST is one of KEYWORDS.
The first element is popped off from LIST and bound to VAR in 
the scope of BODY."
  (let ((l (gensym (symbol-name '#:list-))))
    `(let ((,l ,list))
       (when (and (listp ,l) (member (first ,l) ',(to-list keywords)))
         ,(if varp 
              `(let ((,var (pop ,list)))
                 ,@body)
              `(progn
                 (pop ,list)
                 ,@body))))))

(defun check-variable (x &optional env)
  "Assert that is a non-constant symbol in the environment."
  (assert (and (symbolp x) (not (constantp x env)))
          () "Bad variable: ~S." x))

(defun parse-ordinary-lambda-list (list &optional env)
  "Returns as multiple values: required parameters, optional parameter
specifiers, the rest parameter or NIL, keyword parameter specifiers, a
boolean indicating whether &allow-other-keys was specified, auxiliary
parameter specifiers. Implementations vary on whether repeated variables
are allowed. See, for context:
https://groups.google.com/d/msg/comp.lang.lisp/KdRq2_XMVGc/2oNbz4Pa-UcJ
https://groups.google.com/d/msg/comp.lang.lisp/hh834A0xThQ/IWCuJFhMzzwJ"
  (let ((orig list)
        (reqvars '())
        (optvars '())
        (restvar nil)
        (keyvars '())
        (allow-other-keys-p nil)
        (auxvars '()))
    (do-parameters (req list)
      (check-variable req env)
      (push req reqvars))
    (with-keyword (&optional list)
      (do-parameters (opt list)
        (destructuring-bind (x &optional (dx nil dxp) (xp nil xpp)) 
            (to-list opt)
          (declare (ignore dx #-sbcl dxp))
          (check-variable x env)
          (when xpp (check-variable xp) env)
          (push opt optvars))))
    (with-keyword (&rest list)
      (let ((rest (pop list)))
        (check-variable rest env)
        (setq restvar rest)))
    (with-keyword (&key list) 
      (do-parameters (key list)
        (destructuring-bind (x &optional (dx nil dxp) (xp nil xpp)) 
            (to-list key)
          (declare (ignore dx #-sbcl dxp))
          (when xpp (check-variable xp) env)
          (if (listp x)
              (destructuring-bind (kx x) x
                (assert (symbolp kx) () "Bad keyword argument name: ~S." kx)
                (check-variable x env))
              (check-variable x env))
          (push key keyvars))))
    (with-keyword (&allow-other-keys list)
      (setq allow-other-keys-p t))
    (with-keyword (&aux list)
      (do-parameters (aux list)
        (destructuring-bind (var &optional form)
            (to-list aux)
          (declare (ignore form))
          (check-variable var env)
          (push aux auxvars))))
    (unless (endp list)
      (error "Malformed lambda list: ~S." orig))
    (values (nreverse reqvars)
            (nreverse optvars)
            restvar
            (nreverse keyvars)
            allow-other-keys-p
            (nreverse auxvars))))

(defun parse-generic-function-lambda-list (list &optional env)
  (let ((orig list)
        (reqvars '())
        (optvars '())
        (restvar nil)
        (keyvars '())
        (allow-other-keys-p nil))
    (do-parameters (req list)
      (check-variable req env)
      (push req reqvars))
    (with-keyword (&optional list)
      (do-parameters (opt list)
        (destructuring-bind (x) (to-list opt)
          (check-variable x env)
          (push opt optvars))))
    (with-keyword (&rest list)
      (let ((rest (pop list)))
        (check-variable rest env)
        (setq restvar rest)))
    (with-keyword (&key list)
      (do-parameters (key list)
        (destructuring-bind (x) (to-list key)
          (if (listp x)
              (destructuring-bind (kx x) x
                (assert (symbolp kx) () "Bad keyword argument name: ~S." kx)
                (check-variable x env))
              (check-variable x env))
          (push key keyvars))))
    (with-keyword (&allow-other-keys list)
      (setq allow-other-keys-p t))
    (unless (endp list)
      (error "Malformed generic function lambda list: ~S." orig))
    (values (nreverse reqvars)
            (nreverse optvars)
            restvar
            (nreverse keyvars)
            allow-other-keys-p)))

(defun parse-specialized-lambda-list (list &optional env)
  (let ((orig list)
        (reqvars '())
        (optvars '())
        (restvar nil)
        (keyvars '())
        (allow-other-keys-p nil)
        (auxvars '()))
    (do-parameters (req list)
      (destructuring-bind (var &optional specializer)
          (to-list req)
        (declare (ignore specializer))
        (check-variable var env)
        (push req reqvars)))
    (with-keyword (&optional list)
      (do-parameters (opt list)
        (destructuring-bind (x &optional (dx nil dxp) (xp nil xpp)) 
            (to-list opt)
          (declare (ignore dx #-sbcl dxp))
          (check-variable x env)
          (when xpp (check-variable xp) env)
          (push opt optvars))))
    (with-keyword (&rest list)
      (let ((rest (pop list)))
        (check-variable rest env)
        (setq restvar rest)))
    (with-keyword (&key list)
      (do-parameters (key list)
        (destructuring-bind (x &optional (dx nil dxp) (xp nil xpp)) 
            (to-list key)
          (declare (ignore dx #-sbcl dxp))
          (when xpp (check-variable xp) env)
          (if (listp x)
              (destructuring-bind (kx x) x
                (assert (symbolp kx) () "Bad keyword argument name: ~S." kx)
                (check-variable x env))
              (check-variable x env))
          (push key keyvars))))
    (with-keyword (&allow-other-keys list)
      (setq allow-other-keys-p t))
    (with-keyword (&aux list)
      (do-parameters (aux list)
        (destructuring-bind (var &optional form)
            (to-list aux)
          (declare (ignore form))
          (check-variable var env)
          (push aux auxvars))))
    (unless (endp list)
      (error "Malformed lambda list: ~S." orig))
    (values (nreverse reqvars)
            (nreverse optvars)
            restvar
            (nreverse keyvars)
            allow-other-keys-p
            (nreverse auxvars))))

(defun parse-boa-lambda-list (list &optional environment)
  (parse-ordinary-lambda-list list environment))

(defun parse-defsetf-lambda-list (list &optional env)
  (let ((orig list)
        (reqvars '())
        (optvars '())
        (restvar nil)
        (keyvars '())
        (allow-other-keys-p nil)
        (envvar nil))
    (do-parameters (req list)
      (check-variable req env)
      (push req reqvars))
    (with-keyword (&optional list)
      (do-parameters (opt list)
        (destructuring-bind (x &optional (dx nil dxp) (xp nil xpp)) 
            (to-list opt)
          (declare (ignore dx #-sbcl dxp))
          (check-variable x env)
          (when xpp (check-variable xp))
          (push opt optvars))))
    (with-keyword (&rest list)
      (let ((rest (pop list)))
        (check-variable rest env)
        (setq restvar rest)))
    (with-keyword (&key list)
      (do-parameters (key list)
        (destructuring-bind (x &optional (dx nil dxp) (xp nil xpp)) 
            (to-list key)
          (declare (ignore dx #-sbcl dxp))
          (when xpp (check-variable xp) env)
          (if (listp x)
              (destructuring-bind (kx x) x
                (assert (symbolp kx) () "Bad keyword argument name: ~S." kx)
                (check-variable x env))
              (check-variable x env))
          (push key keyvars))))
    (with-keyword (&allow-other-keys list)
      (setq allow-other-keys-p t))
    (with-keyword (&environment list)
      (let ((env (pop list)))
        (check-variable env)
        (setq envvar env)))
    (unless (endp list)
      (error "Malformed lambda list: ~S." orig))
    (values (nreverse reqvars)
            (nreverse optvars)
            restvar
            (nreverse keyvars)
            allow-other-keys-p
            envvar)))

(defun parse-define-modify-macro-lambda-list (list &optional env)
  (let ((orig list)
        (reqvars '())
        (optvars '())
        (restvar nil))
    (do-parameters (req list)
      (check-variable req env)
      (push req reqvars))
    (with-keyword (&optional list)
      (do-parameters (opt list)
        (destructuring-bind (x &optional (dx nil dxp) (xp nil xpp)) 
            (to-list opt)
          (declare (ignore dx #-sbcl dxp))
          (check-variable x env)
          (when xpp (check-variable xp) env)
          (push opt optvars))))
    (with-keyword (&rest list)
      (let ((rest (pop list)))
        (check-variable rest env)
        (setq restvar rest)))
    (unless (endp list)
      (error "Malformed lambda list: ~S." orig))
    (values (nreverse reqvars)
            (nreverse optvars)
            restvar)))

(defun map-destructuring-lambda-list (function list)
  "Maps over a destructuring lambda list, and returns a new list like
the input, but each pattern variable replaced with the result of calling
function with the type of variable \(:whole, :required, :optional, :rest,
:key, or :aux\). "
  (let ((result '())
        (orig list))
    (labels ((save (x)
               (push x result))
             (handle (type parameter)
               (etypecase parameter
                 (list (map-destructuring-lambda-list function parameter))
                 (symbol (funcall function type parameter)))))
      (with-keyword (&whole list)
        (save '&whole)
        (save (handle :whole (pop list))))
      (do-parameters (required list)
        (save (handle :required required)))
      (with-keyword (&optional list)
        (save '&optional)
        (do-parameters (opt list)
          (save (if (symbolp opt)
                    (handle :optional opt)
                    (list* (handle :optional (first opt)) (rest opt))))))
      (when (and (atom list) (not (null list))) ; turn (... . REST) 
        (setq list (list '&rest list)))         ; into (... &rest REST)
      (with-keyword ((&rest &body) list rest-or-body)
        (save rest-or-body)
        (save (handle :rest (pop list))))
      (with-keyword (&key list)
        (save '&key)
        (do-parameters (key list)
          (save (if (symbolp key)
                    (handle :key key)
                    (destructuring-bind (keyspec . more) key
                      (if (symbolp keyspec)
                          (list* (handle :key keyspec) more)
                          (destructuring-bind (keyword var) keyspec
                            (list* (list keyword (handle :key var)) more))))))))
      (with-keyword (&allow-other-keys list)
        (save '&allow-other-keys))
      (with-keyword (&aux list)
        (save '&aux)
        (do-parameters (aux list)
          (save aux)))
      (unless (null list)
        (error "Bad destructuring lambda list: ~A." orig))
      (nreverse result))))

;; Used in an answer to http://stackoverflow.com/q/25463763/1281433
;; which asks about getting destructuring bind to ignore wildcard
;; pattern variables (e.g., those beginning with _).
#-(and)
(defmacro destructuring-bind* (lambda-list object &body body)
  (let* ((ignores '())
         (lambda-list (map-dll (lambda (type var)
                                 (declare (ignore type))
                                 (if (and (> (length (symbol-name var)) 0)
                                          (char= #\_ (char (symbol-name var) 0)))
                                     (let ((var (gensym)))
                                       (push var ignores)
                                       var)
                                     var))
                               lambda-list)))
    `(destructuring-bind ,lambda-list ,object
       (declare (ignore ,@(nreverse ignores)))
       ,@body)))
