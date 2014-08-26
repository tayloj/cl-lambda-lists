
(defpackage #:cl-parsing-lambda-lists
  (:use "COMMON-LISP")
  (:documentation
   "A small package of utility functions for parsing different types of
lambda lists.  This functionality is present in every implementation in
some form or another, but lack of a standard interface makes it
difficult to write macros that proces lambda lists."))

(in-package #:cl-parsing-lambda-lists)

(defun variablep (object &optional environment)
  "Returns true if object is non-constant symbol."
  (and (symbolp object)
       (not (constantp object environment))))

(defun to-list (object)
  "If object is a list, returns object, otherwise returns a
one-element list containing object."
  (if (listp object) object
      (list object)))

(defun parse-ordinary-lambda-list (list &optional environment)
  "Returns six values: required parameters, optional parameter
specifiers, the rest parameter or NIL, keyword parameter specifiers, a
boolean indicating whether &allow-other-keys was specified, auxiliary
parameter specifiers, and a boolean indicating whether any variable is
repeated. Implementations vary on whether repeated variables are
allowed. See:
https://groups.google.com/d/msg/comp.lang.lisp/hh834A0xThQ/IWCuJFhMzzwJ"
  (let ((orig list)
        (keywords '(&allow-other-keys &key &rest &aux &optional))
        (reqvars '())
        (optvars '())
        (restvar nil)
        (keyvars '())
        (allow-other-keys-p nil)
        (auxvars '())
        (all-vars '()))
    (flet ((check-variable (x)
             (assert (variablep x environment) () "Bad variable: ~S." x)
             (push x all-vars)))
      (macrolet ((do-parameters ((var) &body body)
                   `(do () ((or (endp list) (member (first list) keywords)))
                      (let ((,var (pop list)))
                        ,@body)))
                 (with-keyword ((&rest keywords) &body body)
                   `(when (member (first list) ',keywords)
                      (pop list)
                      ,@body)))
        (do-parameters (req)
          (check-variable req)
          (push req reqvars))
        (with-keyword (&optional)
          (do-parameters (opt)
            (destructuring-bind (x &optional (dx nil dxp) (xp nil xpp)) 
                (to-list opt)
              (declare (ignore dx #-sbcl dxp))
              (check-variable x)
              (when xpp (check-variable xp))
              (push opt optvars))))
        (with-keyword (&rest)
          (let ((rest (pop list)))
            (check-variable rest)
            (setq restvar rest)))
        (with-keyword (&key) 
         (do-parameters (key)
            (destructuring-bind (x &optional (dx nil dxp) (xp nil xpp)) 
                (to-list key)
              (declare (ignore dx #-sbcl dxp))
              (when xpp (check-variable xp))
              (if (listp x)
                  (destructuring-bind (kx x) x
                    (assert (symbolp kx) () "Bad keyword argument name: ~S." kx)
                    (check-variable x))
                  (check-variable x))
              (push key keyvars))))
        (with-keyword (&allow-other-keys)
          (setq allow-other-keys-p t))
        (with-keyword (&aux)
          (do-parameters (aux)
            (destructuring-bind (var &optional form)
                (to-list aux)
              (declare (ignore form))
              (check-variable var)
              (push aux auxvars))))
        (unless (endp list)
          (error "Malformed lambda list: ~S." orig))
        (values (nreverse reqvars)
                (nreverse optvars)
                restvar
                (nreverse keyvars)
                allow-other-keys-p
                (nreverse auxvars))))))

(defun map-destructuring-lambda-list (function list)
  "Maps over a destructuring lambda list, and returns a new list like
the input, but each pattern variable replaced with the result of calling
function with the type of variable \(:whole, :required, :optional, :rest,
:key, or :aux\). "
  (let ((result '())
        (orig list)
        (keywords '(&allow-other-keys &aux &body
                    &key &optional &rest &whole)))
    (labels ((save (x)
               (push x result))
             (handle (type parameter)
               (etypecase parameter
                 (list (map-destructuring-lambda-list function parameter))
                 (symbol (funcall function type parameter)))))
      (macrolet ((with-keyword ((&rest symbols) &body body)
                   `(progn
                      (when (and (not (atom list))
                                 (member (first list) ',symbols))
                        (save (pop list))
                        ,@body)))
                 (do-parameters ((var) &body body)
                   `(do () ((or (atom list) (member (first list) keywords)))
                      (save (let ((,var (pop list)))
                              ,@body)))))
        (with-keyword (&whole)
          (save (handle :whole (pop list))))
        (do-parameters (required)
          (handle :required required))
        (with-keyword (&optional)
          (do-parameters (opt)
            (if (symbolp opt)
                (handle :optional opt)
                (list* (handle :optional (first opt)) (rest opt)))))
        (when (and (atom list) (not (null list))) ; turn (... . REST) 
          (setq list (list '&rest list))) ; into (... &rest REST)
        (with-keyword (&rest &body)
          (save (handle :rest (pop list))))
        (with-keyword (&key)
          (do-parameters (key)
            (if (symbolp key)
                (handle :key key)
                (destructuring-bind (keyspec . more) key
                  (if (symbolp keyspec)
                      (list* (handle :key keyspec) more)
                      (destructuring-bind (keyword var) keyspec
                        (list* (list keyword (handle :key var)) more)))))))
        (with-keyword (&allow-other-keys))
        (with-keyword (&aux)
          (do-parameters (aux) aux))
        (unless (null list)
          (error "Bad destructuring lambda list: ~A." orig))
        (nreverse result)))))

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
