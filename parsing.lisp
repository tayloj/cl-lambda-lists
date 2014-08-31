
(defpackage #:cl-parsing-lambda-lists
  (:use "COMMON-LISP")
  (:documentation
   "A small package of utility functions for parsing different types of
lambda lists.  This functionality is present in every implementation in
some form or another, but lack of a standard interface makes it
difficult to write macros that proces lambda lists."))

(in-package #:cl-parsing-lambda-lists)

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defun to-list (object)
    "If object is a list, returns object, otherwise returns a
one-element list containing object."
    (if (listp object) object
        (list object))))

(defmacro do-parameters ((var lambda-list &optional (result nil)) &body body)
  "Iterate though lambda list parameter specifiers pulled from
LAMBDA-LIST until either a lambda-list keyword or the end of the
lambda-list (which may be an improper list) is encountered.  The tail of
the list is stored back into LAMBDA-LIST (which must be a place)."
  (let ((list (gensym (symbol-name '#:list-))))
    `(do ((,list ,lambda-list))
         ((or (atom ,list) 
              (member (first ,list) lambda-list-keywords))
          (setf ,lambda-list ,list)
          ,result)
       (let ((,var (pop ,list)))
         ,@body))))

(defun %with-keyword (keywords list function)
  "If LIST is a non-empty list and its first element is in the list
designated by KEYWORDS, calls FUNCTION with the first element of LIST.
Otherwise returns NIL."
  (when (and (consp list) 
             (member (first list) keywords))
    (funcall function (first list))))

(defmacro with-keyword ((keywords
                         list
                         &optional (var (gensym (symbol-name '#:ll-keyword-)) varp))
                        &body body)
  "A simple MACRO wrapper around %WITH-KEYWORD."
  `(%with-keyword 
    ',(to-list keywords) ,list
    #'(lambda (,var)
        ,@(unless varp `((declare (ignore ,var)))) 
        (pop ,list)
        ,@body)))

(defun variablep (x &optional env)
  "Retuns true if X can be a variable name; i.e., X is a symbol and is
not a constant in ENV."
  (and (symbolp x)
       (not (constantp x env))))

(defun check-variable (var &optional env)
  "Signals an error unless (VARIABLEP VAR ENV) is true."
  (unless (variablep var env)
    (error "Bad variable: ~S." var)))

(defun check-keyword-argument-name (x)
  "Signals an error unless (SYMBOLP X) is true."
  (unless (symbolp x)
    (error "Bad keyword argument name: ~S." x)))

(defun parse-lambda-list (specifications list
                          &optional env
                          &aux (results '()) (orig list))
  "SPECIFICATIONS is a list of parameter specifications.  Each
specification is either a symbol or a list of a symbol or function.  The
symbol should be either the keyword :REQUIRED or one of the standard
lambda list keywords (i.e., &OPTIONAL, &REST, &BODY, &KEY,
&ALLOW-OTHER-KEYS, &AUX, &WHOLE, or &ENVIRONMENT).  If the specification
is of the second form and has a function, then the function is used to
check the parameter specifer; otherwise the 'standard' form is
assumed.

LIST is list to be parsed.

ENV is an environment.

Multiple values are returned: one per each element of specifications,
with the exception of &KEY, for which two values are returned (the first
is the list of keyword parameter specifiers; the second is a boolean
indicating whether &ALLOW-OTHER-KEYS was specified."
  (dolist (part specifications)
    (destructuring-bind (part &optional (check nil checkp))
        (to-list part)
      (ecase part
        ((:required)
         (let ((reqs '()))
           (do-parameters (req list)
             (if checkp (funcall check req env)
                 (check-variable req))
             (push req reqs))
           (push (nreverse reqs) results)))
        ((&optional)
         (let ((opts '()))
           (with-keyword (&optional list)
             (do-parameters (opt list)
               (if checkp (funcall check opt env)
                   (destructuring-bind (x &optional (dx nil dxp) (xp nil xpp))
                       (to-list opt)
                     (declare (ignore dx #-sbcl dxp))
                     (check-variable x env)
                     (when xpp (check-variable xp env))))
               (push opt opts)))
           (push (nreverse opts) results)))
        ((:dotted)
         ;; Destructuring lambda lists allow (... . VAR) as a variant of
         ;; (... &REST VAR).  When LIST becomes VAR, we simply modify
         ;; LIST to be (&REST VAR).
         (when (and (atom list) (not (null list)))
           (setq list (list '&rest list))))
        ((:body-to-rest)
         ;; Destructuring lambda lists allow (... [&BODY|&REST] VAR ...). 
         ;; To simplify processing, we convert &BODY to &REST
         (with-keyword (&body list)
           (push '&rest list)))
        ((&whole &environment &rest &body)
         (let ((parm nil))
           (%with-keyword 
            (list part) list 
            #'(lambda (ll-keyword)
                (pop list)              ; remove PART
                (unless (consp list)
                  (error "Unexpected end of list after ~S." ll-keyword))
                (when (member (first list) lambda-list-keywords)
                  (error "Missing parameter after lambda-list keyword: ~s." part))
                (setq parm (pop list))  ; remove VAR
                (if checkp (funcall check parm env)
                    (check-variable parm env))))
           (push parm results)))
        ((&key)
         (let ((keys '())
               (allow-other-keys-p nil))
           (with-keyword (&key list) 
             (do-parameters (key list)
               (if checkp (funcall check key env)
                   (destructuring-bind (x &optional (dx nil dxp) (xp nil xpp))
                       (to-list key)
                     (declare (ignore dx #-sbcl dxp))
                     (when xpp (check-variable xp env))
                     (if (listp x)
                         (destructuring-bind (kx x) x
                           (check-keyword-argument-name kx)
                           (check-variable x env))
                         (check-variable x env))))
               (push key keys))
             (with-keyword (&allow-other-keys list)
               (setq allow-other-keys-p t)))
           (push (nreverse keys) results)
           (push allow-other-keys-p results)))
        ((&aux)
         (let ((auxes '()))
           (with-keyword (&aux list)
             (do-parameters (aux list)
               (if checkp (funcall check aux env)
                   (destructuring-bind (var &optional form) 
                       (to-list aux)
                     (declare (ignore form))
                     (check-variable var env)))
               (push aux auxes)))
           (push (nreverse auxes) results))))))
  (unless (endp list)
    (error "Malformed lambda list: ~S." orig))
  (values-list (nreverse results)))

(defun parse-ordinary-lambda-list (list &optional env)
  (parse-lambda-list '(:required &optional &rest &key &aux) list env))

(defun parse-generic-function-lambda-list (list &optional env)
  (flet ((check-opt (opt)
           "no default value allowed"
           (destructuring-bind (x) (to-list opt)
             (check-variable x env)))
         (check-key (key)
           "no default value allowed"
           (destructuring-bind (x) (to-list key)
             (if (listp x)
                 (destructuring-bind (kx x) x
                   (check-keyword-argument-name kx)
                   (check-variable x env))
                 (check-variable x env)))))
    (parse-lambda-list 
     `(:required (&optional ,#'check-opt) &rest (&key ,#'check-key))
     list env)))

(defun parse-specialized-lambda-list (list &optional env)
  (flet ((check-req (req)
           "specializers are allowed"
           (destructuring-bind (var &optional specializer)
               (to-list req)
             (declare (ignore specializer))
             (check-variable var env))))
    (parse-lambda-list
     `((:required ,#'check-req) &optional &rest &key &aux)
     list env)))

(defun parse-boa-lambda-list (list &optional env)
  (parse-ordinary-lambda-list list env))

(defun parse-defsetf-lambda-list (list &optional env)
  (parse-lambda-list '(:required &optional &rest &key &environment) list env))

(defun parse-define-modify-macro-lambda-list (list &optional env)
  (parse-lambda-list '(:required &optional &rest) list env))

(defun parse-method-combination-lambda-list (list &optional env)
  (parse-lambda-list '(&whole :required &optional &rest &key &aux) list env))

(defun parse-macro-lambda-list (list &optional env)
  (parse-destructuring-lambda-list list env t))

(defun parse-deftype-lambda-list (list &optional env)
  (parse-macro-lambda-list list env))

(defun parse-destructuring-lambda-list (list 
                                        &optional env allow-environment-p
                                        &aux (orig list))
  ;; This implements the parsing necessary for destructuring lambda
  ;; lists as well as macro lambda lists.  The only difference is that
  ;; macro lambda lists can have an &environment parameter at the top
  ;; level (but not at nested levels).
  (flet ((handle-env (e env)
           "If environment variables are allowed (macro lambda list
            mode), then an &environment parameter should be a variable. 
            Otherwise it's not permitted, and an error is signalled."
           (if allow-environment-p 
               (check-variable e env)
               (error "&environment variable not allowed here: ~S." e)))
         (pdll (x env)
           "If x is a list (including the empty list), then it is
            recursively parsed as a destructuring lambda list (but
            &environment will not be allowed in any case).  Otherwise,
            it should be a variable."
           (if (listp x)
               (parse-destructuring-lambda-list x env nil)
               (check-variable x env))))
    ;; We bind each of the environment positions separately.  
    ;; At the end we'll make sure that at most one had a value.
    (multiple-value-bind (whole e1 required e2 optional e3
                                rest e4 key allow e5 aux e6)
        (parse-lambda-list
         `((&whole       ,#'pdll)
           (&environment ,#'handle-env)
           (:required    ,#'pdll)
           (&environment ,#'handle-env)
           (&optional    ,#'(lambda (opt env)
                              (destructuring-bind (x &optional xd (xp nil xpp))
                                  (to-list opt)
                                (declare (ignore xd))
                                (pdll x env)
                                (when xpp (check-variable xp env)))))
           (&environment ,#'handle-env)
           :dotted :body-to-rest ; (... . x) | (... &body x) => (... &rest x)
           (&rest        ,#'pdll)
           (&environment ,#'handle-env)
           (&key         ,#'(lambda (key env)
                              (destructuring-bind (k &optional kd (kp nil kpp))
                                  (to-list key)
                                (declare (ignore kd))
                                (when kpp (check-variable kp env))
                                (if (symbolp k)
                                    (check-variable k env)
                                    (destructuring-bind (k v) k
                                      (check-keyword-argument-name k)
                                      (pdll v env))))))
           (&environment ,#'handle-env)
           &aux                         ; same as usual
           (&environment ,#'handle-env))
         list env)
      (let ((nenv (count-if-not 'null (list e1 e2 e3 e4 e5 e6))))
        (if (or (eql nenv 0)
                (and allow-environment-p (eql nenv 1)))
            (values whole required optional rest key allow aux 
                    (or e1 e2 e3 e4 e5 e6))
            (error "Malformed lambda list: ~S." orig))))))

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
                            (list* (list keyword (handle :key var)) more)))))))
        (with-keyword (&allow-other-keys list)
          (save '&allow-other-keys)))
      (with-keyword (&aux list)
        (save '&aux)
        (do-parameters (aux list)
          (save aux)))
      (unless (null list)
        (error "Bad destructuring lambda list: ~A." orig))
      (nreverse result))))
