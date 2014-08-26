
(defpackage #:cl-parsing-lambda-lists
  (:use "COMMON-LISP")
  (:documentation
   "A small package of utility functions for parsing different types of
lambda lists.  This functionality is present in every implementation in
some form or another, but lack of a standard interface makes it
difficult to write macros that proces lambda lists."))

(in-package #:cl-parsing-lambda-lists)

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
      (macrolet ((parse-keyword ((&rest symbols) &body body)
                   `(progn
                      (when (and (not (atom list))
                                 (member (first list) ',symbols))
                        (save (pop list))
                        ,@body)))
                 (doparameters ((var) &body body)
                   `(do () ((or (atom list) (member (first list) keywords)))
                      (save (let ((,var (pop list)))
                              ,@body)))))
        (parse-keyword (&whole)
          (save (handle :whole (pop list))))
        (doparameters (required)
         (handle :required required))
        (parse-keyword (&optional)
         (doparameters (opt)
          (if (symbolp opt)
              (handle :optional opt)
              (list* (handle :optional (first opt)) (rest opt)))))
        (when (and (atom list) (not (null list))) ; turn (... . REST) 
          (setq list (list '&rest list)))         ; into (... &rest REST)
        (parse-keyword (&rest &body)
         (save (handle :rest (pop list))))
        (parse-keyword (&key)
         (doparameters (key)
          (if (symbolp key)
              (handle :key key)
              (destructuring-bind (keyspec . more) key
                (if (symbolp keyspec)
                    (list* (handle :key keyspec) more)
                    (destructuring-bind (keyword var) keyspec
                      (list* (list keyword (handle :key var)) more)))))))
        (parse-keyword (&allow-other-keys))
        (parse-keyword (&aux)
         (doparameters (aux) aux))
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
