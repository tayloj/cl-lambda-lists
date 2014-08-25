
(defpackage #:cl-parsing-lambda-lists
  (:use "COMMON-LISP"))

(in-package #:cl-parsing-lambda-lists)

(defparameter +standard-lambda-list-keywords+
  '(&allow-other-keys &aux &body &environment &key &optional &rest &whole))

(defun parse-destructuring-lambda-list (dll)
  (parse-wholevar dll))

(defmacro parse-list ((list continue &optional (test 't testp)) &body body)
  `(cond 
     ((null ,list) '())
     ((atom ,list) (,continue ,list))
     ,@(if testp 
           `((,test ,@body)
             ((,continue ,list)))
           `((,test ,@body)))))

(defun parse-wholevar (dll)
  (parse-list (dll parse-reqvars (eq '&whole (first dll)))
    (list* :whole (second dll)
           (parse-reqvars (rest (rest dll))))))


(defun take-until (list test)
  (do ((items '() (list* (first list) items))
       (list list (rest list)))
      ((funcall test list)
       (values (nreverse items) list))))

(defun section-boundary-p (list)
  (or (atom list)
      (member list +standard-lambda-list-keywords+)))

(defun parse-reqvars (dll)
  (parse-list (dll parse-optvars)
    (multiple-value-bind (reqvars tail)
        (take-until #'section-boundary-p dll)
      (list* :reqvars reqvars
             (parse-optvars tail)))))

(defun parse-optvars (dll)
  (parse-list (dll parse-restvars (eq (first dll) '&optional))
    (multiple-value-bind (optvars tail) 
        (take-until #'section-boundary-p dll)
      (list* :optvars optvars 
             (parse-restvars tail)))))

(defun parse-restvars (dll)
  (cond
    ((null dll) '())
    ((atom dll) (list :rest dll))
    ((member (first dll) '(&body &rest))
     (list* :rest (second dll)
            (parse-keyvars (rest (rest dll)))))
    ((parse-keyvars dll))))

(defun parse-keyvars (dll)
  (cond
    ((endp dll) '())
    ((eq (first dll) '&key)
     (multiple-value-bind (keyvars tail)
         (take-until #'section-boundary-p dll)
       (list* :keyvars keyvars
              (parse-auxvars tail))))
    ((parse-auxvars dll))))

(defun parse-auxvars (dll)
  (cond 
    ((endp dll) '())
    ((eq (first dll) '&aux)
     (multiple-value-bind (auxvars tail)
         (take-until #'section-boundary-p dll)
       (list* :auxvars auxvars
              (parse-empty tail))))
    ((parse-empty dll))))

(defun parse-empty (dll)
  (if (endp dll) '()
      (error "Malformed destructing lambda list ~a." dll)))

;; (parse-destructuring-lambda-list '(a b c &rest d &aux a))
