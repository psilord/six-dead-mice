(in-package #:lang-e)

;; Language as defined in section 3.2.2 in LiSP, ported to CL

(defparameter *empty-begin-value* NIL)
(defparameter *no-more-arguments* NIL)

;; The object from whence all things come.
(defclass object () ())

;;;;
;;;; Continuations
;;;;

;; Base Continuation Object
(defclass continuation (object)
  ((%k :initarg :k
       :initform nil
       :accessor k)))

(defun make-continuation (k)
  (make-instance 'continuation :k k))

(defun continuation-p (o)
  (subtypep 'continuation (type-of o)))

;; Alternative Continuation Object
(defclass if-continuation (continuation)
  ((%et :initarg :et
        :accessor et)
   (%ef :initarg :ef
        :accessor ef)
   (%r :initarg :r
       :accessor r)))

(defun make-if-continuation (k et ef r)
  (make-instance 'if-continuation :k k :et et :ef ef :r r))

(defun if-continuation-p (o)
  (subtypep 'if-continuation (type-of o)))

;; Sequence Continuation (begin)
(defclass begin-continuation (continuation)
  ((%e* :initarg :e*
        :accessor e*)
   (%r :initarg :r
       :accessor r)))

(defun make-begin-continuation (k e* r)
  (make-instance 'begin-continuation :k k :e* e* :r r))

(defun begin-continuation-p (o)
  (subtypep 'begin-continuation (type-of o)))

;; set! Continuation (set!)
(defclass set!-continuation (continuation)
  ((%n :initarg :n
       :accessor n)
   (%r :initarg :r
       :accessor r)))

(defun make-set!-continuation (k n r)
  (make-instance 'set!-continuation :k k :n n :r r))

(defun set!-continuation-p (o)
  (subtypep 'set!-continuation (type-of o)))

;; Function position evaluation Continuation
(defclass evfun-continuation (continuation)
  ((%e* :initarg :e*
        :accessor e*)
   (%r :initarg :r
       :accessor r)))

(defun make-evfun-continuation (k e* r)
  (make-instance 'evfun-continuation :k k :e* e* :r r))

(defun evfun-continuation-p (o)
  (subtypep 'evfun-continuation (type-of o)))

;; Function Application Continuation
(defclass apply-continuation (continuation)
  ((%f :initarg :f
       :accessor f)
   (%r :initarg :r
       :accessor r)))

(defun make-apply-continuation (k f r)
  (make-instance 'apply-continuation :k k :f f :r r))

(defun apply-continuation-p (o)
  (subtypep 'apply-continuation (type-of o)))

;; Function Argument Evaluation Continuation
(defclass argument-continuation (continuation)
  ((%e* :initarg :e*
        :accessor e*)
   (%r :initarg :r
       :accessor r)))

(defun make-argument-continuation (k e* r)
  (make-instance 'argument-continuation :k k :e* e* :r r))

(defun argument-continuation-p (o)
  (subtypep 'argument-continuation (type-of o)))

;; Gather Evaluated Arguments Continuation
(defclass gather-continuation (continuation)
  ((%v :initarg :v
       :accessor v)))

(defun make-gather-continuation (k v)
  (make-instance 'gather-continuation :k k :v v))

(defun gather-continuation-p (o)
  (subtypep 'gather-continuation (type-of o)))


;; Catch continuation
(defclass catch-continuation (continuation)
  ((%body :initarg :body
          :accessor body)
   (%r :initarg :r
       :accessor r)))

(defun make-catch-continuation (k body r)
  (make-instance 'catch-continuation :k k :body body :r r))

(defun catch-continuation-p (o)
  (subtypep 'catch-continuation (type-of o)))

;; Labled continuation
(defclass labeled-continuation (continuation)
  ((%tag :initarg :tag
         :accessor tag)))

(defun make-labeled-continuation (k tag)
  (make-instance 'labeled-continuation :k k :tag tag))

(defun labeled-continuation-p (o)
  (subtypep 'labeled-continuation (type-of o)))

;; Throw continuation
(defclass throw-continuation (continuation)
  ((%form :initarg :form
          :accessor form)
   (%r :initarg :r
       :accessor r)))

(defun make-throw-continuation (k form r)
  (make-instance 'throw-continuation :k k :form form :r r))

(defun throw-continuation-p (o)
  (subtypep 'throw-continuation (type-of o)))

;; Throwing continuation
(defclass throwing-continuation (continuation)
  ((%tag :initarg :tag
         :accessor tag)
   (%cont :initarg :cont
          :accessor cont)))

(defun make-throwing-continuation (k tag cont)
  (make-instance 'throwing-continuation :k k :tag tag :cont cont))

(defun throwing-continuation-p (o)
  (subtypep 'throwing-continuation (type-of o)))

;; Block continuation
(defclass block-continuation (continuation)
  ((%label :initarg :label
           :accessor label)))

(defun make-block-continuation (k label)
  (make-instance 'block-continuation :k k :label label))

(defun block-continuation-p (o)
  (subtypep 'block-continuation (type-of o)))

;; Return-from continuation
(defclass return-from-continuation (continuation)
  ((%r :initarg :r
       :accessor r)
   (%label :initarg :label
           :accessor label)))

(defun make-return-from-continuation (k r label)
  (make-instance 'return-from-continuation :k k :r r :label label))

(defun return-from-continuation-p (o)
  (subtypep 'return-from-continuation (type-of o)))

;; Unwind-protect continuation
(defclass unwind-protect-continuation (continuation)
  ((%cleanup :initarg :cleanup
             :accessor cleanup)
   (%r :initarg :r
       :accessor r)))

(defun make-unwind-protect-continuation (k cleanup r)
  (make-instance 'unwind-protect-continuation :k k :cleanup cleanup :r r))

(defun unwind-protect-continuation-p (o)
  (subtypep 'unwind-protect-continuation (type-of o)))

;; Protect-Return continuation (second half of unwind-protect)
(defclass protect-return-continuation (continuation)
  ((%value :initarg :value
           :accessor value)))

(defun make-protect-return-continuation (k value)
  (make-instance 'protect-return-continuation :k k :value value))

(defun protect-return-continuation-p (o)
  (subtypep 'protect-return-continuation (type-of o)))


;; Unwind continuation
(defclass unwind-continuation (continuation)
  ((%value :initarg :value
           :accessor value)
   (%target :initarg :target
            :accessor target)))

(defun make-unwind-continuation (k value target)
  (make-instance 'unwind-continuation :k k :value value :target target))

(defun unwind-continuation-p (o)
  (subtypep 'unwind-continuation (type-of o)))



;; Bottom Continuation (passed to the evaluator)
(defclass bottom-continuation (continuation)
  ((%f :initarg :f
       :accessor f)))

(defun make-bottom-continuation (k f)
  (make-instance 'bottom-continuation :k k :f f))

(defun bottom-continuation-p (o)
  (subtypep 'bottom-continuation (type-of o)))

;;;;
;;;; Environments
;;;;

;; The concept of an environment (base class)
(defclass environment (object) ())

(defun make-environment ()
  (make-instance 'environment))

(defun environment-p (o)
  (subtypep 'environment (type-of o)))

;; The Null Environment
(defclass null-env (environment) ())

(defun make-null-env ()
  (make-instance 'null-env))

(defun null-env-p (o)
  (subtypep 'null-env (type-of o)))

;; The Full Environment
(defclass full-env (environment)
  ((%others :initarg :others
            :accessor others)
   (%name :initarg :name
          :accessor name)))

(defun make-full-env (others name)
  (make-instance 'full-env) :others others :name name)

(defun full-env-p (o)
  (subtypep 'full-env (type-of o)))

;; The Variable Environment
(defclass variable-env (full-env)
  ((%value :initarg :value
           :accessor value)))

(defun make-variable-env (others name value)
  (make-instance 'variable-env :others others :name name :value value))

(defun variable-env-p (o)
  (subtypep 'variable-env (type-of o)))


;; A Block Environment
(defclass block-env (full-env)
  ((%cont :initarg :cont
          :accessor cont)))

(defun make-block-env (others name cont)
  (make-instance 'block-env :others others :name name :cont cont))

(defun block-env-p (o)
  (subtypep 'block-env (type-of o)))



;; The concept of a value (base class)
(defclass value (object) ())

;; A Function Value (can't use symbol 'function' due to CL package locks).
(defclass func (value)
  ((%variables :initarg :variables
               :accessor variables)
   (%body :initarg :body
          :accessor body)
   ;; This next slot forces closure over (the whole environment) in
   ;; this function.
   (%env :initarg :env
         :accessor env)))

(defun make-func (variables body env)
  (make-instance 'func :variables variables :body body :env env))

(defun func-p (o)
  (subtypep 'func (type-of o)))

;; A Primitive Value
(defclass primitive (value)
  ((%name :initarg :name
          :accessor name)
   (%address :initarg :address
             :accessor address)))

(defun make-primitive (name address)
  (make-instance 'primitive :name name :address address))

(defun primitive-p (o)
  (subtypep 'primitive (type-of o)))



;; Generic functions
(defgeneric wrong (msg &rest args))
(defgeneric invoke (f v* r k))
(defgeneric resume (k v))
(defgeneric lookup (r n k))
(defgeneric update! (r n k v))
(defgeneric catch-lookup (k tag kk))
(defgeneric block-lookup (r n k v))
(defgeneric unwind (k v ktarget))
