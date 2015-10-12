(in-package #:lang-e)

;; Language as defined in section 3.2.2 in LiSP, ported to CL

(defmethod wrong (msg &rest args)
  (error (apply #'format t (concatenate 'string msg "~&") args)))

;; Most generic types.
(defmethod invoke (f v* r k)
  (wrong "Not a function: f = ~A, r = ~A, k = ~A" f r k))

(defmethod invoke ((f continuation) v* r k)
  ;; This is here to allow call/cc to function.
  (if (= 1 (length v*))
      (resume f (car v*))
      (wrong "Not a function: f = ~A, r = ~A, k = ~A" f r k)))

(defmethod resume ((k continuation) v)
  (wrong "Unknown continuation: v = ~A k = ~A" v k))

(defmethod lookup ((r environment) n k)
  (wrong "Not an environment (in which to lookup): r = ~A, n = ~A, k = ~A"
         r n k))

(defmethod update! ((r environment) n k v)
  (wrong "Not an environment (in which to update!): r = ~A, n = ~A, v = ~A, k = ~A" r n v k))

(defmethod catch-lookup (k tag kk)
  (wrong "Not a catch continuation: k = ~A, tag = ~A, kk = ~A" k tag kk))

(defmethod block-lookup (r n k v)
  (wrong "not a block environment: r = ~A, n = ~A, k = ~A, v = ~A" r n k v))


;; The interpreter
(defun evaluate (e r k)
  (cond
    ((or (eq e nil) (eq e t))
     ;; hard code these symbol's to be CL's true or false, which happens
     ;; when we pass them on.
     (evaluate-quote e r k))
    ((atom e)
     (if (symbolp e)
         (evaluate-variable e r k)
         ;; autoquote atoms I don't understand.
         (evaluate-quote e r k)))
    ((eq (car e) 'quote)
     (evaluate-quote (cadr e) r k))
    ((eq (car e) 'if)
     (evaluate-if (cadr e) (caddr e) (cadddr e) r k))
    ((eq (car e) 'begin)
     (evaluate-begin (cdr e) r k))
    ((eq (car e) 'set!)
     (evaluate-set! (cadr e) (caddr e) r k))
    ((eq (car e) 'lambda)
     (evaluate-lambda (cadr e) (cddr e) r k))
    ;; Implement catch/throw ala Common Lisp
    ((eq (car e) 'catch)
     (evaluate-catch (cadr e) (cddr e) r k))
    ((eq (car e) 'throw)
     (evaluate-throw (cadr e) (caddr e) r k))
    ;; Implement block/return-from ala Common Lisp
    ((eq (car e) 'block)
     (evaluate-block (cadr e) (cddr e) r k))
    ((eq (car e) 'return-from)
     (evaluate-return-from (cadr e) (caddr e) r k))
    (t
     ;; everything else is a function application
     (evaluate-application (car e) (cdr e) r k))))




;; Quoting
(defun evaluate-quote (v r k)
  ;; Just pass the literal form as the value to the continuation.
  (declare (ignore r))
  (resume k v))

;; Alternatives
(defun evaluate-if (ec et ef r k)
  (evaluate ec r (make-if-continuation k et ef r)))

(defmethod resume ((k if-continuation) v)
  ;; If the value is true, we evaluate the true/false espressions
  (evaluate (if v (et k) (ef k))
            (r k)
            (k k)))

;; Sequences
(defun evaluate-begin (e* r k)
  (if (consp e*)
      (if (consp (cdr e*))
          (evaluate (car e*) r (make-begin-continuation k e* r)) ;; cdr in resu.
          (evaluate (car e*) r k))
      (resume k *empty-begin-value*)))

(defmethod resume ((k begin-continuation) v)
  (evaluate-begin (cdr (e* k))
                  (r k)
                  (k k)))

;; Variables Environment
(defun evaluate-variable (n r k)
  (lookup r n k))

;; Getting a variable's value
(defmethod lookup ((r null-env) n k)
  (wrong "Unknown variable: n = ~A, r = ~A, k = ~A" n r k))

(defmethod lookup ((r full-env) n k)
  (lookup (others r) n k))

(defmethod lookup ((r variable-env) n k)
  (if (eql n (name r))
      (resume k (value r))
      (lookup (others r) n k)))

;; Setting a variable's value
(defun evaluate-set! (n e r k)
  ;; We first evaluate the value, then we shove that value to another
  ;; continuation that assigns it.
  (evaluate e r (make-set!-continuation k n r)))

(defmethod resume ((k set!-continuation) v)
  (update! (r k) (n k) (k k) v))

(defmethod update! ((r null-env) n k v)
  (wrong "Unknown variable to update!: r = ~A, n = ~A, k = ~A, v = ~A" r n k v))

(defmethod update! ((r full-env) n k v)
  (update! (others r) n k v))

(defmethod update! ((r variable-env) n k v)
  (if (eql n (name r))
      (progn
        (setf (value r) v)
        (resume k v))
      (update! (others r) n k v)))

;; Functions
;; Creating a function.
(defun evaluate-lambda (n* e* r k)
  (resume k (make-func n* e* r)))

;; Invoking a function.
(defmethod invoke ((f func) v* r k)
  (declare (ignorable r))
  (let ((env (extend-env (env f) (variables f) v*)))
    (evaluate-begin (body f) env k)))

;; Invoking a primitive function

(defun extend-env (env n* v*)
  (cond
    ((and (consp n*) (consp v*))
     (make-variable-env
      (extend-env env (cdr n*) (cdr v*))
      (car n*)
      (car v*)))
    ((and (null n*) (null v*))
     ;; Link to previous environment of arbitrary size.
     env)
    (t
     (wrong "Arity Mismatch: env = ~A, n* = ~A, v* = ~A" env n* v*))))

;; Applying a function (to a list of evaluated arguments)

(defun evaluate-application (e e* r k)
  (evaluate e r (make-evfun-continuation k e* r)))

(defmethod resume ((k evfun-continuation) f)
  (evaluate-arguments (e* k) (r k) (make-apply-continuation (k k) f (r k))))

(defun evaluate-arguments (e* r k)
  (if (consp e*)
      (evaluate (car e*) r (make-argument-continuation k e* r))
      (resume k *no-more-arguments*)))

(defmethod resume ((k argument-continuation) v)
  (evaluate-arguments (cdr (e* k)) (r k) (make-gather-continuation (k k) v)))

(defmethod resume ((k gather-continuation) v*)
  (resume (k k) (cons (v k) v*)))

(defmethod resume ((k apply-continuation) v)
  (invoke (f k) v (r k) (k k)))

;; Invoke primitives
(defmethod invoke ((f primitive) v* r k)
  ;; The address here is the actual function from the underlying lisp system.
  (funcall (address f) v* r k))

;; Catch methods
(defmethod evaluate-catch (tag body r k)
  (evaluate tag r (make-catch-continuation k body r)))

(defmethod resume ((k catch-continuation) v)
  (evaluate-begin (body k) (r k) (make-labeled-continuation (k k) v)))

;; NOTE: This was not in the LiSP book, I had to add it.
(defmethod resume ((k labeled-continuation) v)
  (resume (k k) v))

;; Throw methods
(defmethod evaluate-throw (tag form r k)
  (evaluate tag r (make-throw-continuation k form r)))

(defmethod resume ((k throw-continuation) tag)
  (catch-lookup k tag k))

(defmethod catch-lookup ((k continuation) tag kk)
  (format t "catch-lookup ~A~%" k)
  (catch-lookup (k k) tag kk))

(defmethod catch-lookup ((k bottom-continuation) tag kk)
  (wrong "No associated catch! k = ~A, tag = ~A, kk = ~A" k tag kk))

(defmethod catch-lookup ((k labeled-continuation) tag kk)
  (format t "catch-lookup (eql ~A ~A)~%" tag (tag k))
  (if (eql tag (tag k))
      (evaluate (form kk) (r kk) (make-throwing-continuation kk tag k))
      (catch-lookup (k k) tag kk)))

(defmethod resume ((k throwing-continuation) v)
  (dump-cont k) ;; debugging.
  (resume (cont k) v))


;; Block methods
(defun evaluate-block (label body r k)
  (let ((k (make-block-continuation k label)))
    (evaluate-begin body (make-block-env r label k) k)))

(defmethod resume ((k block-continuation) v)
  (resume (k k) v))

;; Return-from methods
(defun evaluate-return-from (label form r k)
  (evaluate form r (make-return-from-continuation k r label)))

(defmethod resume ((k return-from-continuation) v)
  (block-lookup (r k) (label k) (k k) v))

(defmethod block-lookup ((r block-env) n k v)
  (if (eql n (name r))
      (unwind k v (cont r))
      (block-lookup (others r) n k v)))

(defmethod block-lookup ((r full-env) n k v)
  (block-lookup (others r) n k v))

(defmethod block-lookup ((r null-env) n k v)
  (wrong "Unknown block label: r = ~A, n = ~A, k = ~A, v = ~A" r n k v))

(defmethod resume ((k return-from-continuation) v)
  (block-lookup (r k) (label k) (k k) v))

(defmethod unwind ((k continuation) v ktarget)
  (if (eql k ktarget)
      (resume k v)
      (unwind (k k) v ktarget)))

(defmethod unwind ((k bottom-continuation) v ktarget)
  (wrong "Onselete continuation: k = ~A, v = ~A, ktarget = ~A" k v ktarget))



;; Bottom Continuation methods
(defmethod resume ((k bottom-continuation) v)
  ;; Simply pass the value to the enclosed function, and return.
  (funcall (f k) v))



;; Debugging junk TODO: use this to inspect the continuation
;; structures and how they are linked together during the
;; evaluation. (Hint: I can search through the continuations in order
;; to find a particular continuation for which I may seek.
(defmethod dump-cont (k)
  (format t "dump-cont[DONE]: k = ~A~%" k))

(defmethod dump-cont ((k continuation))
  (format t "dump-cont: k = ~A, k' = ~A~%" k (k k))
  (dump-cont (k k)))


;; Interpreter
(defun dump-env (msg r)
  (labels ((dump-it (r)
             (cond
               ((variable-env-p r)
                (format t "name: ~A, value: ~A~%" (name r) (value r))
                (dump-it (others r)))
               ((null-env-p r)
                (format t "End of environment.~%")))))
    (format t "~A~%" msg)
    (dump-it r)))

;; Return a contination which accepts any single value and prints it
;; out to stdout.
(defun the-bottom ()
  (make-bottom-continuation
   'void
   (lambda (v)
     (format t "Bottom continuation got: ~A~%" v))))

(defun test (form)
  (flet ((gen-primitive-function (f a)
           (lambda (v* r k)
             (declare (ignorable r))
             (cond
               ((eq a 'variable-arity)
                (resume k (apply f v*)))
               ((= a (length v*))
                (resume k (apply f v*)))
               (t
                (wrong "Func: ~A wants ~A args, but given ~A args."
                       f a (length v*)))))))

    (evaluate

     form

     ;; a simple environment for testing
     (extend-env
      (make-null-env)
      (list 'cons 'car 'cdr '+ 'call/cc)
      (list (make-primitive 'cons (gen-primitive-function #'cons 2))
            (make-primitive 'car (gen-primitive-function #'car 1))
            (make-primitive 'cdr (gen-primitive-function #'cdr 1))
            (make-primitive '+ (gen-primitive-function #'+ 'variable-arity))
            (make-primitive
             'call/cc
             (lambda (v* r k)
               (if (= 1 (length v*))
                   (invoke (car v*) (list k) r k)
                   (wrong "call/cc: incorrect arity. v* = ~A, r = ~A, k = ~A"
                          v* r k))))))

     (the-bottom))))
