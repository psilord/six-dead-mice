(in-package :six-dead-mice)

;;;; This is another prototype of a compiler for a dialect called literal-lisp
;;;; which is heavily C-like in its nature. So far, this only describes a
;;;; translation of it to three address code.


(defun inbase (num &optional (base :d))
  (ecase base
    (:d
     (format t "~10R~%" num))
    (:h
     (format t "#x~16R~%" num))
    (:o
     (format t "#o~3R~%" num))
    (:b
     (format t "#b~2R~%" num))))

;; expressions
(defun op (form)
  (first form))
(defun arg1 (form)
  (second form))
(defun arg2 (form)
  (third form))
(defun arg3+ (form)
  (cdddr form))

;; labels
(defun label (form)
  (second form))

;; if constructs
(defun if-cond (form)
  (second form))
(defun if-then (form)
  (third form))
(defun if-else (form)
  (fourth form))

;; <- copy
(defun dest (ast)
  (second ast))
(defun source (ast)
  (third ast))

;; function definitions
(defun func-name (ast)
  (second ast))
(defun func-params (ast)
  (third ast))
(defun func-body (ast)
  (cdddr ast))

;; function invocations
(defun iname (ast)
  (first ast))
(defun args (ast)
  (cdr ast))

;; while form (while cond result-form &body ...)
(defun while-cond (ast)
  (first (second ast)))
(defun while-result (ast)
  (second (second ast)))
(defun while-body (ast)
  (cddr ast))

;; setf form
(defun setf-place (ast)
  (second ast))
(defun setf-newval (ast)
  (third ast))

(defparameter *inst* nil)
(defun ast->3ac (ast)
  (if (atom ast)
      (cond
        ((integerp ast) ;; literal number
         ast)
        ((symbolp ast);; a symbol name
         ast)
        (t
         (error "oops, don't know how to handle this: ~A" ast)))

      (case (first ast)
        ('progn ;; sequencing operator, last expr is value.
          ;; return the label to the last thing processed.
          (let ((last-one nil))
            (loop for f in ast do (setf last-one (ast->3ac f)))
            last-one))

        ((+ * - / < > <= >= = /=) ;; left associative simple operators
         (if (> (length ast) 3)
             ;; reduce the left associative operator across the list.
             (ast->3ac
              `(,(op ast)
                 ,(ast->3ac (list (op ast) (arg1 ast) (arg2 ast)))
                 ,@(arg3+ ast)))

             (let ((name (gensym "T")))
               ;; the base primitive 2 argument operator form (op and 2 nums)
               (cond
                 ((= (length ast) 3)
                  (push `(,name = ,(ast->3ac (second ast))
                                ,(first ast)
                                ,(ast->3ac (third ast)))
                        *inst*)
                  name)
                 ;; handle uanary case (the op and the number)
                 ((= (length ast) 2)
                  (case (op ast)
                    (+
                     (push `(,name = 0 + ,(ast->3ac (arg1 ast))) *inst*))
                    (-
                     (push `(,name = 0 - ,(ast->3ac (arg1 ast))) *inst*))
                    (*
                     (push `(,name = 1 * ,(ast->3ac (arg1 ast))) *inst*))
                    (/
                     (push `(,name = 1 / ,(ast->3ac (arg1 ast))) *inst*))
                    ((< > <= >= = /=)
                     ;; transform, but ignore, whatever value is
                     ;; there, and return T
                     (push `(,(gensym "T") = ,(ast->3ac (arg1 ast))) *inst*)
                     (push `(,name = T) *inst*)
                     name)))
                 (t
                  (error "figure out what is wrong here in binop."))))))


        (l ;; label
         (push `(label ,(label ast)) *inst*))

        (goto ;; goto
         (push `(goto ,(label ast)) *inst*))


        (<- ;; copy from one variable to another
         ;; this is not a pointer assignment or reference, it is just a copy
         (push `(,(dest ast) = ,(source ast)) *inst*))

        (if ;; (if cond-expr then &optional else)
         (let* ((id (symbol-name (gensym "ID")))
                (if-start (gensym (concatenate 'string "IF_" id "_")))
                (else-label (gensym (concatenate 'string "ELSE_" id "_")))
                (endif-label (gensym (concatenate 'string "ENDIF_" id "_")))
                (result-name (gensym "T")))

           (ast->3ac `(l ,if-start))

           (let ((cid (ast->3ac (if-cond ast))))
             (push `(if (= ,cid NIL) (goto ,else-label)) *inst*)
             (let ((tid (ast->3ac (if-then ast))))
               (ast->3ac `(<- ,result-name ,tid))
               (ast->3ac `(goto ,endif-label))
               (ast->3ac `(l ,else-label))
               (when (> (length ast) 3)
                 (let ((eid (ast->3ac (if-else ast))))
                   ;; and now copy the contructed value to the known return
                   ;; variable
                   (ast->3ac `(setf ,result-name ,eid))))
               (ast->3ac `(l ,endif-label))))
           result-name))

        (defun ;; define a non-closure function
            (ast->3ac `(l ,(func-name ast)))
            (push `(begin-func ,(length (func-params ast))) *inst*)
          (let ((result-var (apply #'ast->3ac (func-body ast))))
            (push `(return ,result-var) *inst*)
            (push `(end-func) *inst*)))

        (while ;; unlisp-like, but easier to implement than DO fo rnow.
            (let* ((id (symbol-name (gensym "ID")))
                   (while-start (gensym (concatenate 'string "WHILE_" id "_")))
                   (while-end (gensym (concatenate 'string "ENDWHILE_" id "_")))
                   (while-result (gensym "T")))
              (ast->3ac `(l ,while-start))
              (let ((cid (ast->3ac (while-cond ast))))
                (push `(if (= ,cid NIL) (goto ,while-end)) *inst*)
                ;; we don't save the result of the body anywhere.
                (apply #'ast->3ac (while-body ast))
                (ast->3ac `(goto ,while-start))
                ;; now emit the value to compute when the while is done.
                (ast->3ac `(l ,while-end))
                (push `(,while-result = ,(ast->3ac (while-result ast))) *inst*)
                while-result)))

        (setf
         (if (symbolp (setf-place ast))
             (ast->3ac `(<- ,(setf-place ast) ,(ast->3ac (setf-newval ast))))
             (error "implement setf for form places!")))

        (aref
         ;; implement me
         nil)

        (t
         ;; Everything else is function application.

         ;; Compute the arguments:
         (let ((argvars (mapcar #'ast->3ac (args ast)))
               (retvar (gensym "T")))
           (loop for i in argvars do
                (push `(push-param ,i) *inst*))
           (push `(,retvar = call ,(iname ast) ,(length (args ast))) *inst*)
           (unless (length (args ast))
             (push `(pop-params ,(length (args ast))) *inst*))
           retvar))






        )))

;; http://www.inf.unibz.it/~artale/Compiler/slide10-InterCodeGen.pdf
;; http://web.stanford.edu/class/archive/cs/cs143/cs143.1128/handouts/240%20TAC%20Examples.pdf
;; http://web.stanford.edu/class/archive/cs/cs143/cs143.1128/lectures/13/Slides13.pdf

(defun generate-intermediate-code (ast)
  (format t ";; New instruction stream for:~%~(~A~)~%~%" ast)
  (setf *inst* nil)
  (let ((name (ast->3ac ast)))
    (setf *inst* (nreverse *inst*))
    name))

(defun emit-intermediate-code (forms)
  (let ((*print-right-margin* (* 1024 1024))
        ;;(*print-miser-width* (* 1024 1024))
        (*print-length* (* 1024 1024))
        (*print-level* nil)
        (*print-circle* t)
        (*print-lines* nil))

    (format t "  End var: ~(~A~)~%~%" (generate-intermediate-code forms))
    (loop for i in *inst* do
         (if (eq (car i) 'label)
             (format t "~(~A~)~%" i)
             (format t "  ~(~A~)~%" i)))
    (format t "~%")))

(defun test ()
  (emit-intermediate-code '(+ 1 2 3 4 5))
  (emit-intermediate-code '(* 1 2 3 4 5))
  (emit-intermediate-code '(- 1 2 3 4 5))
  (emit-intermediate-code '(/ 1 2 3 4 5))

  (emit-intermediate-code
   '(+ (if (< (+ 1 2 3) (+ 4 5 6) (+ 7 8 9))
           (* 1 2 3 4 5)
           (- 1 2 3 4 5))
     100))

  (emit-intermediate-code
   '(while ((< x a) (+ x 10))
     (+ 1 2 3)))

  (emit-intermediate-code
   '(progn
     (defun foo (a b)
       (if (< a b) (* a 100) (/ b 200)))
     (defun bar (c d e)
       (* (+ c e) (- d e)))
     (defun main ()
       (+ (foo (+ 10 20)) (bar (- 30 10)) 512))

     (l toplevel-start)
     (main))))





;; right associative traversal:
;; (ast->3ac (list (first ast) (second ast) (list* (first ast) (cddr ast))))
