(in-package :lang-d)

;;;; This is another prototype of a compiler for a dialect called literal-lisp
;;;; which is heavily C-like in its nature. So far, this only describes a
;;;; translation of it to three address code. It is similar to prescheme.
;;;; It is flawed because I don't deal with the environment carefully enough.

(defparameter *block-hack* nil)

(defparameter *inst* nil)

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

(defmacro define-form-type-check (name &rest syms)
  (let ((form (gensym))
        (thing (gensym)))
    `(defun ,name (,form)
       (flet ((op-check (,thing)
                (case ,thing
                  ((,@syms)
                   t)
                  (otherwise nil))))
         (cond
           ((consp ,form)
            (op-check (car ,form)))
           (t nil))))))

;; function applications of primitive functions. (+ 1 2 3) etc.
(define-form-type-check binop-p + - * /)
(define-form-type-check relop-p < > <= >= /= =)
(defun op (form)
  (first form))
(defun op-args (form)
  (cdr form))
(defun op-arg1 (form)
  (second form))
(defun op-arg2 (form)
  (third form))
(defun op-arg3+ (form)
  (cdddr form))

;; (progn 1 2 3 4)
(define-form-type-check progn-p progn)
(defun progn-body (form)
  (cdr form))

;; (l foo)
(define-form-type-check label-p l)
(defun label (form)
  (second form))

;; (goto label)
(define-form-type-check goto-p goto)
(defun goto-label (form)
  (second form))

;; (if cond-expr then <optional-else>)
(define-form-type-check if-p if)
(defun if-cond (form)
  (second form))
(defun if-then (form)
  (third form))
(defun if-else (form)
  (fourth form))

;; (<- dest source) [copy variable]
(define-form-type-check copy-p <-)
(defun dest (form)
  (second form))
(defun source (form)
  (third form))

;; function definition: (defun foo (a b c) (+ 1 2 3))
(define-form-type-check defun-p defun)
(defun func-name (form)
  (second form))
(defun func-params (form)
  (third form))
(defun func-body (form)
  (cdddr form))

;; function invocations: (foo 1 2 3)
(defun iname (form)
  (first form))
(defun args (form)
  (cdr form))

;; while form (while cond result-form &body ...)
(define-form-type-check while-p while)
(defun while-cond (form)
  (first (second form)))
(defun while-result (form)
  (second (second form)))
(defun while-body (form)
  (cddr form))

;; setf form: (setf x 42)
(define-form-type-check setf-p setf)
(defun setf-place (form)
  (second form))
(defun setf-newval (form)
  (third form))

;; aref form: (aref x &rest indexes)
(define-form-type-check aref-p aref)
;; TODO

;; block form: (block foo 1 2 3 4)
(define-form-type-check block-p block)
(defun block-name (form)
  (second form))
(defun block-body (form)
  (cddr form))

;; return-from form: (block foo 1 2 3 (return-from foo 42) 4 5 6)
(define-form-type-check return-from-p return-from)
(defun return-from-name (form)
  (second form))
(defun return-from-value (form)
  (third form))

(defun emit3ac (form)
  (push form *inst*))

(defun form->3ac (form)
  "This may cause TAC instructions to be emitted into *inst*. It returns
a values of either: variable assigned and T, or nil and nil."
  (cond
    ((integerp form) ;; literal number
     (values form t))
    ((symbolp form) ;; a symbol name
     (values form t))
    ((progn-p form) ;; sequencing operator, lform expr is value.
     ;; return the label to the lform thing processed.
     (let ((lform-one nil))
       (loop for f in (progn-body form) do
            (let ((var (gensym "T")))
              (multiple-value-bind (retvar existsp) (form->3ac f)
                (when existsp
                  (emit3ac `(,var = ,retvar))
                  ;; store the lform var we've seen, this is the result of
                  ;; the progn form.
                  (setf lform-one var)))))

       (if lform-one
           (values lform-one t)
           (values nil nil))))

    ;; These are processed differently than the artihmetic operators.
    ;; TODO: I may have to synthesize <= >= /= from < > =... currently
    ;; I don't.
    ((relop-p form)
     ;; We DO short circuit the relation operators! And we are careful not
     ;; to perform multiple evaluations and do left to right execution
     ;; when there are appropriate expressions to compute.
     (cond
       ;; 3 or more arguments to the relop, perform a manual
       ;; reduction (as opposed to recursion) including short
       ;; circuiting.
       ((> (length form) 3)
        (let* ((id (symbol-name (gensym "ID")))
               (rel-start (gensym (concatenate 'string "RELSTART_" id "_")))
               (rel-done (gensym (concatenate 'string "RELDONE_" id "_")))
               (and-var (gensym "T"))
               v1)
          ;; slide across the arguments, computing the value of each one
          ;; once. Then if the relational operator fails, jmp to the end
          ;; of the relational expression.
          (form->3ac `(l ,rel-start)) ;; for notation.
          (setf v1 (form->3ac (op-arg1 form)))
          (loop for i in (cdr (op-args form))
             for c from 1 by 1 ;; we already considered one argument...
             with l = (length (op-args form)) do
               (let ((v2 (form->3ac i)))
                 (emit3ac `(,and-var = ,v1 ,(op form) ,v2))
                 ;; don't emit the lform short circuit jmp which would just
                 ;; go to the next instruction.
                 (unless (= c (1- l))
                   (emit3ac `(if (= ,and-var nil) (goto ,rel-done))))
                 (setf v1 v2)))
          (emit3ac `(l ,rel-done))
          (values and-var t)))

       ;; exactly 2 args to the relop.
       ((= (length form) 3)
        (let ((relvar (gensym "T")))
          (emit3ac `(,relvar = ,(form->3ac (op-arg1 form))
                             ,(op form)
                             ,(form->3ac (op-arg2 form))))
          (values relvar t)))

       ;; exactly 1 arg to the relop (always true, but compute the result).
       ((= (length form) 2)
        (let ((result (gensym "T"))
              (relvar (gensym "T")))
          (emit3ac `(,result = ,(form->3ac (op-arg1 form))))
          (emit3ac `(,relvar = t))
          (values relvar t)))

       ;; a useless error message.
       (t
        (error "Relop 3ac code processing gone wrong!"))))


    ((binop-p form) ;; left associative simple operators
     (if (> (length form) 3)
         ;; reduce the left associative operator across the list.
         (form->3ac
          `(,(op form)
             ,(form->3ac (list (op form) (op-arg1 form) (op-arg2 form)))
             ,@(op-arg3+ form)))

         (let ((name (gensym "T")))
           ;; the base primitive 2 argument operator form (op and 2 nums)
           (cond
             ((= (length form) 3)
              (emit3ac `(,name = ,(form->3ac (second form))
                               ,(first form)
                               ,(form->3ac (third form))))
              (values name t))
             ;; handle unary case (the op and the number)
             ((= (length form) 2)
              (case (op form)
                ((+ -)
                 (emit3ac `(,name = 0 ,(op form) ,(form->3ac (op-arg1 form)))))
                ((* /)
                 (emit3ac `(,name = 1 ,(op form) ,(form->3ac (op-arg1 form)))))
                ((< > <= >= = /=)
                 ;; transform, but ignore, whatever value is
                 ;; there, and return T
                 (emit3ac `(,(gensym "T") = ,(form->3ac (op-arg1 form))))))
              ;; and return the name we used for this expression.
              (values name t))

             (t
              (error "figure out what is wrong here in binop."))))))


    ((label-p form) ;; label
     (emit3ac `(label ,(label form)))
     (values nil nil))


    ((goto-p form) ;; goto
     (emit3ac `(goto ,(goto-label form)))
     (values nil nil))


    ((copy-p form) ;; copy from one variable to another
     ;; this is not a pointer assignment or reference, it is just a copy
     (emit3ac `(,(dest form) = ,(source form)))
     (values (dest form) t))

    ((if-p form) ;; (if cond-expr then &optional else)
     (let* ((id (symbol-name (gensym "ID")))
            (if-start (gensym (concatenate 'string "IF_" id "_")))
            (else-label (gensym (concatenate 'string "ELSE_" id "_")))
            (endif-label (gensym (concatenate 'string "ENDIF_" id "_")))
            (result-var (gensym "T")))

       (form->3ac `(l ,if-start))

       (let ((cid (form->3ac (if-cond form))))
         (emit3ac `(if (= ,cid NIL) (goto ,else-label)))
         (let ((tid (form->3ac (if-then form))))
           (form->3ac `(<- ,result-var ,tid))
           (form->3ac `(goto ,endif-label))
           (form->3ac `(l ,else-label))
           (if (> (length form) 3)
               (let ((eid (form->3ac (if-else form))))
                 ;; and now copy the contructed value to the known return
                 ;; variable
                 (form->3ac `(setf ,result-var ,eid)))
               ;; if there is no else, but we are here, then the
               ;; result if the if expression is NIL
               (form->3ac `(setf ,result-var nil)))

           (form->3ac `(l ,endif-label))))
       (values result-var t)))

    ;; TODO: BROKEN WITHOUT REAL SYMBOL TABLE. (no shadowing, etc.)
    ;;
    ;; using the scoped name of the block, goto the right label to exit.
    ((return-from-p form)  ;; used in BLOCKs and other such things.
     (unless (eq (return-from-name form) (first (first *block-hack*)))
       (error "Fragile return-from broken!"))
     (let ((retvar (second (first *block-hack*)))) ;; TODO: Damn dirty hack
       (emit3ac `(,retvar  = ,(form->3ac (return-from-value form))))
       (emit3ac `(goto ,(third (first *block-hack*))))
       (values retvar t)))

    ;; TODO: BROKEN WITHOUT REAL SYMBOL TABLE (no real symbol table)
    ((block-p form)
     ;; create a named environment by which a return-from may
     ;; exit.  the name must be in the symbol table somewhere and
     ;; it has the exit label I made in here associated with
     ;; it. It also needs the name of the final variable that
     ;; represents the block's value.
     (let* ((lform-one nil)
            (id (symbol-name (gensym "ID")))
            (result-var (gensym "T"))
            (start-of-block
             (gensym (concatenate 'string "BLOCKSTART_" id "_")))
            (end-of-block
             (gensym (concatenate 'string "BLOCKEND_" id "_"))))

       (form->3ac `(l ,start-of-block))

       ;; TODO: A damn dirty hack just for testing. we push
       ;; this name and associated info onto a list so we can
       ;; see it in return-from. We assume the return-from will
       ;; ask for the nearest block name always, this in in
       ;; violation of ansi and should be fixed when we get a
       ;; real sym table.
       (push (list (block-name form) result-var end-of-block)
             *block-hack*)

       (loop for f in (block-body form) do
            (let ((var (gensym "T")))
              (emit3ac `(,var = ,(form->3ac f)))
              (setf lform-one var)))

       (emit3ac `(,result-var = ,lform-one))

       (form->3ac `(l ,end-of-block))

       ;; TODO: HACK! and remove the block out of the scope.
       (pop *block-hack*)

       (values result-var t)))

    ;; TODO: The BLOCK rewrite here in the DEFUN should happen in
    ;; an earlier compiler phase (along with shoving the block
    ;; data into the block appropriate symbol table.

    ((defun-p form) ;; define a non-closure function
     (form->3ac `(l ,(func-name form)))
     (emit3ac `(begin-func ,(length (func-params form))))
     (let ((result-var
            (form->3ac `(block ,(func-name form) ,@(func-body form)))))

       (emit3ac `(return ,result-var))
       (emit3ac `(end-func))

       ;; and return my name, which is sort of like a variable.
       ;; TODO; Not working properly yet. No symbol interning.
       #+ignore (func-name form)
       (values nil nil)))

    ((while-p form) ;; unlisp-like, but easier to implement than DO for now.
     (let* ((id (symbol-name (gensym "ID")))
            (while-start (gensym (concatenate 'string "WHILE_" id "_")))
            (while-end (gensym (concatenate 'string "ENDWHILE_" id "_")))
            (while-result (gensym "T")))
       (form->3ac `(l ,while-start))
       (let ((cid (form->3ac (while-cond form))))
         (emit3ac `(if (= ,cid nil) (goto ,while-end)))
         ;; we don't save the result of the body anywhere.
         (apply #'form->3ac (while-body form))
         (form->3ac `(goto ,while-start))
         ;; now emit the value to compute when the while is done.
         (form->3ac `(l ,while-end))
         (emit3ac `(,while-result = ,(form->3ac (while-result form))))
         (values while-result t))))

    ((setf-p form) ;; make the place evaluate to newval.
     (cond
       ((symbolp (setf-place form))
        (form->3ac `(<- ,(setf-place form) ,(form->3ac (setf-newval form)))))
       (t
        (error "implement setf for form places!"))))

    ((aref-p form)
     ;; implement me
     (error "Implement aref processing."))

    (t
     ;; Everything else is function application.

     ;; Compute the arguments:
     (let ((argvars (mapcar #'form->3ac (args form)))
           (retvar (gensym "T")))
       (loop for i in argvars do
            (emit3ac `(push-param ,i)))
       (emit3ac `(,retvar = call ,(iname form) ,(length (args form))))
       (unless (zerop (length (args form)))
         (emit3ac `(pop-params ,(length (args form)))))
       (values retvar t)))

    ))

;; http://www.inf.unibz.it/~artale/Compiler/slide10-InterCodeGen.pdf
;; http://web.stanford.edu/class/archive/cs/cs143/cs143.1128/handouts/240%20TAC%20Examples.pdf
;; http://web.stanford.edu/class/archive/cs/cs143/cs143.1128/lectures/13/Slides13.pdf

(defun generate-intermediate-code (form)
  (format t ";; New instruction stream for:~%~(~A~)~%~%" form)
  (setf *inst* nil)
  (multiple-value-bind (retvar exists) (form->3ac form)
    (setf *inst* (nreverse *inst*))
    (values retvar exists)))

(defun emit-intermediate-code (forms)
  (let ((*print-right-margin* (* 1024 1024))
        ;;(*print-miser-width* (* 1024 1024))
        (*print-length* (* 1024 1024))
        (*print-level* nil)
        (*print-circle* t)
        (*print-lines* nil))

    (multiple-value-bind (retvar existsp) (generate-intermediate-code forms)
      (format t "  Value var: exists ~(~A~), var: ~(~A~)~%~%" existsp retvar)
      (loop for i in *inst* do
           (if (eq (car i) 'label)
               (format t "~(~A~)~%" i)
               (format t "  ~(~A~)~%" i)))
      (format t "~%"))))

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
;; (form->3ac (list (first form) (second form) (list* (first form) (cddr form))))
