(in-package :six-dead-mice)

;;;; This is another prototype of a compiler for a dialect called literal-lisp
;;;; which is heavily C-like in its nature. So far, this only describes a
;;;; translation of it to three address code. It is similar to prescheme.

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

;; function applications of primitive functions. (+ 1 2 3) etc.
(defun op (form)
  (first form))
(defun arg1 (form)
  (second form))
(defun arg2 (form)
  (third form))
(defun arg3+ (form)
  (cdddr form))

;; (progn 1 2 3 4)
(defun progn-body (ast)
  (cdr ast))

;; (label foo)
(defun label (form)
  (second form))

;; (if cond-expr then <optional-else>)
(defun if-cond (form)
  (second form))
(defun if-then (form)
  (third form))
(defun if-else (form)
  (fourth form))

;; (<- dest source) [copy variable]
(defun dest (ast)
  (second ast))
(defun source (ast)
  (third ast))

;; function definition: (defun foo (a b c) (+ 1 2 3))
(defun func-name (ast)
  (second ast))
(defun func-params (ast)
  (third ast))
(defun func-body (ast)
  (cdddr ast))

;; function invocations: (foo 1 2 3)
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

;; setf form: (setf x 42)
(defun setf-place (ast)
  (second ast))
(defun setf-newval (ast)
  (third ast))

;; block form: (block foo 1 2 3 4)
(defun block-name (ast)
  (second ast))
(defun block-body (ast)
  (cddr ast))

;; return-from form: (block foo 1 2 3 (return-from foo 42) 4 5 6)
(defun return-from-name (ast)
  (second ast))
(defun return-from-value (ast)
  (third ast))

(defparameter *block-hack* nil)

(defparameter *inst* nil)

(defun emit3ac (form)
  (push form *inst*))

(defun ast->3ac (ast)
  "This may cause TAC instructions to be emitted into *inst*. It returns
a values of either: variable assigned and T, or nil and nil."
  (if (atom ast)
      (cond
        ((integerp ast) ;; literal number
         (values ast t))
        ((symbolp ast) ;; a symbol name
         (values ast t))
        (t
         (error "oops, don't know how to handle this: ~A" ast)))

      (case (first ast)
        ('progn ;; sequencing operator, last expr is value.
          ;; return the label to the last thing processed.
          (let ((last-one nil))
            (loop for f in (progn-body ast) do
                 (let ((var (gensym "T")))
                   (multiple-value-bind (retvar existsp) (ast->3ac f)
                     (when existsp
                       (emit3ac `(,var = ,retvar))
                       ;; store the last var we've seen, this is the result of
                       ;; the progn form.
                       (setf last-one var)))))

            (if last-one
                (values last-one t)
                (values nil nil))))

        ;; TODO: The relational operators are wrong when using multiple args
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
                  (emit3ac `(,name = ,(ast->3ac (second ast))
                                   ,(first ast)
                                   ,(ast->3ac (third ast))))
                  (values name t))
                 ;; handle uanary case (the op and the number)
                 ((= (length ast) 2)
                  (case (op ast)
                    (+
                     (emit3ac `(,name = 0 + ,(ast->3ac (arg1 ast)))))
                    (-
                     (emit3ac `(,name = 0 - ,(ast->3ac (arg1 ast)))))
                    (*
                     (emit3ac `(,name = 1 * ,(ast->3ac (arg1 ast)))))
                    (/
                     (emit3ac `(,name = 1 / ,(ast->3ac (arg1 ast)))))
                    ((< > <= >= = /=)
                     ;; transform, but ignore, whatever value is
                     ;; there, and return T
                     (emit3ac `(,(gensym "T") = ,(ast->3ac (arg1 ast))))
                     (emit3ac `(,name = T))
                     (values name t))))
                 (t
                  (error "figure out what is wrong here in binop."))))))


        (l ;; label
         (emit3ac `(label ,(label ast)))
         (values nil nil))


        (goto ;; goto
         (emit3ac `(goto ,(label ast)))
         (values nil nil))


        (<- ;; copy from one variable to another
         ;; this is not a pointer assignment or reference, it is just a copy
         (emit3ac `(,(dest ast) = ,(source ast)))
         (values (dest ast) t))

        (if ;; (if cond-expr then &optional else)
         (let* ((id (symbol-name (gensym "ID")))
                (if-start (gensym (concatenate 'string "IF_" id "_")))
                (else-label (gensym (concatenate 'string "ELSE_" id "_")))
                (endif-label (gensym (concatenate 'string "ENDIF_" id "_")))
                (result-var (gensym "T")))

           (ast->3ac `(l ,if-start))

           (let ((cid (ast->3ac (if-cond ast))))
             (emit3ac `(if (= ,cid NIL) (goto ,else-label)))
             (let ((tid (ast->3ac (if-then ast))))
               (ast->3ac `(<- ,result-var ,tid))
               (ast->3ac `(goto ,endif-label))
               (ast->3ac `(l ,else-label))
               (if (> (length ast) 3)
                   (let ((eid (ast->3ac (if-else ast))))
                     ;; and now copy the contructed value to the known return
                     ;; variable
                     (ast->3ac `(setf ,result-var ,eid)))
                   ;; if there is no else, but we are here, then the
                   ;; result if the if expression is NIL
                   (ast->3ac `(setf ,result-var nil)))

               (ast->3ac `(l ,endif-label))))
           (values result-var t)))

        ;; TODO: BROKEN WITHOUT REAL SYMBOL TABLE. (no shadowing, etc.)
        ;;
        ;; using the scoped name of the block, goto the right label to exit.
        (return-from  ;; used in BLOCKs and other such things.
         (unless (eq (return-from-name ast) (first (first *block-hack*)))
           (error "Fragile return-from broken!"))
          (let ((retvar (second (first *block-hack*)))) ;; TODO: Damn dirty hack
            (emit3ac `(,retvar  = ,(ast->3ac (return-from-value ast))))
            (emit3ac `(goto ,(third (first *block-hack*))))
            (values retvar t)))

        ;; TODO: BROKEN WITHOUT REAL SYMBOL TABLE (no real symbol table)
        (block ;; create a named environment by which a return-from may exit.
            ;; the name must be in the symbol table somewhere and it has the
            ;; exit label I made in here associated with it. It also needs
            ;; the name of the final variable that represents the block's
            ;; value.
            (let* ((last-one nil)
                   (id (symbol-name (gensym "ID")))
                   (result-var (gensym "T"))
                   (start-of-block
                    (gensym (concatenate 'string "BLOCKSTART_" id "_")))
                   (end-of-block
                    (gensym (concatenate 'string "BLOCKEND_" id "_"))))

              (ast->3ac `(l ,start-of-block))

              ;; TODO: A damn dirty hack just for testing. we push
              ;; this name and associated info onto a list so we can
              ;; see it in return-from. We assume the return-from will
              ;; ask for the nearest block name always, this in in
              ;; violation of ansi and should be fixed when we get a
              ;; real sym table.
              (push (list (block-name ast) result-var end-of-block)
                    *block-hack*)

              (loop for f in (block-body ast) do
                   (let ((var (gensym "T")))
                     (emit3ac `(,var = ,(ast->3ac f)))
                     (setf last-one var)))

              (emit3ac `(,result-var = ,last-one))

              (ast->3ac `(l ,end-of-block))

              ;; TODO: HACK! and remove the block out of the scope.
              (pop *block-hack*)

              (values result-var t)))

        ;; TODO: The BLOCK rewrite here in the DEFUN should happen in
        ;; an earlier compiler phase (along with shoving the block
        ;; data into the block appropriate symbol table.

        (defun ;; define a non-closure function
            (ast->3ac `(l ,(func-name ast)))
            (emit3ac `(begin-func ,(length (func-params ast))))
          (let ((result-var
                 (ast->3ac `(block ,(func-name ast) ,@(func-body ast)))))

            (emit3ac `(return ,result-var))
            (emit3ac `(end-func))

            ;; and return my name, which is sort of like a variable.
            ;; TODO; Not working properly yet. No symbol interning.
            #+ignore (func-name ast)
            (values nil nil)))

        (while ;; unlisp-like, but easier to implement than DO fo rnow.
            (let* ((id (symbol-name (gensym "ID")))
                   (while-start (gensym (concatenate 'string "WHILE_" id "_")))
                   (while-end (gensym (concatenate 'string "ENDWHILE_" id "_")))
                   (while-result (gensym "T")))
              (ast->3ac `(l ,while-start))
              (let ((cid (ast->3ac (while-cond ast))))
                (emit3ac `(if (= ,cid nil) (goto ,while-end)))
                ;; we don't save the result of the body anywhere.
                (apply #'ast->3ac (while-body ast))
                (ast->3ac `(goto ,while-start))
                ;; now emit the value to compute when the while is done.
                (ast->3ac `(l ,while-end))
                (emit3ac `(,while-result = ,(ast->3ac (while-result ast))))
                (values while-result t))))

        (setf ;; make the place evaluate to newval.
         (cond
           ((symbolp (setf-place ast))
            (ast->3ac `(<- ,(setf-place ast) ,(ast->3ac (setf-newval ast)))))
           (t
            (error "implement setf for form places!"))))

        (aref
         ;; implement me
         (error "Implement aref processing."))

        (t
         ;; Everything else is function application.

         ;; Compute the arguments:
         (let ((argvars (mapcar #'ast->3ac (args ast)))
               (retvar (gensym "T")))
           (loop for i in argvars do
                (emit3ac `(push-param ,i)))
           (emit3ac `(,retvar = call ,(iname ast) ,(length (args ast))))
           (unless (zerop (length (args ast)))
             (emit3ac `(pop-params ,(length (args ast)))))
           (values retvar t)))






        )))

;; http://www.inf.unibz.it/~artale/Compiler/slide10-InterCodeGen.pdf
;; http://web.stanford.edu/class/archive/cs/cs143/cs143.1128/handouts/240%20TAC%20Examples.pdf
;; http://web.stanford.edu/class/archive/cs/cs143/cs143.1128/lectures/13/Slides13.pdf

(defun generate-intermediate-code (ast)
  (format t ";; New instruction stream for:~%~(~A~)~%~%" ast)
  (setf *inst* nil)
  (multiple-value-bind (retvar exists) (ast->3ac ast)
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
;; (ast->3ac (list (first ast) (second ast) (list* (first ast) (cddr ast))))
