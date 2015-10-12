(in-package :lang-a)

(defstruct %syntax-type
  syntax-type
  constituent-traits
  ;; if this doesn't have a case, then the case-spec is nil, if it does have
  ;; a case, then it is a list of current case, and inverted case and case char.
  ;; Suppose this is a syntax type for a lower case 'x'.
  ;; (case-spec (:lower :upper #\X)
  ;; and for an upper case 'X'.
  ;; (case-spec (:upper :lower #\x)
  (case-spec nil))

(defstruct %readtable
  ;; TODO: Change this for a reading case and a writing case.
  ;; :upcase, :downcase, :preserve, :invert. Used for both reading and writing.
  (case-mode :upcase)
  ;; keyed by character, value syntax-type object
  (syn-table (make-hash-table :test #'eql))
  ;; keyed by character, value function
  (macro-table (make-hash-table :test #'eql)))

;; Enhanced API for readtables.
(defun .insert-syntax-char (rt ch syn-type case-spec &rest ctraits)
  (setf (gethash ch (%readtable-syn-table rt))
        (make-%syntax-type :syntax-type syn-type
                           :case-spec (copy-seq case-spec)
                           :constituent-traits (copy-seq ctraits))))

;; make standard syntax readtable for ANSI Common Lisp (without #.)
(defun .make-readtable ()
  "Return a read table suitable for Common Lisp"
  (let* ((rt (make-%readtable)))

    ;; Insert syntax types with constituent traits if appropriate...
    (mapc #'(lambda (entry)
              (destructuring-bind (ch syn-type case-spec . ctraits)
                  entry
                (apply #'.insert-syntax-char rt ch syn-type case-spec ctraits)))

          ;; From ANSI spec:
          ;; char, syntax-type, case-spec, &rest constituent trait list
          `((#\Backspace :constituent nil :invalid)
            (#\Tab :whitespace[2] nil :invalid)
            (#\Newline :whitespace[2] nil :invalid)
            (#\Linefeed :whitespace[2] nil :invalid)
            (#\Page :whitespace[2] nil :invalid)
            (#\Return :whitespace[2] nil :invalid)
            (#\Space :whitespace[2] nil :invalid)
            (#\! :constituent nil :alphabetic[2])
            (#\" :terminating-macro-char nil :alphabetic[2])
            (#\# :nonterminating-macro-char nil :alphabetic[2])
            (#\$ :constituent nil :alphabetic[2])
            (#\% :constituent nil :alphabetic[2])
            (#\& :constituent nil :alphabetic[2])
            (#\' :terminating-macro-char nil :alphabetic[2])
            (#\( :terminating-macro-char nil :alphabetic[2])
            (#\) :terminating-macro-char nil :alphabetic[2])
            (#\* :constituent nil :alphabetic[2])
            (#\, :terminating-macro-char nil :alphabetic[2])
            (#\- :constituent nil :alphabetic[2])
            (#\. :constituent nil :alphabetic[2] :dot :decimal-point)
            (#\/ :constituent nil :alphabetic[2] :ratio-marker)

            ;; 0 - 9
            ,@(loop for c from (.char-code #\0) upto (.char-code #\9) collect
                   `(,(.code-char c) :constituent nil :alphadigit))

            (#\: :constituent nil :package-marker)
            (#\; :terminating-macro-char nil :alphabetic[2])
            (#\< :constituent nil :alphabetic[2])
            (#\= :constituent nil :alphabetic[2])
            (#\> :constituent nil :alphabetic[2])
            (#\? :constituent nil :alphabetic[2])
            (#\@ :constituent nil :alphabetic[2])

            ;; A - Z
            ,@(loop for c from (.char-code #\A) upto (.char-code #\Z) collect
                   `(,(.code-char c)
                      :constituent
                      ,(list :upper :lower (.char-downcase (.code-char c)))
                      :alphadigit
                      ,@(cond
                         ((eql (.code-char c) #\D)
                          (list :double-float-exponent-marker))
                         ((eql (.code-char c) #\E)
                          (list :float-exponent-marker))
                         ((eql (.code-char c) #\F)
                          (list :single-float-exponent-marker))
                         ((eql (.code-char c) #\L)
                          (list :long-float-exponent-marker))
                         ((eql (.code-char c) #\S)
                          (list :short-float-exponent-marker))
                         (t
                          nil))))

            (#\[ :constituent nil :alphabetic[2])
            (#\\ :single-escape nil :alphabetic[2])
            (#\] :constituent nil :alphabetic[2])
            (#\^ :constituent nil :alphabetic[2])
            (#\_ :constituent nil :alphabetic[2])
            (#\` :terminating-macro-char nil :alphabetic[2])

            ;; a - z
            ,@(loop for c from (.char-code #\a) upto (.char-code #\z) collect
                   `(,(.code-char c)
                      :constituent
                      ,(list :lower :upper (.char-upcase (.code-char c)))
                      :alphadigit
                      ,@(cond
                         ((eql (.code-char c) #\d)
                          (list :double-float-exponent-marker))
                         ((eql (.code-char c) #\e)
                          (list :float-exponent-marker))
                         ((eql (.code-char c) #\f)
                          (list :single-float-exponent-marker))
                         ((eql (.code-char c) #\l)
                          (list :long-float-exponent-marker))
                         ((eql (.code-char c) #\s)
                          (list :short-float-exponent-marker))
                         (t
                          nil))))

            (#\{ :constituent nil :alphabetic[2])
            (#\| :multiple-escape nil :alphabetic[2])
            (#\} :constituent nil :alphabetic[2])
            (#\~ :constituent nil :alphabetic[2])
            (#\Rubout :constituent nil :invalid)

            ))

    rt))
