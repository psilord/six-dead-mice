(in-package #:six-dead-mice)


(defun .error (err &rest args)
  ;; emulate in lisp;
  (apply #'error err args))


;;;
;;; Check constituent traits
;;;
(defun %.has-some-constituent-trait-p (rt ch &rest traits)
  ;; See if the character has at least one of the traits specified
  (let ((rt-stable (%readtable-syn-table rt)))
    (multiple-value-bind (value presentp)
        (gethash ch rt-stable)
      (when presentp
        (intersection traits (%syntax-type-constituent-traits value))))))

(defun .invalid-character-p (rt ch)
  (%.has-some-constituent-trait-p rt ch :invalid))


;;;
;;; Check syntax-types
;;;
(defun %.is-syntax-type (rt ch syntype)
  (let ((rt-stable (%readtable-syn-table rt)))
    (multiple-value-bind (value presentp)
        (gethash ch rt-stable)
      (when presentp
        (eq syntype (%syntax-type-syntax-type value))))))

(defun .whitespace[2]-p (rt ch)
  (%.is-syntax-type rt ch :whitespace[2]))

(defun .terminating-macro-character-p (rt ch)
  (%.is-syntax-type rt ch :terminating-macro-char))

(defun .nonterminating-macro-character-p (rt ch)
  (%.is-syntax-type rt ch :nonterminating-macro-char))

(defun .single-escape-character-p (rt ch)
  (%.is-syntax-type rt ch :single-escape))

(defun .multiple-escape-character-p (rt ch)
  (%.is-syntax-type rt ch :multiple-escape))

(defun .constituent-character-p (rt ch)
  (%.is-syntax-type rt ch :constituent))

(defun .has-case-p (rt ch)
  (let ((rt-stable (%readtable-syn-table rt)))
    (multiple-value-bind (value presentp)
        (gethash ch rt-stable)
      (when presentp
        (%syntax-type-case-spec value)))))

(defun .possibly-change-case (rt ch)
  (declare (ignore rt))
  ;; Depending upon the read table settings, we may change the case of this
  ;; character and return it.

  ;; TODO: Implement me.
  ch)

(defun .reader-macro-character-p (rt ch)
  (or (.terminating-macro-character-p rt ch)
      (.nonterminating-macro-character-p rt ch)))


(defun .invoke-reader-macro (rt is ch)
  (declare (ignore rt is ch))
  nil)


;; This is the function which takes the READ in token and converts it into
;; a true object.
(defun .objectify (token)
  "Convert the string token into a real lisp object and return the values
of VALID and OBJECT."
  (values t (list :object token)))

;; This differs from READ because I MUST pass in a readtable.
(defun .read (rt &optional input-stream eof-error-p eof-value recursive-p)
  (declare (ignorable recursive-p))
  (let ((x nil)
        (token-accum ()))

    (tagbody
     1 ;; If at end of file, end-of-file processing is performed as
       ;; specified in read. Otherwise, one character, x, is read
       ;; from the input stream, and dispatched according to the
       ;; syntax type of x to one of steps 2 to 7.
       (setf x (.read-char input-stream nil 'the-end))
       (format t "Step 1: ~S~%" x)

       (when (eq x 'the-end)
         (if eof-error-p
             (.error 'end-of-file)
             (return-from .read eof-value)))


     2 ;; If x is an invalid character, an error of type
       ;; reader-error is signaled.
       (format t "Step 2: ~S~%" x)
       (when (and (.constituent-character-p rt x)
                  (.invalid-character-p rt x))
         (.error 'reader-error))

     3 ;; If x is a whitespace[2] character, then it is discarded
       ;; and step 1 is re-entered.
       (format t "Step 3: ~S~%" x)
       (when (.whitespace[2]-p rt x)
         (go 1))

     4 ;; If x is a terminating or non-terminating macro character
       ;; then its associated reader macro function is called with two
       ;; arguments, the input stream and x.

       ;; The reader macro function may read characters from the input
       ;; stream; if it does, it will see those characters following
       ;; the macro character. The Lisp reader may be invoked
       ;; recursively from the reader macro function.

       ;; The reader macro function must not have any side effects
       ;; other than on the input stream; because of backtracking and
       ;; restarting of the read operation, front ends to the Lisp
       ;; reader (e.g., "editors" and "rubout handlers") may cause
       ;; the reader macro function to be called repeatedly during the
       ;; reading of a single expression in which x only appears once.

       ;; The reader macro function may return zero values or one
       ;; value. If one value is returned, then that value is returned
       ;; as the result of the read operation; the algorithm is
       ;; done. If zero values are returned, then step 1 is
       ;; re-entered.
       (format t "Step 4: ~S~%" x)
       (when (.reader-macro-character-p rt x)

         (let ((ret (multiple-value-list
                     (.invoke-reader-macro rt input-stream x))))
           (if (null ret)
               (go 1)
               (return-from .read (car ret)))))

     5 ;; If x is a single escape character then the next character,
       ;; y, is read, or an error of type end-of-file is signaled if
       ;; at the end of file. y is treated as if it is a constituent
       ;; whose only constituent trait is alphabetic[2]. y is used to
       ;; begin a token, and step 8 is entered.
       (format t "Step 5: ~S~%" x)
       (when (.single-escape-character-p rt x)
         (let ((y (.read-char input-stream nil 'the-end)))
           (when (eq y 'the-end)
             (.error 'end-of-file))
           (setf token-accum nil)
           (push y token-accum)
           (go 8)))

     6 ;;  If x is a multiple escape character then a token (initially
       ;;  containing no characters) is begun and step 9 is entered.
       (format t "Step 6: ~S~%" x)
       (when (.multiple-escape-character-p rt x)
         (setf token-accum nil)
         (go 9))

     7 ;;  If x is a constituent character, then it begins a
       ;;  token. After the token is read in, it will be interpreted
       ;;  either as a Lisp object or as being of invalid syntax. If
       ;;  the token represents an object, that object is returned as
       ;;  the result of the read operation. If the token is of
       ;;  invalid syntax, an error is signaled. If x is a character
       ;;  with case, it might be replaced with the corresponding
       ;;  character of the opposite case, depending on the readtable
       ;;  case of the current readtable, as outlined in Section
       ;;  23.1.2 (Effect of Readtable Case on the Lisp Reader). X is
       ;;  used to begin a token, and step 8 is entered.
       (format t "Step 7: ~S~%" x)
       (when (.constituent-character-p rt x)
         (setf token-accum nil)
         (when (.has-case-p rt x)
           (setf x (.possibly-change-case rt x))) ;; possible change case
         (push x token-accum)
         (go 8))

     8 ;; At this point a token is being accumulated, and an even
       ;; number of multiple escape characters have been
       ;; encountered. If at end of file, step 10 is
       ;; entered. Otherwise, a character, y, is read, and one of the
       ;; following actions is performed according to its syntax type:

       (let ((y (.read-char input-stream nil 'the-end)))
         (format t "Step 8(y): ~S~%" y)
         (when (eq y 'the-end)
           (go 10))

         ;; If y is a constituent or non-terminating macro character:
         (when (or (.constituent-character-p rt y)
                   (.nonterminating-macro-character-p rt y))
           ;;  If y is a character with case, it might be replaced
           ;;  with the corresponding character of the opposite case,
           ;;  depending on the readtable case of the current
           ;;  readtable, as outlined in Section 23.1.2 (Effect of
           ;;  Readtable Case on the Lisp Reader).
           (when (.has-case-p rt y)
             (setf y (.possibly-change-case rt y))) ; possibly change case.

           ;; Y is appended to the token being built.
           (push y token-accum)
           (go 8))

         ;;  If y is a single escape character, then the next
         ;;  character, z, is read, or an error of type end-of-file is
         ;;  signaled if at end of file. Z is treated as if it is a
         ;;  constituent whose only constituent trait is
         ;;  alphabetic[2]. Z is appended to the token being built,
         ;;  and step 8 is repeated.
         (when (.single-escape-character-p rt y)
           (let ((z (.read-char input-stream nil 'the-end)))
             (when (eq z 'the-end)
               (.error 'end-of-file))
             (push z token-accum)
             (go 8)))

         ;;  If y is a multiple escape character, then step 9 is entered.
         (when (.multiple-escape-character-p rt y)
           (go 9))

         ;; If y is an invalid character, an error of type
         ;; reader-error is signaled.
         (when (and (.constituent-character-p rt y)
                    (.invalid-character-p rt y))
           (.error 'reader-error))

         ;; If y is a terminating macro character, then it terminates
         ;; the token. First the character y is unread (see
         ;; unread-char), and then step 10 is entered.
         (when (.terminating-macro-character-p rt y)
           (.unread-char y input-stream)
           (go 10))

         ;;  If y is a whitespace[2] character, then it terminates the
         ;;  token. First the character y is unread if appropriate
         ;;  (see read-preserving-whitespace), and then step 10 is
         ;;  entered.
         (when (.whitespace[2]-p rt y)
           ;; TODO: not handling read-preserving-whitespace
           (.unread-char y input-stream)
           (go 10)))

     9 ;;  At this point a token is being accumulated, and an odd
       ;;  number of multiple escape characters have been
       ;;  encountered. If at end of file, an error of type
       ;;  end-of-file is signaled. Otherwise, a character, y, is
       ;;  read, and one of the following actions is performed
       ;;  according to its syntax type:
       (let ((y (.read-char input-stream nil 'the-end)))
         (format t "Step 9(y): ~S~%" y)
         (when (eq y 'the-end)
           (.error 'end-of-file))

         ;;  If y is a constituent, macro, or whitespace[2] character,
         ;;  y is treated as a constituent whose only constituent
         ;;  trait is alphabetic[2]. Y is appended to the token being
         ;;  built, and step 9 is repeated.
         (when (or (.constituent-character-p rt y)
                   (.reader-macro-character-p rt y)
                   (.whitespace[2]-p rt y))
           (push y token-accum)
           (go 9))

         ;;  If y is a single escape character, then the next
         ;;  character, z, is read, or an error of type end-of-file is
         ;;  signaled if at end of file. Z is treated as a constituent
         ;;  whose only constituent trait is alphabetic[2]. Z is
         ;;  appended to the token being built, and step 9 is
         ;;  repeated.
         (when (.single-escape-character-p rt y)
           (let ((z (.read-char input-stream nil 'the-end)))
             (when (eq z 'the-end)
               (.error 'end-of-file))

             (push z token-accum)
             (go 9)))

         ;; If y is a multiple escape character, then step 8 is entered.
         (when (.multiple-escape-character-p rt y)
           (go 8))

         ;;  If y is an invalid character, an error of type
         ;;  reader-error is signaled.
         (when (.invalid-character-p rt y)
           (.error 'reader-error)))

     10 ;; An entire token has been accumulated. The object
       ;; represented by the token is returned as the result of the
       ;; read operation, or an error of type reader-error is
       ;; signaled if the token is not of valid syntax.
       (format t "Step 10: ~S~%" token-accum)
       (let ((token (concatenate 'string (nreverse token-accum))))
         (multiple-value-bind (valid object) (.objectify token)
           (if valid
               (return-from .read object)
               (.error 'reader-error))))

       ;; Should never reach here.
       )))



(defun six-dead-mice ()
  nil)
