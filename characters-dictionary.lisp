(in-package #:six-dead-mice)

;; This is a STREAMS concept. In SDM, all streams are unsigned 8 bit oriented.
(defstruct byte-buffer
  raw-bytes
  (last-action :unknown)
  (scan-index 0))

;; TODO: This is not utf-8 aware. In fact, it looks like byte reading.
(defun init-byte-buffer (str)
  (let ((chbuf (make-byte-buffer)))
    (let ((raw-bytes (make-array (length str) :element-type '(unsigned-byte 8))))
      (dotimes (i (length str))
        (setf (aref raw-bytes i) (.char-code (aref str i))))
      (setf (byte-buffer-raw-bytes chbuf) raw-bytes))
    chbuf))



(defun .code-char (code)
  (code-char code))

(defun .char-code (ch)
  (char-code ch))

(defun .char-downcase (ch)
  (char-downcase ch))

(defun .char-upcase (ch)
  (char-upcase ch))

;; read a utf-8 character out of the byte stream
(defun .read-char (&optional input-stream (eof-error-p t) eof-value recursive-p)
  (declare (ignorable recursive-p))
  (when (= (byte-buffer-scan-index input-stream)
           (length (byte-buffer-raw-bytes input-stream)))
    (if eof-error-p
        (.error 'end-of-file)
        (return-from .read-char eof-value)))

  (let ((c (aref (byte-buffer-raw-bytes input-stream)
                 (byte-buffer-scan-index input-stream))))
    (incf (byte-buffer-scan-index input-stream))
    (setf (byte-buffer-last-action input-stream) :action-read)
    (.code-char c)))


(defun .unread-char (ch &optional input-stream)
  ;; if the last operation wasn't a read from the stream, bail.
  (unless (eq :action-read (byte-buffer-last-action input-stream))
    (.error 'type-error "Must read before an unread can happen."))

  ;; if the last character wasn't what I'm about to unread, bail.
  (unless (eql (aref (byte-buffer-raw-bytes input-stream)
                     (1- (byte-buffer-scan-index input-stream)))
               (.char-code ch))
    (.error 'type-error "Can't unread incorrect character."))

  ;; ok, decrement the scan-index and set the action
  (decf (byte-buffer-scan-index input-stream))
  (setf (byte-buffer-last-action input-stream) :action-unread)
  ;; according to ANSI, must return this.
  nil)


;; XXX Move to a lisp object dictionary.
;; (defstruct lo type value)

;;
