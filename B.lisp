(in-package :lang-b)

;; Hrm, create a concept of extendable lambda dispatchers.

(defun make-value (value value-type)
  "A VALUE is a first class citizen, the concept of a value has a type of
'(:value t), and the actual value has its own type. Values are read only after
creation. In a way, this is similar to a lisp-object in a C implementation."
  ((lambda (value value-type) ;; the actual value, the value type
     (lambda (cmd)
       (cond
         ((eq cmd :type-of)
          '(:value :t))
         ((eq cmd :type-contained)
          value-type)
         ((eq cmd :read)
          value))))
   value value-type))


(defun make-place (&key
                     (store-val (make-value 'unbound 'unknown)))
  "A PLACE is a first class citizen that is a memory location which
can hold any VALUE. The type of the place itself is (:place t) where
the type of the value it contains is often something else. A VALUE is
storable in a PLACE."
  ((lambda (place) ;; the actual place, NOTE: could get from an array.
     (lambda (cmd &optional val)
       (cond
         ((eq cmd :type-of)
          ;; The type of THIS object, not the type of the value in the place.
          '(:place :t))
         ((eq cmd :type-contained)
          (funcall place :type-of))
         ((eq cmd :read)
          place)
         ((eq cmd :write)
          ;; NOTE: In here, if I told a place how to verify the type
          ;; and range of the value stored in it, then it can check those
          ;; things before allowing the assignment.

          ;; The side effecting update.
          (setq place val)))))
   store-val))

;; This is not exactly generalized, but x must resolve to a place. So in a sense
;; it is generalized since it may be a complex form to locate the place.
(defun /set (place value)
  (unless (equal (funcall place :type-of) '(:place :t))
    (error "/SET given a bad place."))
  (funcall place :write value))

;;
;; These are used for both VALUE and PLACE
;;
(defun /value (x)
  (funcall x :read))

(defun /type-of (obj)
  (funcall obj :type-of))

(defun /type-contained (obj)
  (funcall obj :type-contained))

;; There is only one NIL object.
(defparameter *%value-nil* (make-value nil '(:nil)))
(defun /vnil ()
  *%value-nil*)

;; There is only ONE T object.
(defparameter *%value-t* (make-value t '(:t)))
(defun /vt ()
  *%value-t*)

;; Make an integer value.
(defun /vint (val)
  (make-value val '(:integer :ratio :real :t)))

;; Make a string value.
(defun /vstring (val)
  (make-value val '((:vector (*) :character) (:array (*) *) (:sequence (*) *) :t)))

(defun /cons (a b)
  (let ((the-car (make-place :store-val a))
        (the-cdr (make-place :store-val b)))
    (lambda (cmd &optional slot val)
      (cond
        ((eq cmd :type-of)
         '(:cons t))
        ((eq cmd :read)
         (cond
           ((eq slot :car)
            the-car)
           ((eq slot :cdr)
            the-cdr)))
        ((eq cmd :write)
         (cond
           ((eq slot :car)
            (funcall the-car :write val))
           ((eq slot :cdr)
            (funcall the-cdr :write val))))))))

(defun /list (&rest args)
  (if (null args)
      (/vnil)
      (/cons (car args) (apply #'/list (cdr args)))))

;; get the car PLACE of the passed in cons.
(defun /par (x)
  (funcall x :read :car))

;; get the VALUE in the PLACE of the passed in cons.
(defun /car (x)
  (/value (funcall x :read :car)))

;; set the value into the car PLACE in the cons.
(defun /rplaca (x val)
  (funcall (/par x) :write val))

;; And the same for CDR.
(defun /pdr (x)
  (funcall x :read :cdr))

(defun /cdr (x)
  (/value (funcall x :read :cdr)))

(defun /rplacd (x val)
  (funcall x :write :cdr val))


(defun unparse (obj)
  (let ((*print-right-margin* (* 1024 1024))
        (*print-miser-width* (* 1024 1024))
        (*print-length* (* 1024 1024))
        (*print-level* nil)
        (*print-circle* t)
        (*print-lines* nil))
    (labels ((%unparse (obj)
               (cond
                 ((eq (car (/type-of obj)) :value)
                  (format t "{~S ~(~S~)}" (/value obj) (/type-contained obj))
                  #+ignore(format t "{~S}" (/value obj)))

                 ((eq (car (/type-of obj)) :place)
                  (format t "[")
                  (%unparse (/value obj))
                  (format t "]"))

                 ((eq (car (/type-of obj)) :cons)
                  (format t "(")
                  (%unparse (/par obj))
                  (format t " . ")
                  (%unparse (/pdr obj))
                  (format t ")"))

                 ((eq (car (/type-of obj)) :nil)
                  (format t "{NIL}"))

                 ((eq (car (/type-of obj)) :t)
                  (format t "{T}"))

                 )))

      (%unparse obj))))
