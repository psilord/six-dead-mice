(declaim (optimize (debug 3) (safety 3)))
(in-package :lang-c)


;; A different experiment about writing a core lisp implementation
;; rooted into a real memory layout of traditional machines.

(defmacro defenum (&body enums)
  "Just a small useful thing to make defining constants easier."
  `(progn
     ,@(loop for enum in enums collecting
            (if (consp enum)
                `(defparameter ,(car enum) ,(cadr enum))
                `(defparameter ,enum nil)))))

(defenum ;; enums for offsets from the base address for each kind of lisp obj.
  ;; Offset for slot locations in all objects.
  (+lo/type+ 0) ;; All objects have a type field.
  ;; Values
  (+lo/atom/value+ 1)
  ;; Conses
  (+lo/cons/car+ 1)
  (+lo/cons/cdr+ 2)
  ;; Vectors
  (+lo/vector/elem-type+ 1)
  (+lo/vector/length+ 2)
  (+lo/vector/data+ 3)
  )

(defenum ;; enums that describes base hardware/lisp types.
  (+type/integer+ 0)
  (+type/string+ 1) ;; todo: deal with sequence types.
  (+type/cons+ 2)
  (+type/vector+ 3) ;; todo: deal with sequence types.
  (+type/nil+ 4)
  (+type/t+ 5))

;; TODO: make memory two half-spaces and garbage collect with mark and sweep.
(defparameter *mem-length* 1024) ;; slots for now, soon bytes!
(defparameter *mem* (make-array *mem-length*)) ;; Make pointer to real memory
(defparameter *index* 0)

(defun //init-mem ()
  (setf *mem* (make-array *mem-length*)
        *index* 0))

(defun //alloc-mem (num-cells)
  "Allocate NUM-CELLs of memory, then return the address of the start
of it. The allocation is a simple increment of a pointer through
memory."
  (let ((mem *index*))
    (incf *index* num-cells)
    mem))

(defun //stm (address value)
  (setf (aref *mem* address) value))

(defun //ldm (address)
  (aref *mem* address))

(defun //alloc-typed-mem (the-type num-cells)
  ;; We assume the type goes into the first cell
  (let ((addr (//alloc-mem (+ num-cells 1))))
    (//stm (+ addr +lo/type+) the-type)
    addr))

(defun //type-of (addr)
  (//ldm addr))

(defun //check-type (addr the-type &optional msg)
  (unless (eql (//type-of addr) the-type)
    (error (concatenate 'string "Failed //check-type: " msg))))

;; There is no //free-mem, there will be garbage collection.

;; Contains the header of the atom type
(defun //vatom (val val-type)
  (let* ((addr (//alloc-typed-mem val-type 1)))
    ;; addr itself is the type, so I want the next one for the value
    (//stm (+ addr +lo/atom/value+) val)
    addr))

;; Get the value of the atom in memory.
(defun //value (addr)
  (//ldm (+ addr +lo/atom/value+)))

(defun //vint (val)
  (//vatom val +type/integer+))

(defun //vstring (val)
  (//vatom val +type/string+))

;; There should only be one of these objects. (but there isn't yet)
(defun //vnil ()
  (//vatom 0 +type/nil+))

;; Ditto.
(defun //vtrue ()
  (//vatom 1 +type/t+))

(defun //cons (a b)
  (let* ((addr (//alloc-typed-mem +type/cons+ 2)))
    (//stm (+ addr +lo/cons/car+) a)
    (//stm (+ addr +lo/cons/cdr+) b)
    addr))

(defun //car (addr)
  (//check-type addr +type/cons+ "not a cons.")
  (//ldm (+ addr +lo/cons/car+)))

;; inverse of car
(defun //rplaca (addr val-addr)
  (//check-type addr +type/cons+ "not a cons.")
  (//stm (+ addr +lo/cons/car+) val-addr)
  addr)

(defun //cdr (addr)
  (//check-type addr +type/cons+ "not a cons.")
  (//ldm (+ addr +lo/cons/cdr+)))

;; inverse of cdr
(defun //rplacd (addr val-addr)
  (//check-type addr +type/cons+ "not a cons.")
  (//stm (+ addr +lo/cons/cdr+) val-addr)
  addr)

(defun //vector (the-type &rest args)
  ;; We add 1 for the storage of the length of the array into the blob.
  ;; We add 1 for the element-type of the array. The element type may
  ;; radically alter how we physically layout the array.
  (let ((addr (//alloc-typed-mem +type/vector+ (+ (length args) 2)))
        (idx 0))

    ;; store the type of the elements. (it should be a lobj symbol, but we're
    ;; using CL's symbols for now because I don't have symbols implemented
    ;; in this dialect yet.
    (//stm (+ addr +lo/vector/elem-type+) the-type)

    ;; store the length of the vector. (it doesn't have to be a lisp object).
    (//stm (+ addr +lo/vector/length+) (length args))

    ;; and now insert the argument addresses into the array
    (dolist (arg args)
      (//stm (+ addr idx +lo/vector/data+) arg)
      (incf idx))

    ;; and return the reference to it.
    addr))

(defun //array-element-type (addr)
  (//check-type addr +type/vector+)
  (//ldm (+ addr +lo/vector/elem-type+)))

(defun //svref (addr idx)
  (//check-type addr +type/vector+ "not a vector.")
  (//ldm (+ addr +lo/vector/data+ (//value idx))))

;; inverse of //svref (not in lisp)
(defun //svset (addr idx val-addr)
  ;; check to ensure I'm working on a vector.
  (//check-type addr +type/vector+ "not a vector.")

  ;; TODO: array element types of T allo storage of ANYTHING into the
  ;; vector. If the type is more specific, like :integer, then you can store
  ;; only those types. If the type is really specific, like int8, then
  ;; the contents of each cell might hold the actual value, as opposed to
  ;; a value of it. I need to figure this out.

  ;; check that the elem I'm setting is of the vector element-type.
  (//check-type val-addr (//array-element-type addr) "elem is not right type.")

  ;; we're storing the address of val into the vector location.
  (//stm (+ addr +lo/vector/data+ (//value idx)) val-addr))

;; Only works for :integer for now, not generalized.
(defun //incf (addr amt-addr)
  (//check-type addr +type/integer+ "not an integer.")
  (//stm (+ addr +lo/atom/value+)
         (+ (//value addr) (//value amt-addr)))
  addr)

;; Only works for :integer for now, not generalized.
(defun //decf (addr amt-addr)
  (//check-type addr +type/integer+ "not an integer.")
  (//stm (+ addr +lo/atom/value+)
         (- (//value addr) (//value amt-addr)))
  addr)


(defun //length (addr)
  (cond
    ((eql (//type-of addr) +type/vector+)
     (//vint (//ldm (+ addr +lo/vector/length+))))
    (t
     (error "//length() wrong type, sorry"))))

;; a small crutch for me to shorten typing...
(defun //list (&rest args)
  (if (null args)
      (//vnil)
      (//cons (car args) (apply #'//list (cdr args)))))


(defun //init ()
  ;; This needs more work to set up the variable bindings, symbol internship,
  ;; initial environment.
  (//init-mem))


(defun //unparse (addr)
  (let ((*print-right-margin* (* 1024 1024))
        (*print-miser-width* (* 1024 1024))
        (*print-length* (* 1024 1024))
        (*print-level* nil)
        (*print-circle* t)
        (*print-lines* nil))
    (labels ((%unparse (addr)
               (cond
                 ((eql (//type-of addr) +type/integer+)
                  (format t "~A" (//value addr)))

                 ((eql (//type-of addr) +type/string+)
                  (format t "~A" (//value addr)))

                 ((eql (//type-of addr) +type/cons+)
                  (format t "(")
                  (%unparse (//car addr))
                  (format t " . ")
                  (%unparse (//cdr addr))
                  (format t ")"))

                 ((eql (//type-of addr) +type/vector+)
                  (format t "#(")
                  (dotimes (idx (//value (//length addr))) ;; lifted into CL
                    (format t "~A " (//value (//svref addr (//vint idx)))))
                  (format t ")"))

                 ((eql (//type-of addr) +type/nil+)
                  (format t "NIL"))

                 ((eql (//type-of addr) +type/t+)
                  (format t "T"))

                 (t
                  (format t "Unknown addr!~%"))

                 )))

      (%unparse addr))))
