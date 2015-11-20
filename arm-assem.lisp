(in-package :arm-assem)

(defmacro destructuring-bits (bindings &body body)
  `(progn ,@body))

(defstruct instdef
  opcode-prefix
  kind
  mode
  syntax-analyzer
  encodings)

(eval-when (:compile-toplevel :load-toplevel :execute)

  (defstruct mcode ;; a single raw machine code instruction
    val
    width
    ann)

  ;; From LOL
  (defun group (lst n)
    (when (zerop n) (error "A zero group size is illegal"))
    (labels ((rec (lst acc)
               (let ((rst (nthcdr n lst)))
                 (if (consp rst)
                     (rec rst (cons (subseq lst 0 n)
                                    acc))
                     (nreverse
                      (cons lst acc))))))
      (if lst (rec lst nil) nil)))


  (defmacro bits (max &optional to (min max))
    (declare (ignore to))
    `(byte (- ,max ,min -1) ,min))

  (defun ensure-val-fits-in-bit-window (maxb minb val)
    (let ((max-bits-required (1+ (- maxb minb)))
          (val-bits-discovered (integer-length val)))
      (if (> val-bits-discovered max-bits-required)
          (error "ensure-val-fits-in-bit-window: val ~A (bits found ~A) is too big for a bit window of ~A bits" val val-bits-discovered max-bits-required)
          T)))

  ;; generate a form which will validate that the values indeed fit into the
  ;; bit windows.
  (defun bit-window-validation (&rest chunks)
    (loop for (maxb minb val) in chunks collecting
         `(ensure-val-fits-in-bit-window ,maxb ,minb ,val)))

  ;; Convert the low level instruction specification form into a lisp
  ;; form which actually computes it. This does no error checking.
  (defun bit-merge (&rest chunks)
    (cond
      ((null chunks)
       (list))
      ((null (cdr chunks))
       ;; one left, and we're processing it
       (destructuring-bind (maxbit minbit val) (car chunks)
         `(ldb (bits ,maxbit 'to ,minbit) ,val)))
      (t
       ;; processing something before the end.
       (destructuring-bind (maxbit minbit val) (car chunks)
         `(dpb ,val (bits ,maxbit 'to ,minbit)
               ,(apply #'bit-merge (cdr chunks)))))))

  (defun %n-bits (n &rest chunks)
    (let* ((gen/bindings (loop for (maxb minb val) in chunks appending
                              (list `(,(gensym) ,maxb)
                                    `(,(gensym) ,minb)
                                    `(,(gensym) ,val))))
           (gen/bindings-grouped (group (mapcar #'first gen/bindings) 3))
           (gen/new-chunks-list-vars
            `((list
               ,@(mapcar (lambda (f) `(list ,@f)) gen/bindings-grouped))))
           (gen/rbcl-var (gensym))
           (gen/bit-count (gensym))

           (gen/bwv (apply #'bit-window-validation gen/bindings-grouped))
           (gen/merge (apply #'bit-merge gen/bindings-grouped)))

      `(let* (,@gen/bindings
              (,gen/rbcl-var ,@gen/new-chunks-list-vars)
              (,gen/bit-count
               (reduce #'+ (mapcar
                            (lambda (c)
                              (destructuring-bind (maxb minb val) c
                                (declare (ignore val))
                                (1+ (- maxb minb))))
                            ,gen/rbcl-var))))
         ;; check that we're setting n bits total.
         (when (/= ,gen/bit-count ,n)
           (error "n-bits: bit-count should be ~A bits, but is ~A bits!"
                  ,n ,gen/bit-count))

         ;; check that were setting n continugous bits starting from 0.
         (let ((bit-magnitude 0))
           (loop for (maxb minb val) in ,gen/rbcl-var do
                (loop for b from minb to maxb do
                     (setf bit-magnitude
                           (logior bit-magnitude (ash 1 b)))))

           (when (/= bit-magnitude (1- (expt 2 ,n)))
             (error "n-bits: There is a bithole in the encoding: ~2R"
                    bit-magnitude)))

         ;; finally now that we've checked that we're setting n
         ;; different bits continguously into an integer
         ;; starting from the 0 bit position, we construct the
         ;; form which both checks that the integers we are
         ;; assembling are valid and then also creates the final
         ;; integer by assembling the values together as
         ;; specified by the bit positions.
         ,@gen/bwv
         (make-mcode :val ,gen/merge :width ,n :ann NIL)))))

(defmacro 16-bits (&rest chunks)
  (apply #'%n-bits 16 chunks))

(defmacro 32-bits (&rest chunks)
  (apply #'%n-bits 32 chunks))

(defun add-imm-thumb (<Rn> <Rd> <imm3>)
  (16-bits (15 13 #b000) ;; bit end, bit start, value
           (12 11 #b11)
           (10 10 #b1)
           (9 9 #b0)
           (8 6 <imm3>)
           (5 3 <Rn>)
           (2 0 <Rd>)))
