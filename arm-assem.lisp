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
    size
    align
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

  (defun %generate-machine-code (bit-size align ann &rest chunks)
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
         (when (/= ,gen/bit-count ,bit-size)
           (error "n-bits: bit-count should be ~A bits, but is ~A bits!"
                  ,bit-size ,gen/bit-count))

         ;; check that were setting n continugous bits starting from 0.
         (let ((bit-magnitude 0))
           (loop for (maxb minb val) in ,gen/rbcl-var do
                (loop for b from minb to maxb do
                     (setf bit-magnitude
                           (logior bit-magnitude (ash 1 b)))))

           (when (/= bit-magnitude (1- (expt 2 ,bit-size)))
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
         (make-mcode :val ,gen/merge :size ,bit-size :align ,align
                     :ann ,ann)))))

;; Generated machine is only a number, bit-size, byte alignment, and annotation.
(defmacro gmc16-1 (ann &body chunks)
  (apply #'%generate-machine-code 16 2 ann chunks))


(defmacro gmc16-2 (ann (&body chunks-high) (&body chunks-low))
  (let ((high (apply #'%generate-machine-code 16 2 NIL chunks-high))
        (low (apply #'%generate-machine-code 16 2 NIL chunks-low))
        (h (gensym))
        (l (gensym)))
    `(let ((,h ,high)
           (,l ,low))
       ;; Then assemble the 32-bit thumb instruction, but leave the alignment
       ;; such that we can place it properly.
       (make-mcode :val (logior (ash (mcode-val ,h) 16) (mcode-val ,l))
                   :size 32
                   :align 2
                   :ann ,ann))))

(defmacro gmc32 (ann &body chunks)
  (apply #'%generate-machine-code 32 4 ann chunks))


(defun add-imm-thumb (<Rn> <Rd> <imm3>)
  (gmc16-1 nil
    (15 13 #b000)
    (12 11 #b11)
    (10 10 #b1)
    (9 9 #b0)
    (8 6 <imm3>)
    (5 3 <Rn>)
    (2 0 <Rd>)))

(defun add-imm-thumb-2 (ann i s <Rn> <Rd> <imm3> <imm8>)
  (gmc16-2 ann
           ((15 11 #b11110)
            (10 10 i)
            (9 9 #b0)
            (8 5 #b1000)
            (4 4 s)
            (3 0 <Rn>))

           ((15 15 #b0)
            (14 12 <imm3>)
            (11 8 <Rd>)
            (7 0 <imm8>))))

(defun add-imm-arm (s <c> <Rd> <Rn> <const>)
  (gmc32 nil
    (31 28 <c>)
    (27 26 #b00)
    (25 25 #b1)
    (24 21 #b0100)
    (20 20 s)
    (19 16 <Rn>)
    (15 12 <Rd>)
    (11 0 <const>)))
