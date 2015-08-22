(defpackage #:six-dead-mice
  (:use #:cl)
  (:export
   #:six-dead-mice))

;; package holding Language A experiment
(defpackage #:lang-a
  (:use #:cl))

;; package holding Language B experiment
(defpackage #:lang-b
  (:use #:cl))

;; package holding Language C experiment
(defpackage #:lang-c
  (:use #:cl))

;; package holding Language D experiment
(defpackage #:lang-d
  (:use #:cl))
