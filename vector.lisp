(in-package :cl-user)

(defpackage :vector
  (:use :common-lisp)
  (:export :vlen
           :vlen3
           :scalevec
           :dot
           :cross
           :posvec
           :norm
           :addvec))

(in-package :vector)

(defun vlen (&rest r)
  (labels ((rec (x acc)
             (cond ((null x) acc)
                   ((numberp x) (+ acc (* x x)))
                   (t (rec (cdr x) (+ acc (* (car x) (car x))))))))
    (sqrt (rec r 0))))

(defun vlen3 (v)
  (destructuring-bind (x y z) v
    (vlen x y z)))

(defun scalevec (s v)
  (loop for i in v
       collect (* s i)))

(defun posvec (p1 p2)
  (loop for a in p1
     for b in p2
     collect (- b a)))

(defun dot (a b)
  (labels ((rec (v1 v2 acc)
             (cond ((equal 1 (length v1)) (+ acc (* (car v1) (car v2))))
                   (t (rec (cdr v1) (cdr v2) (+ acc (* (car v1) (car v2))))))))
    (rec a b 0)))

(defun cross (u v)
  (list
       (- (* (nth 1 u) (nth 2 v)) (* (nth 2 u) (nth 1 v)))
       (- (* (nth 2 u) (nth 0 v)) (* (nth 0 u) (nth 2 v)))
       (- (* (nth 0 u) (nth 1 v)) (* (nth 1 u) (nth 0 v)))))

(defun norm (v)
  (let ((vl (vlen3 v)))
    (list (/ (nth 0 v) vl)
          (/ (nth 1 v) vl)
          (/ (nth 2 v) vl))))

(defun addvec (v1 v2)
  (loop for x in v1
        for y in v2
       collect (+ x y)))
