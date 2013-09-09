(in-package #:json-streams)

;;; From
;;; Printing Floating-Point Numbers Quickly and Accurately
;;; by Robert G. Burger and R. Kent Dybvig

(defun flonum->digits (v f e min-e p b BB)
  (let ((roundp (evenp f)))
    (if (>= e 0)
        (if (not (= f (expt b (- p 1))))
            (let ((be (expt b e)))
              (scale (* f be 2) 2 be be 0 BB roundp roundp v))
            (let* ((be (expt b e))
                   (be1 (* be b)))
              (scale (* f be1 2) (* b 2) be1 be 0 BB roundp roundp v)))
        (if (or (= e min-e) (not (= f (expt b (- p 1)))))
            (scale (* f 2) (* (expt b (- e)) 2) 1 1 0 BB roundp roundp v)
            (scale (* f b 2) (* (expt b (- 1 e)) 2) b 1 0 BB roundp roundp v)))))

#|
(defun scale (r s m+ m- k BB low-ok-p high-ok-p)
  (cond
    ((funcall (if high-ok-p #'>= #'>) (+ r m+) s) ; k is too low
     (scale r (* s BB) m+ m- (+ k 1) BB low-ok-p high-ok-p))
    ((funcall (if high-ok-p #'< #'<=) (* (+ r m+)) BB s) ; k is too high
     (scale (* r BB) s (* m+ BB) (* m- BB) (- k 1) BB low-ok-p high-ok-p))
    (t ; k is correct
     (cons k (generate r s m+ m- BB low-ok-p high-ok-p)))))
|#

(defun generate (r s m+ m- BB low-ok-p high-ok-p)
  (multiple-value-bind (d r)
      (truncate (* r BB) s)
    (let ((m+ (* m+ BB))
          (m- (* m- BB)))
      (let ((tc1 (funcall (if low-ok-p #'<= #'<) r m-))
            (tc2 (funcall (if high-ok-p #'>= #'>) (+ r m+) s)))
        (if (not tc1)
            (if (not tc2)
                (cons d (generate r s m+ m- BB low-ok-p high-ok-p))
                (list (+ d 1)))
            (if (not tc2)
                (list d)
                (if (< (* r 2) s)
                    (list d)
                    (list (+ d 1)))))))))

(defun scale (r s m+ m- k BB low-ok-p high-ok-p v)
  (let ((est (ceiling (- (logB BB v) 1d-10))))
    (if (>= est 0)
        (fixup r (* s (exptt BB est)) m+ m- est BB low-ok-p high-ok-p)
        (let ((scale (exptt BB (- est))))
          (fixup (* r scale) s (* m+ scale) (* m- scale) est BB low-ok-p high-ok-p)))))

(defun fixup (r s m+ m- k BB low-ok-p high-ok-p)
  (if (funcall (if high-ok-p #'>= #'>) (+ r m+) s) ; too low?
      (cons (+ k 1) (generate r (* s BB) m+ m- BB low-ok-p high-ok-p))
      (cons k (generate r s m+ m- BB low-ok-p high-ok-p))))

(let ((table (make-array 326)))
  (do ((k 0 (+ k 1)) (v 1 (* v 10)))
      ((= k 326))
    (setf (svref table k) v))
  (defun exptt (BB k)
    (if (and (= BB 10) (<= 0 k 325))
        (svref table k)
        (expt BB k))))

(let ((table (make-array 37)))
  (do ((BB 2 (+ BB 1)))
      ((= BB 37))
    (setf (svref table BB) (/ (log BB))))
  (defun logB (BB x)
    (if (<= 2 BB 36)
        (* (log x) (svref table BB))
        (/ (log x) (log BB)))))

(defun write-float (float stream)
  (multiple-value-bind (f e s)
      (integer-decode-float float)
    (let ((digits (flonum->digits (abs float) f e -1022 53 2 10)))
      (when (minusp s)
        (princ "-" stream))
      (format stream "0.~{~D~}E~D" (rest digits) (first digits)))))
