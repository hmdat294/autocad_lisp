;; ============================================================
;; MFL - MULTI FILLET
;; - Chon LINE va LWPOLYLINE
;; - Tu EXPLODE LWPOLYLINE thanh cac doi tuong rieng
;; - Nhom vuong goc: ghep cap toi uu theo dau mut
;; - Giam R theo do lech hang cua ca hai nhom
;; - Nhom song song: tao cung ban nguyet
;; ============================================================

(defun MFL-PT2 (p) 
  (list (car p) (cadr p) (if (caddr p) (caddr p) 0.0))
)

;; ------------------------------------------------------------
;; Lay hai dau mut LINE
;; ------------------------------------------------------------

(defun MFL-ENDS (en / ed) 
  (setq ed (entget en))
  (list 
    (MFL-PT2 (cdr (assoc 10 ed)))
    (MFL-PT2 (cdr (assoc 11 ed)))
  )
)

(defun MFL-MID (en / pts) 
  (setq pts (MFL-ENDS en))
  (mapcar 
    '(lambda (a b) (/ (+ a b) 2.0))
    (car pts)
    (cadr pts)
  )
)

(defun MFL-Y (en) 
  (cadr (MFL-MID en))
)

(defun MFL-DIR (en / ed a b) 
  (setq ed (entget en)
        a  (cdr (assoc 10 ed))
        b  (cdr (assoc 11 ed))
  )
  (angle a b)
)

(defun MFL-PARALLEL (a b) 
  (< (abs (sin (- a b))) 1e-6)
)

(defun MFL-PERP (a b) 
  (< (abs (cos (- a b))) 1e-6)
)

(defun MFL-SORT-Y (lst) 
  (vl-sort lst 
           '(lambda (a b) (> (MFL-Y a) (MFL-Y b)))
  )
)

(defun MFL-SS-LIST (ss / i lst) 
  (setq i   0
        lst nil
  )
  (repeat (sslength ss) 
    (if (= (cdr (assoc 0 (entget (ssname ss i)))) "LINE") 
      (setq lst (cons (ssname ss i) lst))
    )
    (setq i (1+ i))
  )
  (MFL-SORT-Y lst)
)

;; ------------------------------------------------------------
;; KHOANG CACH NHO NHAT GIUA DAU MUT HAI LINE
;; ------------------------------------------------------------

(defun MFL-END-DIST (e1 e2 / a b p q d best) 
  (setq a    (MFL-ENDS e1)
        b    (MFL-ENDS e2)
        best nil
  )

  (foreach p a 
    (foreach q b 
      (setq d (distance p q))
      (if (or (null best) (< d best)) 
        (setq best d)
      )
    )
  )
  best
)


;; ------------------------------------------------------------
;; TIM HAI DAU MUT GAN NHAU NHAT
;; ------------------------------------------------------------



(defun MFL-NEAREST-ENDS (e1 e2 / a b p q d best bestp bestq)
  (setq a (MFL-ENDS e1)
        b (MFL-ENDS e2)
        best nil)

  (foreach p a
    (foreach q b
      (setq d (distance p q))
      (if (or (null best) (< d best))
        (setq best d
              bestp p
              bestq q)
      )
    )
  )

  (list bestp bestq)
)


