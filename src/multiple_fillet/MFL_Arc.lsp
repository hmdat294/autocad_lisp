
;; ------------------------------------------------------------
;; TAO CUNG BAN NGUYET
;; ------------------------------------------------------------

(defun MFL-MAKE-ARC (p1 p2 pick / cen rad a1 amid vx vy dotp st en) 
  (setq cen (mapcar '(lambda (a b) (/ (+ a b) 2.0)) p1 p2)
        rad (/ (distance p1 p2) 2.0)
  )

  (if (> rad 1e-8) 
    (progn 
      (setq a1   (angle cen p1)
            amid (+ a1 (/ pi 2.0))
            vx   (- (car pick) (car cen))
            vy   (- (cadr pick) (cadr cen))
            dotp (+ (* (cos amid) vx) 
                    (* (sin amid) vy)
                 )
      )

      (if (>= dotp 0.0) 
        (setq st a1)
        (setq st (angle cen p2))
      )

      (setq en (rem (+ st pi) (* 2.0 pi)))

      (entmakex 
        (list 
          '(0 . "ARC")
          '(100 . "AcDbEntity")
          '(100 . "AcDbCircle")
          (cons 10 cen)
          (cons 40 rad)
          '(100 . "AcDbArc")
          (cons 50 st)
          (cons 51 en)
        )
      )
    )
  )
)


(defun MFL-ARC-FOR-PAIR (e1 e2 groupcen / ends p1 p2 cen m1 m2 vx vy pick) 

  (setq ends (MFL-NEAREST-ENDS e1 e2)
        p1   (car ends)
        p2   (cadr ends)
        cen  (mapcar '(lambda (a b) (/ (+ a b) 2.0)) p1 p2)
        m1   (MFL-MID e1)
        m2   (MFL-MID e2)
  )

  ;; Xac dinh huong tu nhom 1 sang nhom 2
  (setq vx (- (car m2) (car m1))
        vy (- (cadr m2) (cadr m1))
  )

  ;; Chon phia cung theo truc hinh hoc, khong theo thu tu quet
  (if (> (+ (* vx vx) (* vy vy)) 1e-12) 
    (setq pick (list (+ (car cen) vx) 
                     (+ (cadr cen) vy)
                     (caddr cen)
               )
    )
    (setq pick (list 
                 (- (car cen) (- (cadr p2) (cadr p1)))
                 (+ (cadr cen) (- (car p2) (car p1)))
                 (caddr cen)
               )
    )
  )

  (MFL-MAKE-ARC p1 p2 pick)
)


