
;; Tao cung ban nguyet tu p1 den p2 ve phia pick
(defun MFL-MAKE-ARC (p1 p2 pick / cen rad a1 amid vx vy dotp st en)
  (setq cen (mapcar '(lambda (a b) (/ (+ a b) 2.0)) p1 p2)
        rad (/ (distance p1 p2) 2.0))
  (if (> rad 1e-8)
    (progn
      (setq a1 (angle cen p1)
            amid (+ a1 (/ pi 2.0))
            vx (- (car pick) (car cen))
            vy (- (cadr pick) (cadr cen))
            dotp (+ (* (cos amid) vx) (* (sin amid) vy)))
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

;; Tao cung cho cap LINE song song.
;; highSide=T: cung ve phia Y lon; nil: cung ve phia Y nho.
(defun MFL-ARC-FOR-PAIR (e1 e2 highSide / ends p1 p2 tmp cen dx dy nx ny pick)
  (setq ends (MFL-NEAREST-ENDS e1 e2 highSide)
        p1 (car ends)
        p2 (cadr ends))

  ;; Chuan hoa thu tu diem de ket qua ARC khong phu thuoc nhom nao duoc chon truoc.
  (if (or (> (car p1) (car p2))
          (and (equal (car p1) (car p2) 1e-8)
               (> (cadr p1) (cadr p2))))
    (setq tmp p1 p1 p2 p2 tmp))

  (setq cen (mapcar '(lambda (a b) (/ (+ a b) 2.0)) p1 p2)
        dx (- (car p2) (car p1))
        dy (- (cadr p2) (cadr p1))
        nx (- dy)
        ny dx)

  ;; Chon phia cung theo thu tu chon nhom:
  ;; nhom 1 ben trai nhom 2 -> Y lon; nguoc lai -> Y nho.
  ;; Neu vector phap tuyen gan song song truc X (ny ~ 0), giu huong X duong.
  (cond
    (highSide
      (if (or (< ny -1e-10)
              (and (<= (abs ny) 1e-10) (< nx 0.0)))
        (setq nx (- nx) ny (- ny))))
    (T
      (if (or (> ny 1e-10)
              (and (<= (abs ny) 1e-10) (< nx 0.0)))
        (setq nx (- nx) ny (- ny)))))

  (setq pick (list (+ (car cen) nx)
                   (+ (cadr cen) ny)
                   (caddr cen)))
  (MFL-MAKE-ARC p1 p2 pick)
)