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
;; CHUYEN TAP CHON THANH DANH SACH LINE
;; LINE: giu nguyen
;; LWPOLYLINE: tu EXPLODE va thu thap cac LINE moi tao
;; ------------------------------------------------------------

(defun MFL-EXPAND-SS (ss / i en typ result before after cur out) 

  (setq i   0
        out nil
  )

  (repeat (sslength ss) 
    (setq en  (ssname ss i)
          typ (cdr (assoc 0 (entget en)))
    )

    (cond 
      ;; LINE co san
      ((= typ "LINE")
       (setq out (cons en out))
      )

      ;; Polyline: tu dong EXPLODE
      ((= typ "LWPOLYLINE")
       (setq before (entlast))

       (setq result (vl-catch-all-apply 
                      'vl-cmdf
                      (list "_.EXPLODE" en "")
                    )
       )

       (if (not (vl-catch-all-error-p result)) 
         (progn 
           (setq after (entlast))

           ;; Chi doc cac entity moi duoc tao
           (if after 
             (progn 
               (setq cur (if before 
                           (entnext before)
                           (entnext nil)
                         )
               )

               (while cur 
                 (if (= (cdr (assoc 0 (entget cur))) "LINE") 
                   (setq out (cons cur out))
                 )

                 (if (eq cur after) 
                   (setq cur nil)
                   (setq cur (entnext cur))
                 )
               )
             )
           )
         )
       )
      )
    )

    (setq i (1+ i))
  )

  (MFL-SORT-Y (reverse out))
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
;; GHEP CAP TOI UU - GIU THUAT TOAN HIEN TAI
;; ------------------------------------------------------------

(defun MFL-PAIR-OPTIMAL (l1 l2 / bestPairs) 
  (setq bestPairs nil)

  (if (= (length l1) (length l2)) 
    (progn 
      (setq *MFL-BEST-COST*  nil
            *MFL-BEST-PAIRS* nil
      )

      (MFL-PAIR-SEARCH l1 l2 nil 0.0)
      (setq bestPairs *MFL-BEST-PAIRS*)
    )
  )

  bestPairs
)

(defun MFL-PAIR-SEARCH (l1 l2 current cost / e1 e2 d) 
  (if (null l1) 
    (if 
      (or (null *MFL-BEST-COST*) 
          (< cost *MFL-BEST-COST*)
      )
      (setq *MFL-BEST-COST*  cost
            *MFL-BEST-PAIRS* (reverse current)
      )
    )
    (progn 
      (setq e1 (car l1))
      (foreach e2 l2 
        (setq d (MFL-END-DIST e1 e2))
        (if 
          (or (null *MFL-BEST-COST*) 
              (<= (+ cost d) *MFL-BEST-COST*)
          )
          (MFL-PAIR-SEARCH 
            (cdr l1)
            (vl-remove e2 l2)
            (cons (list e1 e2) current)
            (+ cost d)
          )
        )
      )
    )
  )
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


;; ------------------------------------------------------------
;; LENH CHINH MFL
;; ------------------------------------------------------------

(defun c:MFL (/ *error* ss1 ss2 l1 l2 dir1 dir2 mode pairs pair e1 e2 r rr spacing 
              ybase1 ybase2 result i groupcen oldrad
             ) 

  (defun *error* (msg) 
    (if oldrad 
      (setvar "FILLETRAD" oldrad)
    )
    (if 
      (and msg 
           (not (wcmatch (strcase msg) "*CANCEL*,*QUIT*,*BREAK*"))
      )
      (princ (strcat "\nMFL loi: " msg))
    )
    (princ)
  )

  (setq oldrad (getvar "FILLETRAD"))

  ;; Cho nhom 1
  (princ "\nQuet chon nhom thu nhat (LINE / LWPOLYLINE): ")
  (setq ss1 (ssget '((0 . "LINE,LWPOLYLINE"))))

  (cond 
    ((null ss1)
     (princ "\nKhong co doi tuong nao duoc chon.")
    )

    (T
     ;; Cho nhom 2
     (princ "\nQuet chon nhom thu hai (LINE / LWPOLYLINE): ")
     (setq ss2 (ssget '((0 . "LINE,LWPOLYLINE"))))

     (cond 
       ((null ss2)
        (princ "\nKhong co doi tuong nao duoc chon.")
       )

       (T
        ;; Tu dong tach Polyline thanh cac LINE
        (setq l1 (MFL-EXPAND-SS ss1)
              l2 (MFL-EXPAND-SS ss2)
        )

        (cond 
          ((or (null l1) (null l2))
           (princ 
             "\nKhong tim thay LINE hop le sau khi xu ly Polyline."
           )
          )

          ((/= (length l1) (length l2))
           (princ 
             (strcat 
               "\nHai nhom phai co cung so luong LINE sau khi tach."
               "\nNhom 1: "
               (itoa (length l1))
               " LINE; nhom 2: "
               (itoa (length l2))
               " LINE."
             )
           )
          )

          (T
           (setq dir1 (MFL-DIR (car l1))
                 dir2 (MFL-DIR (car l2))
                 mode nil
           )

           (cond 
             ((MFL-PARALLEL dir1 dir2)
              (setq mode 'parallel)
              (princ "\nNhan dien hai nhom song song.")
             )

             ((MFL-PERP dir1 dir2)
              (setq mode 'perp)
              (princ "\nNhan dien hai nhom vuong goc.")
             )

             (T
              (princ 
                "\nHai nhom khong song song hoac vuong goc."
              )
             )
           )

           (if mode 
             (progn 

               ;; ============================================
               ;; NHOM SONG SONG
               ;; ============================================

               (if (eq mode 'parallel) 
                 (progn 
                   (setq pairs nil)

                   (while (and l1 l2) 
                     (setq pairs (cons (list (car l1) (car l2)) pairs)
                           l1    (cdr l1)
                           l2    (cdr l2)
                     )
                   )

                   (setq pairs (reverse pairs))

                   (if pairs 
                     (progn 
                       (setq groupcen (mapcar 
                                        '(lambda (a b) (/ (+ a b) 2.0))
                                        (MFL-MID (caar pairs))
                                        (MFL-MID (cadar pairs))
                                      )
                       )

                       (setq i 0)

                       (foreach pair pairs 
                         (setq i      (1+ i)
                               e1     (car pair)
                               e2     (cadr pair)
                               result (vl-catch-all-apply 
                                        'MFL-ARC-FOR-PAIR
                                        (list e1 e2 groupcen)
                                      )
                         )

                         (if 
                           (or 
                             (vl-catch-all-error-p result)
                             (null result)
                           )
                           (princ 
                             (strcat 
                               "\nCap "
                               (itoa i)
                               ": khong tao duoc cung."
                             )
                           )
                           (princ 
                             (strcat 
                               "\nCap "
                               (itoa i)
                               ": da tao cung."
                             )
                           )
                         )
                       )
                     )
                     (princ "\nKhong tao duoc cap LINE.")
                   )
                 )

                 ;; ==========================================
                 ;; NHOM VUONG GOC
                 ;; ==========================================

                 (progn 
                   (setq r (getdist "\nNhap ban kinh R co so <0>: "))

                   ;; Enter = R 0
                   (if (null r) 
                     (setq r 0.0)
                   )

                   (if (>= r 0.0) 
                     (progn 
                       (setq pairs (MFL-PAIR-OPTIMAL l1 l2))

                       (if pairs 
                         (progn 
                           ;; Moc rieng cho moi nhom
                           (setq ybase1 (MFL-Y (caar pairs))
                                 ybase2 (MFL-Y (cadar pairs))
                                 i      0
                           )

                           (foreach pair pairs 
                             (setq i  (1+ i)
                                   e1 (car pair)
                                   e2 (cadr pair)
                             )

                             ;; Do lech cua hai nhom
                             (setq spacing (max 
                                             (abs (- (MFL-Y e1) ybase1))
                                             (abs (- (MFL-Y e2) ybase2))
                                           )
                             )

                             ;; Giam ban kinh theo do lech
                             (setq rr (max 0.0 (- r spacing)))

                             (princ 
                               (strcat 
                                 "\nCap "
                                 (itoa i)
                                 " | Lech nhom 1="
                                 (rtos 
                                   (abs (- (MFL-Y e1) ybase1))
                                   2
                                   3
                                 )
                                 " | Lech nhom 2="
                                 (rtos 
                                   (abs (- (MFL-Y e2) ybase2))
                                   2
                                   3
                                 )
                                 " | R="
                                 (rtos rr 2 3)
                               )
                             )

                             ;; Dat ban kinh FILLET
                             (setq result (vl-catch-all-apply 
                                            'setvar
                                            (list "FILLETRAD" rr)
                                          )
                             )

                             (if (not (vl-catch-all-error-p result)) 
                               (progn 
                                 (setq result (vl-catch-all-apply 
                                                'vl-cmdf
                                                (list "_.FILLET" e1 e2)
                                              )
                                 )

                                 (if (vl-catch-all-error-p result) 
                                   (princ "\nFILLET cap nay that bai.")
                                   (princ "\nDa xu ly cap.")
                                 )
                               )
                               (princ "\nKhong dat duoc ban kinh FILLET.")
                             )
                           )
                         )
                         (princ "\nKhong tim duoc cap LINE phu hop.")
                       )
                     )
                     (princ "\nBan kinh khong hop le.")
                   )
                 )
               )

               (princ "\nMFL hoan tat.")
             )
           )
          )
        )
       )
     )
    )
  )

  ;; Khoi phuc ban kinh ban dau
  (if oldrad 
    (setvar "FILLETRAD" oldrad)
  )

  (princ)
)
