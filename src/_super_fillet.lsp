;; ============================================================
;; SPMFL - MULTI FILLET
;; - Chon LINE va LWPOLYLINE
;; - Tu EXPLODE LWPOLYLINE thanh cac LINE
;; - Nhom song song: sap xep cum 1, DAO NGUOC cum 2 roi ghep cap
;; - Huong cung phu thuoc thu tu chon: trai->phai ve Y lon; phai->trai ve Y nho
;; - Nhom vuong goc: ghep cap toi uu theo dau mut
;; - Giam R theo do lech hang cua ca hai nhom
;; ============================================================

(vl-load-com)

(defun MFL-PT2 (p)
  (list (car p) (cadr p) (if (caddr p) (caddr p) 0.0))
)

(defun MFL-ENDS (en / ed)
  (setq ed (entget en))
  (list
    (MFL-PT2 (cdr (assoc 10 ed)))
    (MFL-PT2 (cdr (assoc 11 ed)))
  )
)

(defun MFL-MID (en / pts)
  (setq pts (MFL-ENDS en))
  (mapcar '(lambda (a b) (/ (+ a b) 2.0)) (car pts) (cadr pts))
)

(defun MFL-Y (en)
  (cadr (MFL-MID en))
)

(defun MFL-X (en)
  (car (MFL-MID en))
)

(defun MFL-DIR (en / ed a b)
  (setq ed (entget en)
        a (cdr (assoc 10 ed))
        b (cdr (assoc 11 ed)))
  (angle a b)
)

(defun MFL-PARALLEL (a b)
  (< (abs (sin (- a b))) 1e-6)
)

(defun MFL-PERP (a b)
  (< (abs (cos (- a b))) 1e-6)
)

;; Sap xep LINE theo trung diem: Y giam dan, neu bang Y thi X tang dan
(defun MFL-SORT-Y (lst)
  (vl-sort lst
    '(lambda (a b)
       (if (equal (MFL-Y a) (MFL-Y b) 1e-8)
         (< (MFL-X a) (MFL-X b))
         (> (MFL-Y a) (MFL-Y b))
       )
     )
  )
)

;; ------------------------------------------------------------
;; SAP XEP CAC LINE SONG SONG THEO PHUONG VUONG GOC VOI LINE
;; Vi du: LINE doc -> sap theo X (trai/phai)
;;        LINE ngang -> sap theo Y (duoi/tren)
;; Khong phu thuoc thu tu chon nhom 1/nhom 2.
;; ------------------------------------------------------------

(defun MFL-CANON-DIR (en / pts dx dy ln)
  (setq pts (MFL-ENDS en)
        dx (- (car (cadr pts)) (car (car pts)))
        dy (- (cadr (cadr pts)) (cadr (car pts)))
        ln (sqrt (+ (* dx dx) (* dy dy))))
  (if (> ln 1e-12)
    (progn
      (setq dx (/ dx ln) dy (/ dy ln))
      ;; Chuan hoa huong: X duong; neu X=0 thi Y duong
      (if (or (< dx -1e-10)
              (and (<= (abs dx) 1e-10) (< dy 0.0)))
        (setq dx (- dx) dy (- dy)))
      (list dx dy)
    )
    (list 1.0 0.0)
  )
)

(defun MFL-PARALLEL-LESS (a b / ma mb pa pb qa qb ha hb)
  (setq ma (MFL-MID a)
        mb (MFL-MID b)
        ;; *MFL-NX*/*MFL-NY*: truc sap xep vuong goc voi LINE
        ;; *MFL-UX*/*MFL-UY*: truc doc theo LINE de pha hoa neu trung
        pa (+ (* (car ma) *MFL-NX*) (* (cadr ma) *MFL-NY*))
        pb (+ (* (car mb) *MFL-NX*) (* (cadr mb) *MFL-NY*))
        qa (+ (* (car ma) *MFL-UX*) (* (cadr ma) *MFL-UY*))
        qb (+ (* (car mb) *MFL-UX*) (* (cadr mb) *MFL-UY*))
        ha (cdr (assoc 5 (entget a)))
        hb (cdr (assoc 5 (entget b))))
  (cond
    ((< pa (- pb 1e-8)) T)
    ((> pa (+ pb 1e-8)) nil)
    ((< qa (- qb 1e-8)) T)
    ((> qa (+ qb 1e-8)) nil)
    ((and ha hb) (vl-string-lessp ha hb))
    (T nil)
  )
)

(defun MFL-SORT-PARALLEL (lst ref / uv)
  (setq uv (MFL-CANON-DIR ref)
        *MFL-UX* (car uv)
        *MFL-UY* (cadr uv)
        *MFL-NX* (- (cadr uv))
        *MFL-NY* (car uv))
  (vl-sort lst '(lambda (a b) (MFL-PARALLEL-LESS a b)))
)

(defun MFL-SS-LIST (ss / i lst)
  (setq i 0 lst nil)
  (repeat (sslength ss)
    (if (= (cdr (assoc 0 (entget (ssname ss i)))) "LINE")
      (setq lst (cons (ssname ss i) lst))
    )
    (setq i (1+ i))
  )
  (MFL-SORT-Y lst)
)

;; Chuyen tap chon thanh danh sach LINE; LWPOLYLINE se duoc EXPLODE
(defun MFL-EXPAND-SS (ss / i en typ result before after cur out)
  (setq i 0 out nil)
  (repeat (sslength ss)
    (setq en (ssname ss i)
          typ (cdr (assoc 0 (entget en))))
    (cond
      ((= typ "LINE")
       (setq out (cons en out))
      )
      ((= typ "LWPOLYLINE")
       (setq before (entlast))
       (setq result
         (vl-catch-all-apply 'vl-cmdf (list "_.EXPLODE" en ""))
       )
       (if (not (vl-catch-all-error-p result))
         (progn
           (setq after (entlast))
           (if after
             (progn
               (setq cur (if before (entnext before) (entnext nil)))
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

;; Khoang cach nho nhat giua bat ky hai dau mut cua hai LINE
(defun MFL-END-DIST (e1 e2 / a b p q d best)
  (setq a (MFL-ENDS e1)
        b (MFL-ENDS e2)
        best nil)
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

;; Tim cap LINE toi uu cho truong hop vuong goc
(defun MFL-PAIR-OPTIMAL (l1 l2 / bestPairs)
  (setq bestPairs nil)
  (if (= (length l1) (length l2))
    (progn
      (setq *MFL-BEST-COST* nil
            *MFL-BEST-PAIRS* nil)
      (MFL-PAIR-SEARCH l1 l2 nil 0.0)
      (setq bestPairs *MFL-BEST-PAIRS*)
    )
  )
  bestPairs
)

(defun MFL-PAIR-SEARCH (l1 l2 current cost / e1 e2 d)
  (if (null l1)
    (if (or (null *MFL-BEST-COST*) (< cost *MFL-BEST-COST*))
      (setq *MFL-BEST-COST* cost
            *MFL-BEST-PAIRS* (reverse current))
    )
    (progn
      (setq e1 (car l1))
      (foreach e2 l2
        (setq d (MFL-END-DIST e1 e2))
        (if (or (null *MFL-BEST-COST*)
                (<= (+ cost d) *MFL-BEST-COST*))
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

;; ============================================================
;; CHON DAU MUT PHIA CO Y LON NHAT
;; - Dau co Y lon nhat cua LINE 1 ghep voi dau co Y lon nhat cua LINE 2
;; - Neu Y bang nhau (LINE ngang), chon dau co X lon nhat
;; - Khong phu thuoc thu tu diem DXF 10/11
;; ============================================================

(defun MFL-POINT-LESS (p q /)
  (if (equal (cadr p) (cadr q) 1e-8)
    (< (car p) (car q))
    (< (cadr p) (cadr q))
  )
)

(defun MFL-SORT-ENDS-LOW-HIGH (ends)
  (if (MFL-POINT-LESS (car ends) (cadr ends))
    ends
    (reverse ends)
  )
)

(defun MFL-NEAREST-ENDS (e1 e2 highSide / a b)
  ;; highSide = T: lay dau phia Y lon (neu Y bang nhau thi X lon)
  ;; highSide = nil: lay dau phia Y nho (neu Y bang nhau thi X nho)
  ;; Khong phu thuoc thu tu diem 10/11 trong DXF.
  (setq a (MFL-SORT-ENDS-LOW-HIGH (MFL-ENDS e1))
        b (MFL-SORT-ENDS-LOW-HIGH (MFL-ENDS e2)))
  (if highSide
    (list (cadr a) (cadr b))
    (list (car a) (car b))
  )
)

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

;; Trung diem X cua mot nhom LINE, dung de biet nhom 1 nam ben trai hay phai.
(defun MFL-GROUP-X (lst / total n en)
  (setq total 0.0 n 0)
  (foreach en lst
    (setq total (+ total (MFL-X en))
          n (1+ n)))
  (if (> n 0) (/ total n) 0.0)
)


;; ============================================================
;; LENH CHINH SPMFL
;; ============================================================

(defun c:SPMFL (/ *error* ss1 ss2 l1 l2 dir1 dir2 mode pairs pair
                  e1 e2 r rr spacing ybase1 ybase2 result i groupcen oldrad
                  parref temp1 temp2 highSide)

  (defun *error* (msg)
    (if oldrad (setvar "FILLETRAD" oldrad))
    (if (and msg
             (not (wcmatch (strcase msg) "*CANCEL*,*QUIT*,*BREAK*")))
      (princ (strcat "\nSPMFL loi: " msg))
    )
    (princ)
  )

  (setq oldrad (getvar "FILLETRAD"))

  (princ "\nQuet chon nhom thu nhat (LINE / LWPOLYLINE): ")
  (setq ss1 (ssget '((0 . "LINE,LWPOLYLINE"))))

  (cond
    ((null ss1)
     (princ "\nKhong co doi tuong nao duoc chon."))
    (T
     (princ "\nQuet chon nhom thu hai (LINE / LWPOLYLINE): ")
     (setq ss2 (ssget '((0 . "LINE,LWPOLYLINE"))))
     (cond
       ((null ss2)
        (princ "\nKhong co doi tuong nao duoc chon."))
       (T
        (setq l1 (MFL-EXPAND-SS ss1)
              l2 (MFL-EXPAND-SS ss2))
        (cond
          ((or (null l1) (null l2))
           (princ "\nKhong tim thay LINE hop le sau khi xu ly Polyline."))
          ((/= (length l1) (length l2))
           (princ
             (strcat
               "\nHai nhom phai co cung so luong LINE sau khi tach."
               "\nNhom 1: " (itoa (length l1))
               " LINE; nhom 2: " (itoa (length l2)) " LINE.")))
          (T
           (setq dir1 (MFL-DIR (car l1))
                 dir2 (MFL-DIR (car l2))
                 mode nil)
           (cond
             ((MFL-PARALLEL dir1 dir2)
              (setq mode 'parallel)
              (princ "\nNhan dien hai nhom song song."))
             ((MFL-PERP dir1 dir2)
              (setq mode 'perp)
              (princ "\nNhan dien hai nhom vuong goc."))
             (T
              (princ "\nHai nhom khong song song hoac vuong goc.")))

           (if mode
             (progn
               ;; ------------------------------------------------
               ;; NHOM SONG SONG
               ;; ------------------------------------------------
               (if (eq mode 'parallel)
                 (progn
                   ;; Sap xep nhom 1 theo truc chuan; sap xep nhom 2 cung truc
                   ;; nhung DAO NGUOC danh sach truoc khi ghep cap.
                   ;; Neu trung toa do truc ghep: tie-break theo toa do doc LINE,
                   ;; sau cung dung handle de thu tu on dinh.
                   (setq parref (car l1)
                         temp1 (MFL-SORT-PARALLEL l1 parref)
                         ;; Chu y: chu dong DAO NGUOC thu tu cum thu 2
                         ;; de ghep cap doi xung theo yeu cau.
                         temp2 (reverse (MFL-SORT-PARALLEL l2 parref))
                         ;; Huong bo phu thuoc thu tu chon cum:
                         ;; cum 1 ben trai cum 2 -> Y lon; cum 1 ben phai -> Y nho.
                         highSide (< (MFL-GROUP-X l1) (MFL-GROUP-X l2))
                         pairs nil)
                   (while (and temp1 temp2)
                     (setq pairs (cons (list (car temp1) (car temp2)) pairs)
                           temp1 (cdr temp1)
                           temp2 (cdr temp2)))
                   (setq pairs (reverse pairs))

                   (if pairs
                     (progn
                       (setq groupcen
                         (mapcar
                           '(lambda (a b) (/ (+ a b) 2.0))
                           (MFL-MID (caar pairs))
                           (MFL-MID (cadar pairs))))
                       (setq i 0)
                       (foreach pair pairs
                         (setq i (1+ i)
                               e1 (car pair)
                               e2 (cadr pair)
                               result
                               (vl-catch-all-apply
                                 'MFL-ARC-FOR-PAIR
                                 (list e1 e2 highSide)))
                         (if (or (vl-catch-all-error-p result) (null result))
                           (princ (strcat "\nCap " (itoa i) ": khong tao duoc cung."))
                           (princ (strcat "\nCap " (itoa i) ": da tao cung.")))
                       )
                     )
                     (princ "\nKhong tao duoc cap LINE.")
                   )
                 )

                 ;; ------------------------------------------------
                 ;; NHOM VUONG GOC - GIU NGUYEN CHUC NANG CU
                 ;; ------------------------------------------------
                 (progn
                   (setq r (getdist "\nNhap ban kinh R co so <0>: "))
                   (if (null r) (setq r 0.0))
                   (if (>= r 0.0)
                     (progn
                       (setq pairs (MFL-PAIR-OPTIMAL l1 l2))
                       (if pairs
                         (progn
                           (setq ybase1 (MFL-Y (caar pairs))
                                 ybase2 (MFL-Y (cadar pairs))
                                 i 0)
                           (foreach pair pairs
                             (setq i (1+ i)
                                   e1 (car pair)
                                   e2 (cadr pair))
                             ;; Do lech cua ca hai nhom
                             (setq spacing
                               (max
                                 (abs (- (MFL-Y e1) ybase1))
                                 (abs (- (MFL-Y e2) ybase2))))
                             ;; Giam R theo do lech
                             (setq rr (max 0.0 (- r spacing)))
                             (princ
                               (strcat
                                 "\nCap " (itoa i)
                                 " | Lech nhom 1="
                                 (rtos (abs (- (MFL-Y e1) ybase1)) 2 3)
                                 " | Lech nhom 2="
                                 (rtos (abs (- (MFL-Y e2) ybase2)) 2 3)
                                 " | R=" (rtos rr 2 3)))
                             (setq result
                               (vl-catch-all-apply 'setvar (list "FILLETRAD" rr)))
                             (if (not (vl-catch-all-error-p result))
                               (progn
                                 (setq result
                                   (vl-catch-all-apply
                                     'vl-cmdf
                                     (list "_.FILLET" e1 e2)))
                                 (if (vl-catch-all-error-p result)
                                   (princ "\nFILLET cap nay that bai.")
                                   (princ "\nDa xu ly cap.")))
                               (princ "\nKhong dat duoc ban kinh FILLET."))
                           )
                         )
                         (princ "\nKhong tim duoc cap LINE phu hop.")
                       )
                     )
                     (princ "\nBan kinh khong hop le.")
                   )
                 )
               )
               (princ "\nSPMFL hoan tat.")
             )
           )
          )
        )
       )
     )
    )
  )

  ;; Khoi phuc ban kinh FILLET ban dau
  (if oldrad (setvar "FILLETRAD" oldrad))
  (princ)
)

(princ "\nSPMFL da duoc nap. Go SPMFL de su dung.")
(princ)
