
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


;; Trung diem X cua mot nhom LINE, dung de biet nhom 1 nam ben trai hay phai.
(defun MFL-GROUP-X (lst / total n en)
  (setq total 0.0 n 0)
  (foreach en lst
    (setq total (+ total (MFL-X en))
          n (1+ n)))
  (if (> n 0) (/ total n) 0.0)
)

