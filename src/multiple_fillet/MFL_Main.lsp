
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