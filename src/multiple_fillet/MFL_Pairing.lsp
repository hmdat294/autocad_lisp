
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