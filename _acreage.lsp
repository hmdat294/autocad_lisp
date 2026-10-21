(defun c:APL (/ area pt h) 
  (setq h 100.0)

  (command "_.AREA")

  (while (> (getvar "CMDACTIVE") 0) (command pause))

  (setq area (getvar "AREA"))

  (setq pt (getpoint "\nChon vi tri hien dien tich: "))

  (if pt 
    (entmakex 
      (list 
        '(0 . "TEXT")
        (cons 10 pt)
        (cons 40 h)

        (cons 1 
              (strcat 
                "S = "
                (rtos (/ area 1000000) 2 2)
                " m2"
              )
        )

        (cons 7 (getvar "TEXTSTYLE"))
      )
    )
  )

  (prompt (strcat "\nS = " (rtos (/ area 1000000) 2 2) " m2"))
  (princ)
)


(defun c:ACR (/ cen rad area inspt h txt) 

  (setq h 100.0)

  (setq cen (getpoint "\nChon tam hinh tron: "))

  (setq rad (getdist cen "\nNhap ban kinh: "))

  (setq area (* pi rad rad))

  (setq inspt (getpoint "\nChon vi tri chen text: "))

  (setq txt (strcat 
              "S = PI x "
              (rtos rad 2 2)
              "^2 = "
              (rtos (/ area 1000000) 2 2)
            )
  )

  (entmakex 
    (list 
      '(0 . "TEXT")
      (cons 10 inspt)
      (cons 40 h)
      (cons 1 txt)
      (cons 7 (getvar "TEXTSTYLE"))
    )
  )

  (prompt (strcat "\nS = " (rtos (/ area 1000000) 2 2) " m2"))
  (princ)
)