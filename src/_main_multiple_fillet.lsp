
(vl-load-com)

;; Luu duong dan cua file MFL.lsp dang duoc nap
(setq *MFL-DIR*
  (if (and
        (boundp '*load-truename*)
        *load-truename*)
    (vl-filename-directory *load-truename*)
    nil
  )
)

(defun MFL-LOAD-MODULE (filename / fullpath)
  (if *MFL-DIR*
    (progn
      (setq fullpath
        (strcat *MFL-DIR* "\\" filename)
      )

      (if (findfile fullpath)
        (progn
          (load fullpath)
          (princ (strcat "\nDa nap: " filename))
        )
        (prompt
          (strcat "\nKhong tim thay file: " fullpath)
        )
      )
    )
    (prompt "\nKhong xac dinh duoc thu muc MFL.")
  )
)

;; Nap theo thu tu phu thuoc
(MFL-LOAD-MODULE "MFL_Geometry.lsp")
(MFL-LOAD-MODULE "MFL_Pairing.lsp")
(MFL-LOAD-MODULE "MFL_Arc.lsp")
(MFL-LOAD-MODULE "MFL_Main.lsp")

(princ "\nMFL da nap xong.")
(princ)
