
(defun c:SDE (/ ss i ent obj val total extra inspt txt str h)

  ;; chiều cao text
  (setq h 100.0)

  ;; tổng ban đầu
  (setq total 0)

  ;; chuỗi phép tính
  (setq str "")

  ;; chọn dim
  (prompt "\nChon cac DIM can cong: ")

  (if (setq ss (ssget '((0 . "DIMENSION"))))

    (progn

      (setq i 0)

      (while (< i (sslength ss))

        (setq ent (ssname ss i))
        (setq obj (vlax-ename->vla-object ent))

        ;; lấy giá trị dim
        (setq val (fix (vla-get-Measurement obj)))

        ;; cộng tổng
        (setq total (+ total val))

        ;; tạo chuỗi phép tính
        (if (= i 0)
          (setq str (itoa val))
          (setq str (strcat str " + " (itoa val)))
        )

        (setq i (1+ i))
      )

      ;; nhập thêm
      (setq extra (getint "\nNhap gia tri cong them <0>: "))

      (if (null extra)
        (setq extra 0)
      )

      ;; thêm vào chuỗi
      (if (> extra 0)
        (setq str (strcat str " + " (itoa extra)))
      )

      ;; cộng thêm
      (setq total (+ total extra))

      ;; hoàn thiện biểu thức
      (setq str
        (strcat
          str
          " = "
          (itoa total)
        )
      )

      ;; vị trí chèn
      (setq inspt (getpoint "\nChon vi tri chen text: "))

      ;; tạo text
      (entmakex
        (list
          '(0 . "TEXT")
          (cons 10 inspt)
          (cons 40 h)
          (cons 1 str)
          (cons 7 (getvar "TEXTSTYLE"))
          '(72 . 0)
          '(73 . 0)
        )
      )

      (prompt (strcat "\nKet qua: " str))
    )

    (prompt "\nKhong chon DIM nao.")
  )

  (princ)
)
